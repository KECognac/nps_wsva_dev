#' Calculate Water System Importance
#'
#' Assigns each water supply an importance category (High, Moderate, Low, or
#' Not rated) with a reason. This isn't a scored indicator, so it isn't
#' normalized; it's used to filter or add context to the rapid results.
#'
#' Each system is rated on two things, then combined with park visitation:
#' - Scale: population served from EPA SDWA (EPA size breaks at 500 and 3,300).
#'   Municipal supplies get no population, because SDWA reports the whole utility.
#' - Park role: how the system compares with the park's other visitor-facing,
#'   park-run systems (Primary = largest, Secondary = 30%+ share, Minor, Staff only).
#'
#' Park roles and municipal counts need every active source at the park, including
#' municipal supplies that aren't on the rapid list, so they're calculated from
#' `all_water_supplies` before the output is limited to `water_supplies`.
#' Sources that are inactive in the WSD or SDWA are removed before anything is
#' calculated.
#'
#' See Explore/explore_system_importance.Rmd for the logic, results, and the
#' items that need data clarification.
#'
#' @param water_supplies sf or data frame of the sources to return, usually
#'   `get_wsd(rapid = TRUE)`. Only `wsd_source_id` is used.
#' @param all_water_supplies all CONUS sources, including municipal, purchased,
#'   and hauled. Defaults to `get_wsd(conus = TRUE, fill_empty_coords = TRUE)`.
#' @param sdwa data frame of `SDWA_PUB_WATER_SYSTEMS.csv`. Defaults to
#'   `get_sdwa(file = "SDWA_PUB_WATER_SYSTEMS.csv")`.
#' @param visitation_file path to the NPS visitor use statistics CSV
#'   (`Main_Data.csv`), with columns UnitCode, Year, Statistic, and Value.
#' @param visitation_years years averaged for recreation visits (`TRV`).
#' @param tier_low_max,tier_moderate_max population breaks between Low, Moderate,
#'   and High scale.
#' @param assumed_pop_non_pws population given to non-public systems with no SDWA match.
#' @param assumed_pop_pws population given to PWS with no usable SDWA population.
#' @param secondary_share_min share of the park's visitor-facing population needed
#'   to be a Secondary system.
#' @param park_vis_high,park_vis_moderate annual recreation visits for a busy and a
#'   moderately busy park.
#' @param return_details logical. If TRUE, returns a list with the results, all
#'   active sources, sources removed as inactive in SDWA, and the settings used.
#' @param save logical, whether to save output
#' @param output_dir character, directory path for saving files
#' @param verbose logical, whether to show progress messages
#'
#' @return tibble with one row per source in `water_supplies` that is active,
#'   with `importance_category`, `importance_reason`, `high_importance`, the
#'   supporting scale, role, and visitation columns, and `review_*` flags.
#'   A list if `return_details = TRUE`.
#'
calc_system_importance <- function(water_supplies,
                                   all_water_supplies = NULL,
                                   sdwa = NULL,
                                   visitation_file = "data/all/2024_Visitor_Use_Statistics/Main_Data.csv",
                                   visitation_years = 2020:2024,
                                   tier_low_max = 500,
                                   tier_moderate_max = 3300,
                                   assumed_pop_non_pws = 24,
                                   assumed_pop_pws = 25,
                                   secondary_share_min = 0.3,
                                   park_vis_high = 1000000,
                                   park_vis_moderate = 100000,
                                   return_details = FALSE,
                                   save = FALSE,
                                   output_dir = NULL,
                                   verbose = TRUE) {

  # Input validation
  stopifnot(
    "water_supplies must contain wsd_source_id" = "wsd_source_id" %in% names(water_supplies),
    "visitation_file not found" = file.exists(visitation_file),
    "save is TRUE but output_dir not provided" = !save || !is.null(output_dir)
  )

  start_time <- Sys.time()

  pws_categories <- c("Low", "Moderate", "High")
  park_run_categories <- c("Non-PWS", pws_categories)
  municipal_pattern <- regex("municipal|hauled|tribal|purchased", ignore_case = TRUE)

  # Keyword groups for the free-text point_of_use field. The first match wins, so a
  # source serving both visitors and staff counts as visitor-facing. Only
  # "Staff/admin" changes the result: those systems are left out of the role comparison.
  pou_patterns <- c(
    "Lodging"         = "lodg|hotel|inn\\b|cabin|motel|resort|village",
    "Campground"      = "camp|\\brv\\b",
    "Visitor day use" = "visitor|\\bvc\\b|picnic|restroom|comfort|beach|bath|restaurant|store|marina|museum|day use|trailhead|fountain|concession|shower|snack|guest|rafter",
    "Parkwide"        = "all park|park facilities|entire park|headquarters|\\bhq\\b",
    "Staff/admin"     = "resid|hous|employee|staff|quarters|dorm|maint|office|ranger station|shop"
  )

  classify_point_of_use <- function(x) {
    type <- rep(NA_character_, length(x))
    for (name in names(pou_patterns)) {
      match <- str_detect(coalesce(x, ""), regex(pou_patterns[[name]], ignore_case = TRUE))
      type <- if_else(is.na(type) & match, name, type)
    }
    case_when(!is.na(type) ~ type,
              is.na(x) | trimws(x) == "" ~ "Missing",
              TRUE ~ "Other")
  }

  # WSD codes that differ from the visitation codes (same as calc_visitation_sensitivity)
  visitation_code_crosswalk <- c("ANAC" = "NCPE", "NACE" = "NCPE", "NAMA" = "NCPC", "VALR" = "PERL")

  # 1. Import all sources and remove inactive ones -----------------------------
  if (verbose) message("Reading water supplies and SDWA data...")

  if (is.null(all_water_supplies)) {
    all_water_supplies <- get_wsd(conus = TRUE, fill_empty_coords = TRUE)
  }
  if (is.null(sdwa)) {
    sdwa <- get_sdwa(file = "SDWA_PUB_WATER_SYSTEMS.csv")
  }

  sources <- all_water_supplies |>
    sf::st_drop_geometry() |>
    distinct(wsd_source_id, .keep_all = TRUE) |>
    # Inactive and temporarily inactive sources are never considered. Missing counts as active.
    dplyr::filter(coalesce(active, "active") == "active") |>
    select(park_unit, park_name, water_system_name, wsd_system_id, wsd_source_id, classification,
           system_type, source_type, use_category, active, pws_number, point_of_use)

  sdwa_clean <- sdwa |>
    mutate(
      pws_number_clean = toupper(trimws(PWSID)),
      pws_type_desc = case_when(
        PWS_TYPE_CODE == "CWS"    ~ "Community Water System",
        PWS_TYPE_CODE == "NTNCWS" ~ "Non-Transient Non-Community",
        PWS_TYPE_CODE == "TNCWS"  ~ "Transient Non-Community",
        TRUE ~ "Unknown/Other"
      ),
      owner_type_desc = case_when(
        OWNER_TYPE_CODE == "F" ~ "Federal Government",
        OWNER_TYPE_CODE == "L" ~ "Local Government",
        OWNER_TYPE_CODE == "S" ~ "State Government",
        OWNER_TYPE_CODE == "P" ~ "Private",
        OWNER_TYPE_CODE == "M" ~ "Mixed Ownership",
        OWNER_TYPE_CODE == "N" ~ "Native American",
        TRUE ~ "Other"
      )
    ) |>
    select(pws_number_clean,
           pws_name            = PWS_NAME,
           pws_activity_status = PWS_ACTIVITY_CODE,
           pws_type_desc,
           owner_type_desc,
           population_served   = POPULATION_SERVED_COUNT)

  # One SDWA record per source. A few sources list two PWS numbers, so prefer an
  # active record, then the largest population served.
  sdwa_by_source <- sources |>
    select(wsd_source_id, pws_number) |>
    separate_rows(pws_number, sep = ",\\s*") |>
    mutate(pws_number_clean = toupper(trimws(pws_number))) |>
    inner_join(sdwa_clean, by = "pws_number_clean") |>
    arrange(wsd_source_id, desc(pws_activity_status == "A"), desc(population_served)) |>
    distinct(wsd_source_id, .keep_all = TRUE) |>
    select(-pws_number)

  # Sources whose PWS isn't active in SDWA are also removed. They're active in the
  # WSD, so they're kept for review.
  sdwa_inactive <- sources |>
    inner_join(sdwa_by_source, by = "wsd_source_id") |>
    dplyr::filter(pws_activity_status != "A")

  sources <- sources |>
    dplyr::filter(!wsd_source_id %in% sdwa_inactive$wsd_source_id)

  # Mean annual recreation visits. Total visits (TV) also count non-recreation
  # traffic such as commuters, which inflates parkways.
  park_visits <- readr::read_csv(visitation_file, show_col_types = FALSE) |>
    dplyr::filter(Statistic == "TRV", Year %in% visitation_years) |>
    group_by(UnitCode, Year) |>
    summarize(annual_visits = sum(Value, na.rm = TRUE), .groups = "drop") |>
    group_by(UnitCode) |>
    summarize(park_rec_visits = mean(annual_visits))

  # 2-3. Scale category and point of use ---------------------------------------
  if (verbose) message("Assigning scale and park role to ", nrow(sources), " active sources...")

  # The first matching condition wins.
  system_categories <- sources |>
    left_join(sdwa_by_source, by = "wsd_source_id") |>
    mutate(
      has_sdwa_pop = !is.na(population_served) & population_served > 0,
      # Non-federal utilities report the population of their whole service area, not
      # the park's portion. Federally owned systems buying water report their own.
      is_municipal = str_detect(coalesce(source_type, ""), municipal_pattern) &
        coalesce(owner_type_desc, "") != "Federal Government",
      population_estimate = case_when(
        classification %in% c("Non-Potable (NonPOT)", "Proposed System") | is_municipal ~ NA_real_,
        has_sdwa_pop ~ population_served,
        classification == "Non-Public Water System (NP)" ~ assumed_pop_non_pws,
        TRUE ~ assumed_pop_pws # most likely a missing or incorrect PWS number
      ),
      population_source = case_when(
        is.na(population_estimate) ~ NA_character_,
        has_sdwa_pop ~ "SDWA",
        TRUE ~ "Assumed"
      ),
      system_category = case_when(
        classification == "Proposed System" ~ "Proposed",
        classification == "Non-Potable (NonPOT)" ~ "Non-potable",
        is_municipal ~ "Municipal/purchased",
        !has_sdwa_pop & classification == "Non-Public Water System (NP)" ~ "Non-PWS",
        population_estimate <= tier_low_max ~ "Low",
        population_estimate <= tier_moderate_max ~ "Moderate",
        TRUE ~ "High"
      ),
      pou_type = classify_point_of_use(point_of_use),
      # Each PWS counts once at a park, however many sources it has. Systems without
      # a matched population fall back to the WSD system ID.
      unit_id = if_else(has_sdwa_pop, pws_number_clean, wsd_system_id),
      visitation_code = coalesce(visitation_code_crosswalk[park_unit], park_unit),
      # Classification defines non-potable; flag systems where use_category disagrees
      review_potable_conflict = !is.na(use_category) & (
        (system_category == "Non-potable" & str_detect(use_category, "human-drinking")) |
          (system_category %in% park_run_categories & !str_detect(use_category, "human-drinking"))
      ),
      # SDWA population may not describe the park: federal systems buying water can
      # serve others, and non-federal CWS listed as park-run may be utilities
      review_population = system_category %in% pws_categories & (
        str_detect(coalesce(source_type, ""), municipal_pattern) |
          (coalesce(pws_type_desc, "") == "Community Water System" &
             coalesce(owner_type_desc, "") != "Federal Government")
      )
    )

  # 4. Park role ----------------------------------------------------------------
  # Each park's own potable systems are compared with each other. Staff-only systems
  # are left out, so the role reflects the systems visitors rely on.
  park_roles <- system_categories |>
    dplyr::filter(system_category %in% park_run_categories) |>
    group_by(park_unit, unit_id) |>
    summarize(population_estimate = first(population_estimate),
              visitor_facing = any(pou_type != "Staff/admin"),
              .groups = "drop") |>
    group_by(park_unit) |>
    mutate(n_park_run = n()) |>
    group_by(park_unit, visitor_facing) |>
    mutate(share_visitor_facing = if_else(visitor_facing, population_estimate / sum(population_estimate), NA_real_),
           is_largest = visitor_facing & population_estimate == max(population_estimate),
           n_tied_primary = sum(is_largest)) |>
    ungroup() |>
    mutate(park_role = case_when(
      !visitor_facing ~ "Staff only",
      is_largest ~ "Primary",
      share_visitor_facing >= secondary_share_min ~ "Secondary",
      TRUE ~ "Minor"
    )) |>
    select(park_unit, unit_id, n_park_run, share_visitor_facing, n_tied_primary, park_role)

  # Active municipal connections per park. This counts connections, not volume supplied.
  park_municipal <- system_categories |>
    dplyr::filter(system_category == "Municipal/purchased") |>
    distinct(park_unit, wsd_system_id) |>
    count(park_unit, name = "n_municipal")

  # 5-6. Park context and importance category -----------------------------------
  all_results <- system_categories |>
    left_join(park_roles, by = c("park_unit", "unit_id")) |>
    left_join(park_municipal, by = "park_unit") |>
    left_join(park_visits, by = c("visitation_code" = "UnitCode")) |>
    group_by(park_unit) |>
    mutate(n_park_run = if (all(is.na(n_park_run))) 0L else max(n_park_run, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      n_municipal = replace_na(n_municipal, 0L),
      park_role = factor(coalesce(park_role, "Not park-run"),
                         levels = c("Primary", "Secondary", "Minor", "Staff only", "Not park-run")),
      park_visitation_class = case_when(
        is.na(park_rec_visits) ~ "No data",
        park_rec_visits >= park_vis_high ~ "High",
        park_rec_visits >= park_vis_moderate ~ "Moderate",
        TRUE ~ "Low"
      ),
      # Only PWS can qualify through the park role: non-PWS systems serve fewer than 25
      # people per day by definition. Parks with as many municipal connections as
      # park-run systems are left out, because their own systems may not be their main supply.
      role_counts = system_category %in% pws_categories & n_municipal < n_park_run,
      # The first matching condition wins.
      importance_reason = case_when(
        system_category == "Non-potable" ~ "Non-potable",
        system_category == "Municipal/purchased" ~ "Municipal or purchased",
        system_category == "Proposed" ~ "Proposed",
        system_category == "High" ~ "High scale",
        role_counts & park_role == "Primary" & park_visitation_class == "High" ~ "Primary system at a busy park",
        system_category == "Moderate" ~ "Moderate scale",
        role_counts & park_role == "Primary" & park_visitation_class == "Moderate" ~ "Primary system at a moderately busy park",
        role_counts & park_role == "Secondary" & park_visitation_class == "High" ~ "Secondary system at a busy park",
        park_role == "Staff only" ~ "Staff-only system",
        system_category == "Non-PWS" ~ "Non-public system",
        TRUE ~ "Small system"
      ),
      importance_category = case_when(
        importance_reason %in% c("Non-potable", "Municipal or purchased", "Proposed") ~ "Not rated",
        importance_reason %in% c("High scale", "Primary system at a busy park") ~ "High",
        importance_reason %in% c("Moderate scale", "Primary system at a moderately busy park",
                                 "Secondary system at a busy park") ~ "Moderate",
        TRUE ~ "Low"
      ) |>
        factor(levels = c("High", "Moderate", "Low", "Not rated")),
      high_importance = importance_category == "High",
      # Review flags
      review_staff_only_pws = park_role == "Staff only" & system_category %in% pws_categories,
      review_tied_primary = park_role == "Primary" & coalesce(n_tied_primary, 1L) > 1,
      review_assumed_pws = population_source == "Assumed" & system_category %in% pws_categories,
      review_population_vs_visits = population_source == "SDWA" & !is.na(park_rec_visits) &
        population_estimate >= park_rec_visits / 365,
      across(starts_with("review_"), \(x) coalesce(x, FALSE)),
      system_category = factor(system_category, levels = c("High", "Moderate", "Low", "Non-PWS",
                                                           "Municipal/purchased", "Non-potable", "Proposed"))
    ) |>
    select(wsd_source_id, park_unit, park_name, water_system_name, wsd_system_id,
           importance_category, importance_reason, high_importance,
           system_category, population_estimate, population_source, park_role,
           share_visitor_facing, n_tied_primary, n_park_run, n_municipal,
           park_rec_visits, park_visitation_class,
           classification, source_type, use_category, point_of_use, pou_type, unit_id,
           pws_number_clean, pws_name, pws_type_desc, owner_type_desc, pws_activity_status,
           population_served, starts_with("review_"))

  # 7. Limit to the requested sources -------------------------------------------
  # Roles and municipal counts above used all active sources, so they still
  # reflect each park's full set of systems.
  final_results <- all_results |>
    dplyr::filter(wsd_source_id %in% unique(water_supplies$wsd_source_id))

  # Save results
  if (save) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    filename <- file.path(output_dir, paste0("system_importance_", Sys.Date(), ".csv"))
    write_csv(final_results, filename)

    if (verbose) {
      message("Results saved to: ", filename)
    }
  }

  # Print timing
  if (verbose) {
    n_removed <- dplyr::n_distinct(water_supplies$wsd_source_id) - nrow(final_results)
    message(nrow(final_results), " sources returned (", n_removed, " removed as inactive); ",
            sum(final_results$high_importance), " High")
    end_time <- Sys.time()
    total_time <- end_time - start_time
    message("\nSystem importance processing completed in ",
            round(total_time, 2), " ", attr(total_time, "units"))
  }

  if (return_details) {
    return(list(
      results = final_results,
      all_sources = all_results,
      sdwa_inactive = sdwa_inactive,
      settings = list(
        tier_low_max = tier_low_max, tier_moderate_max = tier_moderate_max,
        assumed_pop_non_pws = assumed_pop_non_pws, assumed_pop_pws = assumed_pop_pws,
        secondary_share_min = secondary_share_min, park_vis_high = park_vis_high,
        park_vis_moderate = park_vis_moderate, visitation_years = visitation_years,
        pou_patterns = pou_patterns
      )
    ))
  }

  return(final_results)
}
