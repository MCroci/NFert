#' Fertilization distribution plan
#'
#' Builds the fertilisation distribution plan for a crop according to DPI
#' Emilia-Romagna 2026 (Fert_Office v1.26 foglio `Distribuz`). Combines:
#'
#' - Organic applications (one or more matrices from `fert_org.table`, with
#'   efficiency selected from `efficienza.table` by soil group, dose level,
#'   and sector of matrix).
#' - Mineral / synthesis fertilisers (from `concimi.table` or user-specified
#'   triplets of N, P2O5, K2O titres).
#'
#' For each input row the function computes the "useful" contribution to the
#' crop for N (via efficiency), P2O5 and K2O (assumed 100%), and aggregates
#' the totals. If `n_balance`, `p_balance`, `k_balance` are provided the
#' function also checks the totals against the required doses and returns
#' alerts (`OK` / `Eccesso` / `Deficit`).
#'
#' @param soil_group Italian DPI texture group.
#' @param n_balance,p_balance,k_balance Optional numeric requirements (kg/ha).
#' @param organic_rows A list of lists, each with:
#'   - `fertilizer`: name matching `fert_org.table$fertilizer` (or a custom
#'     name plus explicit titres)
#'   - `quantity_t_ha`: amount in t/ha (fresh matter)
#'   - `year`: application year (e.g. 2024, 2025)
#'   - `modality_epoch`: ID or label from `mod_distribuz.table`
#'   - `level` (optional): efficiency level `"bassa"/"media"/"alta"` if you
#'     want to override the default selection.
#'   - Optional `N_pct`, `P2O5_pct`, `K2O_pct`, `ss_pct` override the table.
#' @param mineral_rows A list of lists, each with:
#'   - `concime`: matching `concimi.table$concime`, or custom
#'   - `quantity_q_ha`: quintals/ha (100 kg/ha)
#'   - `modality_epoch`: ID or label from `mod_distribuz.table`
#'   - `N`, `P2O5`, `K2O` titres (kg per q) if custom.
#' @param zvn Logical, if TRUE, enforce the ZVN 170 kg N/ha zootec cap.
#' @param fert_org.table,concimi.table,efficienza.table,mod_distribuz.table,ragg_tes.table Defaults.
#'
#' @return A list with:
#'   - `rows`: per-application data frame (fertilizer, quantity, delivered N/P/K, efficiency, useful N/P/K)
#'   - `totals`: totals of useful N, P2O5, K2O delivered
#'   - `alerts`: if balances provided, OK/Eccesso/Deficit for each nutrient
#'   - `zvn_zoot_N`: total zootec N delivered (useful for ZVN check)
#' @export
plan_distribution <- function(soil_group,
                              n_balance = NULL, p_balance = NULL, k_balance = NULL,
                              organic_rows = list(),
                              mineral_rows = list(),
                              zvn = FALSE,
                              fert_org.table      = NFert::fert_org.table,
                              concimi.table       = NFert::concimi.table,
                              efficienza.table    = NFert::efficienza.table,
                              mod_distribuz.table = NFert::mod_distribuz.table,
                              ragg_tes.table      = NFert::ragg_tes.table) {

  ID_Rag <- normalise_soil_group(soil_group)$id_rag

  level_map <- c("bassa" = 1, "media" = 2, "alta" = 3)

  lookup_mod <- function(mod) {
    if (is.numeric(mod)) return(as.integer(mod))
    idx <- match(mod, mod_distribuz.table$modality_epoch)
    if (is.na(idx)) return(NA_integer_)
    as.integer(mod_distribuz.table$ID_Mo[idx])
  }

  dose_level_from_kg <- function(kg_N) {
    # DPI: < 125 low, 125-249 medium, >= 250 high
    if (is.na(kg_N)) return("media")
    if (kg_N < 125) return("bassa")
    if (kg_N < 250) return("media")
    "alta"
  }

  rows <- list()

  # -------- Organic --------
  for (r in organic_rows) {
    fert <- r$fertilizer
    q    <- if (!is.null(r$quantity_t_ha)) r$quantity_t_ha else 0
    row_fo <- fert_org.table[fert_org.table$fertilizer == fert, , drop = FALSE]
    if (nrow(row_fo) == 0 && (is.null(r$N_pct) || is.null(r$P2O5_pct) || is.null(r$K2O_pct))) {
      warning(sprintf("Organic fertilizer '%s' not in fert_org.table and no titres given; skipping.", fert))
      next
    }
    N_pct   <- if (!is.null(r$N_pct))    r$N_pct    else as.numeric(row_fo$avg_N[1])
    P_pct   <- if (!is.null(r$P2O5_pct)) r$P2O5_pct else as.numeric(row_fo$avg_P2O5[1])
    K_pct   <- if (!is.null(r$K2O_pct))  r$K2O_pct  else as.numeric(row_fo$avg_K2O[1])
    ss_pct  <- if (!is.null(r$ss_pct))   r$ss_pct   else as.numeric(row_fo$avg_ss[1])
    sector  <- if (nrow(row_fo) > 0) as.character(row_fo$tipo[1]) else NA
    zootec  <- if (nrow(row_fo) > 0) identical(as.character(row_fo$zootec_100pct[1]), "sì") else FALSE

    # Kg delivered (fresh): titres are kg/t of fresh matter
    N_kg  <- q * N_pct
    P_kg  <- q * P_pct
    K_kg  <- q * K_pct

    # Dose level for efficiency (based on N kg)
    level <- if (!is.null(r$level)) r$level else dose_level_from_kg(N_kg)
    ID_Liv <- level_map[[level]]

    # Efficiency lookup: (ID_Rag, ID_Liv, sector, N_org_id)
    eff <- NA_real_
    if (!is.na(sector)) {
      e <- efficienza.table[
        efficienza.table$ID_Rag == ID_Rag &
          efficienza.table$ID_Liv == ID_Liv &
          efficienza.table$ID_Sett == sector, , drop = FALSE]
      if (nrow(e) > 0) eff <- as.numeric(e$efficiency_pct[1])
    }
    # Fallback: medium-medium-60%
    if (is.na(eff)) eff <- 50

    rows[[length(rows) + 1]] <- data.frame(
      source = "organic",
      fertilizer = fert,
      year = if (!is.null(r$year)) r$year else NA_integer_,
      modality_epoch = if (!is.null(r$modality_epoch)) r$modality_epoch else NA_character_,
      ID_Mo = lookup_mod(r$modality_epoch %||% NA_integer_),
      quantity_t_ha = q,
      N_kg = N_kg, P2O5_kg = P_kg, K2O_kg = K_kg,
      efficiency_pct = eff,
      N_useful   = N_kg * eff / 100,
      P2O5_useful = P_kg,
      K2O_useful = K_kg,
      zootec = zootec,
      stringsAsFactors = FALSE
    )
  }

  # -------- Mineral --------
  for (r in mineral_rows) {
    conc <- r$concime
    q    <- if (!is.null(r$quantity_q_ha)) r$quantity_q_ha else 0
    row_c <- concimi.table[concimi.table$concime == conc, , drop = FALSE]
    if (nrow(row_c) == 0 && (is.null(r$N) || is.null(r$P2O5) || is.null(r$K2O))) {
      warning(sprintf("Mineral '%s' not found and no titres given; skipping.", conc))
      next
    }
    N_t <- if (!is.null(r$N))    r$N    else as.numeric(row_c$N[1])
    P_t <- if (!is.null(r$P2O5)) r$P2O5 else as.numeric(row_c$P2O5[1])
    K_t <- if (!is.null(r$K2O))  r$K2O  else as.numeric(row_c$K2O[1])

    # Titres are kg/q = %: delivered kg/ha = q/ha * %
    N_kg <- q * N_t
    P_kg <- q * P_t
    K_kg <- q * K_t

    rows[[length(rows) + 1]] <- data.frame(
      source = "mineral",
      fertilizer = conc,
      year = if (!is.null(r$year)) r$year else NA_integer_,
      modality_epoch = if (!is.null(r$modality_epoch)) r$modality_epoch else NA_character_,
      ID_Mo = lookup_mod(r$modality_epoch %||% NA_integer_),
      quantity_t_ha = q / 10,   # q/ha to t/ha for reporting consistency
      N_kg = N_kg, P2O5_kg = P_kg, K2O_kg = K_kg,
      efficiency_pct = 100,
      N_useful = N_kg, P2O5_useful = P_kg, K2O_useful = K_kg,
      zootec = FALSE,
      stringsAsFactors = FALSE
    )
  }

  rows_df <- if (length(rows) > 0) do.call(rbind, rows) else
    data.frame(source = character(), fertilizer = character(),
               year = integer(), modality_epoch = character(), ID_Mo = integer(),
               quantity_t_ha = numeric(), N_kg = numeric(),
               P2O5_kg = numeric(), K2O_kg = numeric(),
               efficiency_pct = numeric(),
               N_useful = numeric(), P2O5_useful = numeric(), K2O_useful = numeric(),
               zootec = logical(), stringsAsFactors = FALSE)

  tot_N <- sum(rows_df$N_useful, na.rm = TRUE)
  tot_P <- sum(rows_df$P2O5_useful, na.rm = TRUE)
  tot_K <- sum(rows_df$K2O_useful, na.rm = TRUE)
  tot_zoot_N <- sum(rows_df$N_kg[rows_df$zootec], na.rm = TRUE)

  alert <- function(total, target) {
    if (is.null(target) || is.na(target)) return(NA_character_)
    if (total >  target * 1.05) return("Eccesso")
    if (total <  target * 0.95) return("Deficit")
    "OK"
  }

  alerts <- list(
    N = alert(tot_N, n_balance),
    P2O5 = alert(tot_P, p_balance),
    K2O  = alert(tot_K, k_balance)
  )

  zvn_warn <- NA
  if (isTRUE(zvn)) {
    zvn_warn <- if (tot_zoot_N > 170) "Eccesso ZVN (>170 kg N/ha/anno da effluenti)" else "OK ZVN"
  }

  list(
    rows = rows_df,
    totals = c(N_useful = tot_N, P2O5_useful = tot_P, K2O_useful = tot_K),
    targets = c(N = n_balance %||% NA_real_, P2O5 = p_balance %||% NA_real_, K2O = k_balance %||% NA_real_),
    alerts  = alerts,
    zvn_zoot_N = tot_zoot_N,
    zvn_alert  = zvn_warn
  )
}

# null-coalesce helper
`%||%` <- function(a, b) if (is.null(a)) b else a
