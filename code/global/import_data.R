#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# This script imports time series inputs (ts).
# The path to the time series is defined by /config/paths.R
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import river flow and reservoir inflow ts:
#   - temporarily import flows_daily_mgd.csv used in 2018drex
#   - Luke - next step is to import USGS flow data from Zach's app
#     and then put it into the format used in flows_daily_mgd.csv
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Make adjustments to Occoquan inflows, and river flows
#  - Occoquan inflows are increased by UOSA discharge,
#     and decreased by Lake Manassas operations
#  - River flow at POR and LFalls are decreased by upstr CU
#     and increased by WWTP discharges, ie dflow = wwtp - cu
#     (see parameters.R for values)
flows.daily.mgd.df0 <- data.table::fread(paste(ts_path, "flows_daily_mgd.csv", sep = ""),
                                      data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date),
                month_sim = month(date_time),
                occ_in0 = occ) %>%
  left_join(uosa.out.df, by = "month_sim", copy = FALSE)
flows.daily.mgd.df <- flows.daily.mgd.df0 %>%
  mutate(occ_in = occ_in0 + uosa_discharge -
           lake_manassas_reduction) %>%
  left_join(dflow.upstr.df, by = "month_sim", copy = FALSE) %>%
  mutate(lfalls_nat = lfalls_nat + dflow,
         por_nat = por_nat + dflow) %>%
  dplyr::select(sim_day, date_time, month_sim,
                jrr_in = jrr,
                lsen_in = lsen,
                pat_in = pat,
                occ_in0,
                occ_in,
                uosa_discharge,
                dflow,
                por_nat, below_por, lfalls_nat) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import a time series of total system demands.
#   - temporarily just use demands from 2018drex
#   - Luke - next step is to import time series of demands from Sarah's DataPortal website
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#
# (Later we could create a stochastic demand model like in PRRISM.)
demands.daily.df <- data.table::fread(paste(ts_path, "demands_daily.csv", sep = ""),
                                       data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date)) %>%
  dplyr::select(date_time, d_fw_e, d_fw_w, d_fw_c, d_lw,
                d_wa, d_wssc, d_total) %>%
  dplyr:: filter(date_time <= date_end,
               date_time >= date_start)
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import time series representing state drought status.
#   - temporarily just use time series from 2018drex.
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# 
state.drought.df <- data.table::fread(paste(ts_path, "state_drought_status.csv", sep = ""),
                                      data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr::select(date_time, 
                gw_va_shen, p_va_shen, sw_va_shen, r_va_shen,
                gw_va_nova, p_va_nova, sw_va_nova, r_va_nova,
                gw_md_cent, p_md_cent, sw_md_cent, r_md_cent,
                gw_md_west, p_md_west, sw_md_west, r_md_west,
                region_md_cent, region_md_west
                ) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import reservoir dfs - this is a temporary fix to get us going in 2019
#   - just importing tables outputted by 2018drex to serve as temporary reservoir data frames
#   - these will be used to initialize the res dfs from date_start to date_today0 (ie today())
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
print("date_today0")
print(date_today0)
sen.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_sen.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
jrr.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_jrr.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
occ.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_occ.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
pat.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_pat.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
                                        