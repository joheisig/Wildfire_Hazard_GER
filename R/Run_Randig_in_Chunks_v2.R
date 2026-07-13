################################################################################
#'
#'                     RANDIG FIRE SPREAD SIMULATION
#' 
#' Version 2:
#' - FBFM: 
#' - ignitions: random ignitions for D3L3 / p_80 / wd_0 
#' -----> reuse locations for all other runs
#' - duration: 20 hrs
#'
#'##############################################################################

library(tictoc)
library(glue)
library(dplyr)

# Paths
randig_bat = "C:/Users/jheisig/Desktop/FB_x64/bin/TestRandig"
nfdrs_exe = "C:/Users/jheisig/Desktop/FB_x64/NFDRS4_cli/bin/NFDRS4_cli"
home = "D:/Jo/FireHazard"
#setwd(home)

source("R/write_randig_inputs.R")
#source("R/initial_fuel_moisture.R")

# Switches and constants
foa = sf::st_read("input/FOA_n7_elevation.gpkg", quiet=T) |> 
  sf::st_drop_geometry() |> 
  mutate(Elevation_feet = round(elevation * 3.28084))

weather_stream = readr::read_csv("input/weather/ERA5_WeatherStream_Quantiles_RandIG_metric.csv") |> 
    mutate(FOA = as.factor(FOA), year = 2022, month = 7) |> 
  select(FOA, year, month, hour, everything())
weather_percentiles = c(80, 90, 97)
wind_direction = seq(0,359, by = 90)
wind_speed = readr::read_csv("input/weather/ERA5_WindSpeedMax_Quantiles_RandIG_metric.csv") |> 
  mutate(val = round(val*0.6213)) # mph

fms_d = list("D1"=c(3,4,5), "D2"=c(6,7,8), "D3"=c(9,10,11), "D4"=c(12,13,14))
fms_l = list("L1"=c(30,60),"L2"= c(60,90), "L3"=c(90,120), "L4"=c(120,150))
fms_scenarios = list(
  "D1L1" = c(fms_d[[1]], fms_l[[1]]), 
  "D2L2" = c(fms_d[[2]], fms_l[[2]]),
  "D3L3" = c(fms_d[[3]], fms_l[[3]])#,

  #"D1L2" = c(fms_d[[1]], fms_l[[2]]), 
  #"D2L1" = c(fms_d[[2]], fms_l[[1]]),
  #"D3L2" = c(fms_d[[3]], fms_l[[2]])
  ) |> rev()

fbfms = c(102:109, 142:149, 161:165, 183, 186)

landscape_chunks = list.files("input/landscape/chunks_v3/")
chunk_dirs = gsub("LCP_|\\.tif", "", landscape_chunks)

o_format = 2 # tif only

source("R/cleanup_randig.R")
rm_empty_dirs(parent_dir = "output/v2/0_chunks/")


################################################################################
#' Loop over 
#' 1) Fuel Moisture Scenarios
#' 2) Extreme Weather Percentiles
#' 3) Wind Direction Bins
#' 4) (FOA-specific) Wind Speeds / Conditioning Weather

Sys.time()

for (fms in names(fms_scenarios)){
  fms_settings = c("FUEL_MOISTURES_DATA: 24", "0 6 7 11 60 90",
    paste(fbfms, paste(fms_scenarios[[fms]], collapse = " ")))
  for (perc in weather_percentiles){
    for (wd in wind_direction){

      # run directory 
      message("Run: ", fms, " - Perc=", perc, "th - WD=", wd,"°")
      r_dir = file.path("output", "v2", "0_chunks", fms, paste0("p_", perc), paste0("wd_", wd), fsep = "/")
      if (! dir.exists(r_dir)) dir.create(r_dir, recursive = T)
      
      # check if still remaining tasks in this run
      existing_chunk_dirs = list.dirs(r_dir, full.names = F)
      existing_chunk_dirs = existing_chunk_dirs[!existing_chunk_dirs==r_dir]
      chunks_to_process = setdiff(chunk_dirs, existing_chunk_dirs)
    
      if (length(chunks_to_process)==0){
        message("--> complete.")
        next
      }

      for (f in 1:nrow(foa)){
        f_name = foa$name[f]
        
        # check again
        existing_chunk_dirs = list.dirs(r_dir, full.names = F)
        existing_chunk_dirs = existing_chunk_dirs[!existing_chunk_dirs==r_dir]
        chunks_to_process = setdiff(chunk_dirs, existing_chunk_dirs)

        # check if still remaining tasks for this FOA
        foa_chunks_to_process = chunks_to_process[grepl(paste0('^',f_name, '_'), chunks_to_process)] |> sort()
        if (length(foa_chunks_to_process) == 0) next

        # FOA-specific wind speed setting
        ws = filter(wind_speed, quant == perc/100, FOA == f_name) |> pull(val)  # kph
        message("\n", f_name, ": WS = ", ws, " mph; ",  length(existing_chunk_dirs), "/", length(chunk_dirs), " chunks completed.")

        # Run by chunk
        for (fc in foa_chunks_to_process){
          c_dir = file.path(r_dir, fc)
          if (! dir.exists(c_dir)) dir.create(c_dir, recursive = T)
          ch = sub("^.+_", "", c_dir)
          cat(ch, " ")

          # Randig Inputs File
          in_file = file.path(c_dir, paste0(c(fms,perc,wd,fc,"randig.input"), collapse = "_"))
          ignition_file = paste0("D:\\Jo\\FireHazard\\output\\v2\\0_chunks\\D3L3\\p_80\\wd_0\\", fc, "\\_FireSizeList.txt")
          write_input_file(in_file, chunk = fc, ws = ws, wd = wd, duration = 1200,
                            fuel_moisture = fms_settings, firesizelist = ignition_file)
                            
          # Run
          #tic()
          run_cmd = paste(normalizePath(randig_bat), normalizePath(in_file), paste0(normalizePath(c_dir), "\\"), o_format) 
          writeLines(run_cmd, file.path(c_dir, "command.txt"))
          sys::exec_wait(run_cmd, 
                        std_out = file.path(c_dir, "log.txt"),
                        std_err = file.path(c_dir, "error.txt"))
          #t = toc(quiet = T)
          
          # save memory by converting FLP from txt to parquet
          flp_file = file.path(c_dir, "_FLP.txt")
          if (file.exists(flp_file)){
            arrow::read_delim_arrow(flp_file) |> 
              arrow::write_parquet(file.path(c_dir, "_FLP.parquet"))
            unlink(flp_file)
          }
          #ch_progress[ch_progress$foa==foa & ch_progress$wt==weather_type & ch_progress$chunk == ch,]$t = t$toc - t$tic
          #saveRDS(ch_progress, "runtimes_chunks.rds")
        }
      }
      # display progress
      #plot_progress(ch_progress)
      message("\n====== ", Sys.time(), " ======")
    }
  }
}
