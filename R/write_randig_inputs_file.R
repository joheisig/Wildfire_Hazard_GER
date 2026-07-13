p = function(a, b) paste(a,b, sep = ": ")

write_input_file = function(file = "C:\\Users\\jheisig\\Desktop\\FireHazard\\randig.input", 
                            foa = "NE", chunk = NULL, ws = 5, wd = 270, duration = 600, res = 180,
                            fuel_moisture, raws, nfires = 0){
  
  # ig_file = grep(x = list.files(paste0("C:\\Users\\jheisig\\Desktop\\FireHazard\\input\\FOA\\ignitions\\"),
  #                               full.names = T), pattern = paste0("\\",foa,"_ignitions"), value = T)
  if (is.null(chunk)){
    ig_file = paste0("C:\\Users\\jheisig\\Desktop\\FireHazard\\input\\FOA\\ignitions\\", foa,  "_ignitions.txt")
    lsc = paste0("C:\\Users\\jheisig\\Desktop\\FireHazard\\input\\landscape\\landscape_GER_", foa, ".tif")
  } else {
    ig_file = paste0("C:\\Users\\jheisig\\Desktop\\FireHazard\\input\\FOA\\ignitions\\chunks\\", 
                     foa, "_", chunk,  "_ignitions.txt")
    lsc = paste0("C:\\Users\\jheisig\\Desktop\\FireHazard\\input\\landscape\\chunks\\", foa, "_", chunk, ".tif")
  }
  
  opts = c(
  p("Landscape", lsc),
  p("FireListFile", ig_file),
  p("NUMFIRES", nfires),
  p("DURATION", duration),
  p("RESOLUTION", res),
  p("SPOTPROBABILITY", 0.15),
  p("SPOTTING_SEED", 599695),
  #OUTPUTFIREPERIMS: 1
  #TargetBurnProportion: 0.970
  #MinimumNumberFires: 10000
  "FOLIAR_MOISTURE_CONTENT: 100",
  "CROWN_FIRE_METHOD: Finney",
  p("WIND_SPEED", ws),
  p("WIND_DIRECTION", wd),
  "SPREAD_DIRECTION_FROM_MAX: 0",
  "GRIDDED_WINDS_GENERATE: Yes",
  "GRIDDED_WINDS_RESOLUTION: 360"
  )
  
  # fuel moisture
  
  # RAWS
  
  ################################
  # Outputs
  outputs = c(
    "RANDIG_BURN_PROBABILITY:"
    ,"RANDIG_FLP_CLASS_0_2:"
    ,"RANDIG_FLP_CLASS_2_4:"
    ,"RANDIG_FLP_CLASS_4_6:"
    ,"RANDIG_FLP_CLASS_6_8:"
    ,"RANDIG_FLP_CLASS_8_12:"
    ,"RANDIG_FLP_CLASS_12:"
    ,"RANDIG_CONDITIONAL_FLAMELENGTH:"
    ,"RANDIGFIRESIZELIST:"
    ,"RANDIGPERIMETERS:"
    #,"RANDIGEMBERS:"
    #,"FLAMELENGTH:"
    #,"SPREADRATE:"
    #,"INTENSITY:"
    #,"HEATAREA:"
    #,"CROWNSTATE:"
    #,"CROWNFRACTIONBURNED:"
    #,"SOLARRADIATION:"
    #,"FUELMOISTURE1:"
    #,"FUELMOISTURE10:"
    #,"FUELMOISTURE100:"
    #,"FUELMOISTURE1000:"
    #,"WINDDIRGRID:"
    #,"WINDSPEEDGRID:"
    #,"MIDFLAME:"
    #,"HORIZRATE:"
    #,"MAXSPREADDIR:"
    #,"ELLIPSEDIM_A:"
    #,"ELLIPSEDIM_B:"
    #,"ELLIPSEDIM_C:"
    #,"MAXSPOT:"
    #,"MAXSPOT_DIR:"
    #,"MAXSPOT_DX:"
  ) 
  writeLines(c(opts, fuel_moisture, raws, outputs), con = file, sep = "\n")    
}

options(dplyr.summarise.inform = F)
plot_progress = function(x){
  if ("chunk" %in% names(x)){
    x = dplyr::group_by(x, foa, wt) |> dplyr::summarise(t = sum(t, na.rm = T))
    x$region = x$foa
  } 
  
  p = dplyr::mutate(x, `Runtime [h]` = t/3600, ) |> 
    ggplot2::ggplot(ggplot2::aes(x=wt, y=`Runtime [h]`, color=region)) +
    ggplot2::geom_jitter(size=6, alpha=0.7, width = 0.3) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle('Randig Fire Simulation Progress')
  suppressWarnings(print(p))
}
