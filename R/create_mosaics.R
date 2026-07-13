library(terra)
library(crayon)
library(dplyr)

# ====================================================================================================
# Chunks to single run mosaics (N = 36)

outdir = "output/v2"
fl = list.files(file.path(outdir, "0_chunks"), pattern = "_RandigOutputs.tif$", full.names = T, recursive = T)
mos_tifs = strsplit(fl, "/") |> purrr::map(~ paste("mosaic", .x[[4]], .x[[5]], .x[[6]], ".tif", sep = "_"))

crs = crs("epsg:3035")
foa_poly = vect("input/FOA_n7_elevation.gpkg")
ger = vect("input/de_mainland_etrs.gpkg")  |> terra::aggregate()
just_checking = F
sc = "D1L1"

# check for completeness -----------------------

tif_list = unique(mos_tifs)
for (t in 1:length(tif_list)){
  t_file = file.path(outdir, "1_mosaics", tif_list[t])
  
  if (!file.exists(t_file)){
    tiles = fl[mos_tifs==tif_list[[t]]]
    scc = substr(t_file, 18,21)
    if (!sc == scc){
      message("=========================================================")
      sc = scc
    }
    text = paste0(t, ": ", t_file, "  - N tiles = ", length(tiles))
    message(ifelse(length(tiles)==195, green(text), red(text)))
  }
}


# FOA TIFs -----------------------------------------------------

foa_tifs = strsplit(fl, "/") |> purrr::map(~ paste0(paste("FOA", sub('\\_..?','',.x[[7]]), .x[[4]], .x[[5]], .x[[6]], sep = "_"), ".tif"))
foa_dir = file.path(outdir, "1_mosaics/Randig/1_FOAs")
if (!dir.exists(foa_dir)) dir.create(foa_dir, recursive=T)

foa_tif_list = unique(foa_tifs)
for (t in 1:length(foa_tif_list)){
  t_file = file.path(foa_dir, foa_tif_list[t])
  
  if (!file.exists(t_file)){
    tiles = fl[foa_tifs==foa_tif_list[[t]]] 
    message(basename(t_file))   
    x = lapply(tiles, rast) |> 
      sprc() |> 
      mosaic(fun = 'max') 
    crs(x) = crs
    writeRaster(x, filename = t_file)
  }
}
  
# FOA TIFs weigthed by WD ---------------------------------------

weight_dir = file.path(outdir, "1_mosaics/Randig/2_WD_weighted")
if (!dir.exists(weight_dir)) dir.create(weight_dir, recursive=T)

wd_freq = read.csv("input/weather/ERA5_WindDir_Freq_RandIG_4class_v2.csv")
wd_freq$WD_deg = c(0,270,180,90)
foa_tifs = list.files(foa_dir, full.names = T)
cbp_rcl = matrix(c(seq(0,0.8,0.2), seq(0.2,1, 0.2), 1:5), ncol=3)
cfl_rcl = matrix(c(
  0,2,4,6,8,12, 
  2,4,6,8,12,100,
  1:6), ncol=3)

weight_by_wd = function(wd, w_list){
  x = rast(grep(paste0("wd_", wd, ".tif"), w_list, value=T))
  w = tb$temp_freq[tb$WD_deg == wd]
  x$CBP_weighted = x$`Burn Probability` * w 
  x$CFL_weighted = x$`Conditional Flame Length` * w
  return(x[[c("CBP_weighted", "CFL_weighted")]])
}

for (f in wd_freq$FOA){
  tb = filter(wd_freq, FOA == f)
  foa_buff = foa_poly[foa_poly$name == f,] |> buffer(200)
  foa_mask = terra::intersect(foa_buff, ger)

  for (s in c("D1L1","D2L2","D3L3")){
    for (p in c(80,90,97)){
      w_list = grep(paste0("\\_", f, "\\_", s, "\\_p\\_", p), foa_tifs, value = T)
      w_file = file.path(weight_dir, sub("\\_wd\\_[0,90,180,270]", "", basename(w_list[[1]])))
      
      if (!file.exists(w_file)){
        message(f,'-', s,'-', p,'-', length(w_list))
        w_mos = lapply(c(0,90,180,270), function(x) weight_by_wd(x, w_list = w_list)) |> 
          sprc() |> 
          mosaic(fun = sum) |> 
          mask(foa_mask) |> 
          crop(foa_mask, ext=T)

        maxBP = max(values(w_mos$CBP_weighted), na.rm = T)
        w_mos$CBP_scaled = w_mos$CBP_weighted / maxBP
        w_mos$CBP_class = classify(w_mos$CBP_scaled, cbp_rcl, include.lowest = T, others = NA, right = F)
        w_mos$CFL_class = classify(w_mos$CFL_weighted, cfl_rcl, include.lowest = T, others = NA, right = F)
        writeRaster(w_mos, filename = w_file)
      }
    }
  }
}

# Fire Hazard Mosaics ---------------------------------------
w_tifs = list.files(weight_dir, full.names = T)

burnable = rast("input/landscape/LCP_randig_v3_Germany_epsg3035_100m.tif", lyrs=4) |>
  mask(ger) |> crop(ger, ext=T)
burnable = ifel(burnable > 100, 7, 8)

hazard_class = function(bp, fl){
  h = rep(5, length(bp))
  h[is.na(fl)] = NA

  h[fl==6] = 4
  h[fl==6 & bp>2] = 5

  h[fl==5] = 3
  h[fl==5 & bp>2] = 4
  h[fl==5 & bp==5] = 5

  h[fl==4] = 2
  h[fl==4 & bp>1] = 3
  h[fl==4 & bp>3] = 4

  h[fl==3] = 2
  h[fl==3 & bp>2] = 3
  h[fl==3 & bp==5] = 4

  h[fl==2] = 1
  h[fl==2 & bp>2] = 2
  h[fl==2 & bp==5] = 3

  h[fl==1] = 1
  h[fl==1 & bp>3] = 2
  return(h)
}

# WFH & CBP
col_tb_5 = data.frame(value=c(1:5,7,8), col = c('#4ADCFF', '#93FFBC', '#FAF96A','#F4853B','#E0001B','#7A7A7A','#CCCCCC'))
labels_5 = data.frame(ID=c(1:5, 7,8), class = c('Lowest','Lower','Middle','Higher','Highest','Unburned','Non-Burnable'))

# CFL
col_tb_6 = data.frame(value=c(1:8), col = c('#4ADCFF', '#93FFBC', '#A4E758', '#FAF96A','#F4853B','#E0001B','#7A7A7A','#CCCCCC'))
labels_6 = data.frame(ID=c(1:8), class = c('>0 - 0.6','>0.6 - 1.2','>1.2 - 1.8','>1.8 - 2.4','>2.4 - 3.6', '>3.6','Unburned','Non-Burnable'))

opts = c("COMPRESS=DEFLATE", "TILED=YES")
sc_dir = file.path(outdir, "1_mosaics/Randig/3_Scenarios")
if (!dir.exists(sc_dir)) dir.create(sc_dir, recursive=T)

for (s in c("D1L1","D2L2","D3L3")){
  for (p in c(80,90,97)){
    
    w_list = grep(paste0("\\_", s, "\\_p\\_", p), w_tifs, value = T)
    wfh_file = file.path(sc_dir, paste0("WildFireHazard_class_",s, "_p_", p, ".tif"))
    bp_class_file = file.path(sc_dir, paste0("CondBurnProbability_class_",s, "_p_", p, ".tif"))
    fl_class_file = file.path(sc_dir, paste0("CondFlameLength_class_",s, "_p_", p, ".tif"))
    bp_file = file.path(sc_dir, paste0("CondBurnProbability_continuous_",s, "_p_", p, ".tif"))
    fl_file = file.path(sc_dir, paste0("CondFlameLength_continuous_",s, "_p_", p, ".tif"))
      
    if (!file.exists(wfh_file)){
      message(s,'-', p,'-', length(w_list))

      # combine 7 WD-weighted FOAs for current Scenario/WS combination ---------------------------------------
      # mosaic 200 meter overlap with mean, then round CBD/CFL classes to 
      # i) get lower class if x.5
      # ii) get class mean if x.0
      w_mos = lapply(w_list, rast) |> 
        sprc() |> 
        mosaic(fun = "mean") |> 
        mask(ger) |> 
        crop(ger, ext=T)
      w_mos$CBP_class = round(w_mos$CBP_class)
      w_mos$CFL_class = round(w_mos$CFL_class)
      
      # calculate hazard by combining CBP & CFL ---------------------------------------
      message("calc WFH")
      w_mos$WFH = lapp(w_mos[[c("CBP_class", "CFL_class")]], fun = hazard_class, cores = 4) 
      w_mos$WFH = toMemory(w_mos$WFH)
      gc()

      # process CBP  --------------------------------------------------------------------
      # add classes for non-burnable and burnable (but not burned); add meta data & color table and write
      message("CBP")
      w_mos[["CBP_class"]] = ifel(is.na(w_mos[["WFH"]]), burnable, w_mos[["CBP_class"]])
      coltab(w_mos, layer="CBP_class") = col_tb_5
      levels(w_mos$CBP_class) = labels_5
      metags(w_mos, layer="CBP_class") = c("Variable=Conditional Burn Probability","Units=dimensionless", 
                                            "Notes=Number of times a pixel burned divided by total number of ignitions. Scaled to [0,1] using the analysis maximum. Binned into 5 equal classes.",
                                            paste0("Analysis Maximum=",terra::minmax(w_mos[["CBP_weighted"]])["max",]))
      
      writeRaster(w_mos[["CBP_class"]], names = "CBP", filename = bp_class_file, datatype="INT1U", NAflag = 0, gdal=opts, overwrite=T)
      # write continuous version scaled to 1-100
      message("CBP continuous")
      writeRaster(w_mos[["CBP_scaled"]], names = "CBP", filename = bp_file, datatype="INT1U", gdal=opts, scale = 0.01, overwrite=T)
      gc()

      # process CFL  ------------------------------------------------------------------------------
      # add classes for non-burnable and burnable (but not burned); add meta data & color table and write 
      message("CFL class")
      w_mos[["CFL_class"]] = ifel(is.na(w_mos$WFH), burnable, w_mos[["CFL_class"]])
      coltab(w_mos, layer="CFL_class") = col_tb_6
      levels(w_mos$CFL_class) = labels_6
      metags(w_mos, layer="CFL_class") = c("Variable=Conditional Flame Length", "Units=meters", "Notes=Sum product of flame lengths across 6 fire intensity levels.")
      writeRaster(w_mos[["CFL_class"]], names="CFL", filename = fl_class_file, datatype="INT1U", gdal=opts, NAflag = 0, overwrite=T)  
      # write continuous version in meters
      message("CFL continuous")
      w_mos$CFL_weighted = w_mos$CFL_weighted * 0.3048 # feet to meters
      writeRaster(w_mos[["CFL_weighted"]], names="CFL", filename = fl_file, datatype="INT2U", gdal=opts, scale = 0.01, overwrite=T)  
      gc()

      # process WFH  ------------------------------------------------------------------------------
      # add classes for non-burnable and burnable (but not burned); add meta data & color table and write 
      message("prep WFH")
      w_mos[["WFH"]] = ifel(is.na(w_mos[["WFH"]]), burnable, w_mos[["WFH"]])
      coltab(w_mos, layer="WFH") = col_tb_5
      levels(w_mos$WFH) = labels_5
      metags(w_mos, layer="WFH") = c("Variable=Integrated Wildfire Hazard","Units=dimensionless", "Notes=5 hazard categories: lowest, lower, middle, higher, highest. Classification based on CFL and CBP.")
      message("write WFH")
      writeRaster(w_mos[["WFH"]], filename = wfh_file, datatype="INT1U", gdal=opts)
      gc()
    }
  }
}

# FlamMap Outputs =============================================================================================

fl = list.files(file.path(outdir, "0_chunks"), pattern = "_FlamMapOutputs.tif$", full.names = T, recursive = T)
foa_tifs = strsplit(fl, "/") |> purrr::map(~ paste0(paste("FOA", sub('\\_..?','',.x[[7]]), .x[[4]], .x[[5]], .x[[6]], sep = "_"), ".tif"))
fm_nms = c("Flame Length", "Rate of Spread", "Crown Fraction Burned")

# ---------- FOAs

fl_foa_dir = file.path(outdir, "1_mosaics/Flammap/1_FOAs")
if (!dir.exists(fl_foa_dir)) dir.create(fl_foa_dir, recursive = T)

foa_tif_list = unique(foa_tifs)
for (t in 1:length(foa_tif_list)){
  t_file = file.path(fl_foa_dir, foa_tif_list[t])
  
  if (!file.exists(t_file)){
    tiles = fl[foa_tifs==foa_tif_list[[t]]] 
    message(basename(t_file))   
    x = lapply(tiles, rast, lyrs = c(1,2,18)) |> 
      sprc() |> 
      mosaic(fun = 'max') 
    crs(x) = crs
    writeRaster(x, filename = t_file, names = fm_nms)
  }
}

# ---------- WD weights

weight_by_wd_simple = function(wd, w_list){
  x = rast(grep(paste0("wd_", wd, ".tif"), w_list, value=T))
  w = tb$temp_freq[tb$WD_deg == wd]
  x = x * w 
  return(x)
}
foa_tifs = list.files(fl_foa_dir, full.names = T)
fl_weight_dir = file.path(outdir, "1_mosaics/Flammap/2_WD_weighted")
if (!dir.exists(fl_weight_dir)) dir.create(fl_weight_dir, recursive = T)
for (f in wd_freq$FOA){
  tb = filter(wd_freq, FOA == f)
  foa_buff = foa_poly[foa_poly$name == f,] |> buffer(200)
  foa_mask = terra::intersect(foa_buff, ger)

  for (s in c("D1L1","D2L2","D3L3")){
    for (p in c(80,90,97)){
      w_list = grep(paste0("\\_", f, "\\_", s, "\\_p\\_", p), foa_tifs, value = T)
      w_file = file.path(fl_weight_dir, sub("\\_wd\\_[0,90,180,270]", "", basename(w_list[[1]])))
      
      if (!file.exists(w_file)){
        message(f,'-', s,'-', p,'-', length(w_list))
        w_mos = lapply(c(0,90,180,270), function(x) weight_by_wd_simple(x, w_list = w_list)) |> 
          sprc() |> 
          mosaic(fun = sum) |> 
          mask(foa_mask) |> 
          crop(foa_mask, ext=T)

        writeRaster(w_mos, filename = w_file)
      }
    }
  }
}

# ---------- mosaic weighted FOAs

opts = c("COMPRESS=DEFLATE", "TILED=YES")
fl_sc_dir = file.path(outdir, "1_mosaics/Flammap/3_Scenarios")
if (!dir.exists(fl_sc_dir)) dir.create(fl_sc_dir, recursive = T)
w_tifs = list.files(fl_weight_dir, full.names = T)

for (s in c("D1L1","D2L2","D3L3")){
  for (p in c(80,90,97)){
    w_list = grep(paste0("\\_", s, "\\_p\\_", p), w_tifs, value = T)
    wfh_file = file.path(fl_sc_dir, paste0("Flammap_outputs_",s, "_p_", p, ".tif"))
     if (!file.exists(wfh_file)){
      message(s,'-', p,'-', length(w_list))
       
      w_mos = lapply(w_list, rast) |> 
              sprc() |> 
              mosaic(fun = "mean") |> 
              mask(ger) |> 
              crop(ger, ext=T)
       
      w_mos[[1]] = w_mos[[1]]*100
      w_mos[[2]] = w_mos[[2]]*100
      w_mos[[3]] = w_mos[[3]]*100 
       
     writeRaster(w_mos, filename = wfh_file, datatype="INT2U", gdal=opts)
     }
  }
  }


  
# Firesize -------------------------------------------------

fsfl = list.files(file.path(outdir, "0_chunks"), pattern = "_FireSizeList.txt$", full.names = T, recursive = T)
firesize = strsplit(fsfl, "/") |> purrr::map(~ paste("firesize", .x[[4]], .x[[5]], .x[[6]], ".parquet", sep = "_"))
fs_list = unique(firesize)
if (!dir.exists(file.path(outdir, "2_tables"))) dir.create(file.path(outdir, "2_tables"), recursive = T)


for (i in 1:length(fs_list)){
  f_file = file.path(outdir, "2_tables", fs_list[i])
  
  if (!file.exists(f_file)){
    
    tiles = fsfl[firesize==fs_list[[i]]]
    fs_table = vroom::vroom(tiles, id="chunk", show_col_types = FALSE) |> 
      mutate(Size_ha = round(Acres * 0.40468564),
      chunk = sub(file.path(outdir, "0_chunks","/"), "", dirname(chunk))) |> 
      tidyr::separate(col = chunk, into = c("Scenario", "Perc", "WD", "FOA"), sep="\\/") |> 
      tidyr::separate(col = FOA, into = c("FOA", "Chunk"), sep="\\_") |> 
      mutate(Perc = as.numeric(sub("p_", "", Perc)),
              WD = as.numeric(sub("wd_", "", WD)),
            Chunk = as.numeric(Chunk)) |> 
      select(Scenario, Perc, WD, FOA, Chunk, Size_ha, XStart, YStart)

    text = paste0(i, ": ", f_file, "  - Fire Size [ha]: Median = ", median(fs_table$Size_ha), "; Mean = ",  round(mean(fs_table$Size_ha)), "; Max = ",  max(fs_table$Size_ha))
    message(ifelse(length(tiles)==195, green(text), red(text)))

    arrow::as_arrow_table(fs_table) |> arrow::write_parquet(f_file)

  }
}


#====================================================================================================
# Single run mosaics to weighted averages

read.csv("input/weather/ERA5_WindDir_Freq_RandIG.csv")
