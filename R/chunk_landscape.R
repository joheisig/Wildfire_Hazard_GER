library(stars)


#tifs = list.files(pattern = ".tif$", full.names = T)
foa = st_read("input/FOA_n7_elevation.gpkg") |> 
  dplyr::mutate(name = c('NW','SW','S','ALPS','CE','NE','W')) |>
  dplyr::arrange(name)
plot(foa |> st_union(), border="red", col=0)

lsc_file = "input/landscape/LCP_randig_v2_Germany_epsg3035_100m.tif"
l = lsc_file|> 
  stars::read_stars(proxy = T, quiet = T)

# chunk by FOA
foa_buff = st_buffer(foa, 20000)
plot(foa_buff$geom)
plot(foa, add=T, border="green", color=0)
plot(foa |> st_union(), add=T, border="red", color=0)

foa_buff = st_intersection(foa_buff, st_union(foa))


for (i in 1:nrow(foa_buff)){ 
  out = paste0("landscape/FOAs/lsc_90m_ETRS_", foa_buff$name[i], ".tif")
  stars::write_stars(l[foa_buff[i,]], out, type = "Int16")
  message(out)
}

# ger = read_stars(tifs[1])

# bbs = purrr::map(tifs[2:7], function(x) {
#   read_stars(x) |> st_bbox() |> st_as_sfc() |> st_make_grid(50000)
#   })
# names(bbs) = basename(tifs[2:7])
# bbs

# plot(st_bbox(ger) |> st_as_sfc(), reset=F)
# purrr::map(1:6, function(x) plot(bbs[[x]], border=x, add=T))

##############
u_foa = st_union(foa)
grid_foas = function(i,...){
  g = foa[i,] |> 
    st_buffer(10000) |> 
    st_intersection(u_foa) |> 
    st_make_grid(...) |> 
    st_as_sf() |> 
    st_filter(foa[i,])
  g$foa = foa$name[i]
  g$block = 1:nrow(g)
  b = st_buffer(g, 5000, nQuadSegs = 0) 
  plot(g |> st_geometry(), main=foa$name[i])
  plot(b |> st_geometry(), main=foa$name[i], border = "yellow3", add=T)
  plot(foa[i,]$geom, add=T, border ="red")
  
  return(g)
}

blocks_foas = rbind(
grid_foas(1, cellsize = c(64000,62000))
,grid_foas(2, cellsize = c(58000, 56000))
,grid_foas(3, cellsize = c(59000, 58000))
,grid_foas(4, cellsize = c(58000, 62000))
,grid_foas(5, cellsize = c(62000, 48000))
,grid_foas(6, cellsize = c(67000, 57000))
,grid_foas(7, cellsize = c(56000, 52000))
)
names(blocks_foas)[3] = "geometry"
st_geometry(blocks_foas) = "geometry"

blocks_foas = st_buffer(blocks_foas, 5000, nQuadSegs = 0) 
plot(blocks_foas["foa"], col = "transparent")

st_write(blocks_foas, "FOA/FOA_7_chunks_randig_195.gpkg", delete_dsn = T)


blocks_foas = st_read("input/FOA_7_chunks_randig_195.gpkg")

library(terra)

b_vec = vect(blocks_foas)
ls = rast(lsc_file)
ls[[4]][ls[[4]] %in% c(0, 99, 255)] = 98
freq(ls[[4]])

for (f in foa$name){
  message(f)
  out = paste0("input/landscape/chunks/LCP_", f, "_.tif")
  makeTiles(ls, b_vec[b_vec$foa == f,], filename = out, datatype = "INT2S",
            overwrite = T, wopt = list(gdal = c("COMPRESS=DEFLATE","PREDICTOR=2")))
}


(elev = extract(ls[[1]], vect(foa), fun=mean, na.rm=T))
foa$elevation = elev$LCP_randig_v1_Germany_epsg3035_100m |> round()

st_write(foa, "FOA/FOA_n7_elevation.gpkg", delete_dsn = T)

##########################  v3

ls1 = rast("input/landscape/LCP_randig_v1_Germany_epsg3035_100m.tif")

plot(ls1["LCP_randig_v2_Germany_epsg3035_100m_4"])
freq(ls1["LCP_randig_v1_Germany_epsg3035_100m_4"])


ls2 = rast("input/landscape/LCP_randig_v2_Germany_epsg3035_100m.tif")

plot(ls2["LCP_randig_v2_Germany_epsg3035_100m_4"])
freq(ls2["LCP_randig_v2_Germany_epsg3035_100m_4"])

# TU1 and TL3 unchanged
fbfm3 = rast("input/landscape/GER_FBFM_FireRes_100m_nearForest_artifactsRemoved_epsg3035.tif")
freq(fbfm3)

fbfm4 = crop(fbfm3, ls2, snap="near", extend=T)
fbfm4
ls2

ext(fbfm4) = ext(ls2)
ext(fbfm4) 
ext(ls2)

fbfm4[fbfm4 == 161] = 165
fbfm4[fbfm4 == 183] = 165

values(ls2$LCP_randig_v2_Germany_epsg3035_100m_4) = values(fbfm4$FuelModelClassLUT22)
ls2[ls2[[4]] %in% c(0, 99, 255)] = 98
freq(ls2[[4]])

writeRaster(ls2, "input/landscape/LCP_randig_v3_Germany_epsg3035_100m.tif",
            datatype = "INT2S",
            overwrite = T, wopt = list(gdal = c("COMPRESS=DEFLATE","PREDICTOR=2")))

for (f in foa$name){
  message(f)
  out = paste0("input/landscape/chunks_v3/LCP_", f, "_.tif")
  makeTiles(ls2, b_vec[b_vec$foa == f,], filename = out, datatype = "INT2S",
            overwrite = T, wopt = list(gdal = c("COMPRESS=DEFLATE","PREDICTOR=2")))
}