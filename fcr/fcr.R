
rm(list = ls())

# cash rate forecast hydra plot
######################################################
PATH        = "/Users/twozniak/Research/cash-rate-survey-forecasts/"

# dates for the plot
id1         = "2019-06" # the beginning of data plot
id2         = "2023-09" # forecast origin
id2a        = "2023-09" # data available until
id3         = "2024-09" # the last forecast
time_id     = seq(from = zoo::as.yearmon(id1), to = zoo::as.yearmon(id3), by = 1/12)


# download series
icr_dwnld   = readrba::read_rba(series_id = "FIRMMCRTD")   # Cash Rate Target
icr_tmp     = xts::xts(icr_dwnld$value, icr_dwnld$date)
icr         = xts::to.monthly(icr_tmp, OHLC = FALSE)
icr         = icr[paste0(id1,"/",id2a)]

# upload forecasts
files       = list.files(paste0(PATH,"forecasts-backup/"))
files       = files[!grepl("quarter", files)]
fc          = colorRampPalette(c("darkorchid1", "darkorchid4"))
cols        = fc(length(files))

xs          = matrix(NA, length(files), 13)
ys          = matrix(NA, length(files), 13)

for (i in 1:length(files)) {
  forecasts_tmp = read.csv(paste0(PATH,"forecasts-backup/",files[i]))[,1:2]
  xs[i,]        = seq(from = zoo::as.yearmon(forecasts_tmp[1,1]) - 1/12, to = zoo::as.yearmon(tail(forecasts_tmp[,1], 1)), by = 1/12)
  ys[i,]        = c(as.numeric(icr[zoo::as.yearmon(forecasts_tmp[1,1]) - 1/12]), forecasts_tmp[,2])
}


svg("fcr/hydra.svg", width = 4 * 7, height = 4 * 5)
plot(
  x = time_id, 
  y = c(as.vector(icr), rep(NA,12)), 
  main = "",
  ylab = "", 
  xlab = "", 
  ylim = range(as.vector(icr),ys),
  lwd = 40, 
  col = "darkorchid4",
  bty = "n", 
  type = "l",
  axes = FALSE
)
for (fi in files) {
  forecasts_tmp = read.csv(paste0(PATH,"forecasts-backup/",fi))[,1:2]
  lines( 
    x = seq(from = zoo::as.yearmon(forecasts_tmp[1,1]) - 1/12, to = zoo::as.yearmon(tail(forecasts_tmp[,1], 1)), by = 1/12),
    y = c(as.numeric(icr[zoo::as.yearmon(forecasts_tmp[1,1]) - 1/12]), forecasts_tmp[,2]),
    col = cols[which(grepl(fi, files))],
    lwd = 10
  )
}
dev.off()



img <- magick::image_read_svg("fcr/hydra.svg", width = 1.66*1080, height = 1.66*800)
# img |> magick::image_crop(geometry = "1600x1150+100+80")  -> img
# img

library(hexSticker)


final_res <- sticker(img, 
                     package = "forecast", 
                     p_size = 34,
                     p_family = "sans",
                     p_fontface = "plain",
                     p_y = 1.4,
                     p_x = 0.6,
                     p_color = "darkorchid1",
                     s_x = 0.98, 
                     s_y = 1, 
                     s_width = 2,
                     s_height = 1.13,
                     url = "cash rate",
                     u_size = 30,
                     u_family = "sans",
                     u_color = "darkorchid1",
                     u_angle = 0,
                     u_x = 0.2,
                     u_y = 1.2,
                     filename="fcr/fcr.png",
                     h_fill="white",
                     h_color = "darkorchid4",
                     h_size = 1.2,
                     dpi = 600) 
plot(final_res)



# library(ggplot2)
# library(ggimage)
# 
# hex = ggplot() + geom_hexagon(size = 3, fill = "white", color = "darkorchid4")
# hex = hex + geom_image(aes(x = 0.98, y = 1, image = img), 
#                        data.frame(x = 0.98, y = 1, image = img)
#                        , size = 2, asp = 1)
# 
# hex = hex + geom_url(
#   url = "forecast",
#   x = 1.01,
#   y = 0.14,
#   family = "Aller_Rg",
#   size = 13,
#   color = "darkorchid1",
#   angle = 30,
#   hjust = 0.05
# )
# hex = hex + geom_url(
#   url = "cash rate",
#   x = 1.75,
#   y = 0.57,
#   family = "Aller_Rg",
#   size = 13,
#   color = "darkorchid1",
#   angle = 90,
#   hjust = -0.0
# )
# hex = hex + theme_sticker(size = 1)
# plot(hex)
