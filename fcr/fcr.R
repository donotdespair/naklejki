
rm(list = ls())

# cash rate forecast hydra plot
######################################################
PATH        = "/Users/twozniak/Research/forecasting-cash-rate/forecasting-cash-rate.github.io/"

# dates for the plot
id1         = "2019-01" # the beginning of data plot
id2         = "2024-10" # forecast origin
id3         = "2025-11" # the last forecast
time_id     = seq(from = zoo::as.yearmon(id1), to = zoo::as.yearmon(id3), by = 1/12)

# download series
icr_dwnld   = readrba::read_rba(series_id = "FIRMMCRTD")   # Cash Rate Target
icr_tmp     = xts::xts(icr_dwnld$value, icr_dwnld$date)
icr         = xts::to.monthly(icr_tmp, OHLC = FALSE)
icr         = icr[paste0(id1,"/",id2)]

# upload forecasts
files       = list.files(paste0(PATH,"forecasts-backup/"))
files       = files[!grepl("quarter", files)]
fc          = colorRampPalette(c("darkorchid1", "darkorchid4"))
cols        = fc(length(files))


svg("fcr/hydra.svg", width = 4 * 7, height = 4 * 5)
plot(
  x = time_id, 
  y = c(as.vector(icr), rep(NA,13)), 
  main = "",
  ylab = "", 
  xlab = "", 
  ylim = c(0,5),
  lwd = 40, 
  col = "darkorchid4",
  bty = "n", 
  type = "l",
  axes = FALSE
)
for (fi in 1:length(files)) {
  header = TRUE
  if (fi > 16) header = FALSE
  forecasts_tmp = read.csv(paste0(PATH,"forecasts-backup/",files[fi]), header = header)[,1:2]
  if (fi != 20) {
    lines( 
      x = seq(from = zoo::as.yearmon(forecasts_tmp[1,1]) - 1/12, to = zoo::as.yearmon(tail(forecasts_tmp[,1], 1)), by = 1/12),
      y = c(as.numeric(icr[zoo::as.yearmon(forecasts_tmp[1,1]) - 1/12]), forecasts_tmp[,2]),
      col = cols[which(grepl(files[fi], files))],
      lwd = 10
    )
  } else {
    lines( 
      x = seq(from = zoo::as.yearmon(forecasts_tmp[1,1]) - 1/12, to = zoo::as.yearmon(tail(forecasts_tmp[,1], 1)), by = 1/12),
      y = c(as.numeric(icr[zoo::as.yearmon(forecasts_tmp[1,1]) - 2/12]), forecasts_tmp[,2]),
      col = cols[which(grepl(files[fi], files))],
      lwd = 10
    )
  }
}
dev.off()





img <- magick::image_read_svg("fcr/hydra.svg", width = 1.66*1080, height = 1.66*800)
# img |> magick::image_crop(geometry = "1600x1150+100+80")  -> img
# img

library(hexSticker)

sticker_color = "darkorchid1"
# "darkorchid3"
final_res <- sticker(img, 
                     package = "forecasting", 
                     p_size = 4.2,
                     p_family = "sans",
                     p_fontface = "plain",
                     p_y = 1.4,
                     p_x = 0.62,
                     p_color = sticker_color,
                     s_x = 1.0, 
                     s_y = 0.96, 
                     s_width = 2,
                     s_height = 1.1,
                     url = "cash rate",
                     u_size = 5,
                     u_family = "sans",
                     u_color = sticker_color,
                     u_angle = 0,
                     u_x = 0.2,
                     u_y = 1.22,
                     filename = "fcr/fcr.png",
                     h_fill = "white",
                     h_color = sticker_color,
                     h_size = 1.2,
                     dpi = 600) 
plot(final_res)
