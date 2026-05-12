
rm(list=ls())
dev.off()

data    = read.csv(file = "qm1/education_data.csv")
returns = data[,2:3]

ret = lm(
  wage ~ education,
  data = returns
)

modelsummary::modelsummary(
  ret, 
  statistic = c("std.error","statistic", "p.value", "conf.low", "conf.high"),
  output = "data.frame"
  # output = "q5.csv"
)


# Define subject colors
qm1 = "#000F46"
qm2 = "#46C8F0"
qm3 = "#FF2D3C"
qm2_rgb = col2rgb(qm2)
qm2_rgb = rgb(qm2_rgb[1], qm2_rgb[2], qm2_rgb[3], 70, maxColorValue = 255)
qm3_rgb = col2rgb(qm3)
qm3_rgb = rgb(qm3_rgb[1], qm3_rgb[2], qm3_rgb[3], 120, maxColorValue = 255)
stickerColor = qm1

# Forecast se computation
xrange = c(8,21)
n   = length(ret$residuals)
su  = sqrt(sum(ret$residuals^2)/(n - 2))
xx  = seq(from = xrange[1] - 1, to = xrange[2] + 1, by = 0.1)
# sx2 = var(returns$education)
# x_bar = mean(returns$education)
# seY = su * sqrt((1/n) + (((xx - x_bar)^2)/((n-1)*sx2)))
reg = ret$coefficients[1] + ret$coefficients[2] * xx
ul  = cbind(reg - 1.96*su, reg + 1.96*su)

set.seed(12)

svg("qm1/qq.svg",  width = 4 * 7, height = 4 * 5, bg = "transparent")
par(
  mar = rep(0,4)
)
plot(
  returns$education + rnorm(length(returns$education), 0, 0.07), 
  returns$wage,
  xlab = "",
  ylab = "",
  main = "",
  pch = 19,
  xlim = xrange,
  col = qm2_rgb,
  bty = "n",
  axes = FALSE,
  type = "n"
)
polygon(
  x = c(xx, rev(xx)),
  y = c(ul[,1], rev(ul[,2])),
  col = "white",
  border = NA
)
polygon(
  x = c(xx, rev(xx)),
  y = c(ul[,1], rev(ul[,2])),
  col = qm2_rgb,
  border = NA
)
ppoint = returns$education + rnorm(length(returns$education), 0, 0.06)
points(
  ppoint, 
  returns$wage,
  pch = 19,
  col = "white",
  lwd = 30
)
points(
  ppoint, 
  returns$wage,
  pch = 19,
  col = qm3_rgb,
  lwd = 30
)
abline(
  a = ret$coefficients[1], 
  b = ret$coefficients[2],
  lwd = 65,
  col = qm1
)
dev.off()



# Define subject colors
stickerColor = qm1

img <- magick::image_read_svg("qm1/qq.svg", width = 1.66*1080, height = 1.66*800)
# img <- magick::image_background(img, color = "none", flatten = TRUE)
final_res<- hexSticker::sticker(img, 
                                package = "qm1", 
                                p_size = 60,
                                p_family = "sans",
                                p_y = 1.5,
                                p_color = stickerColor,
                                s_x = 1.0, 
                                s_y = 0.92, 
                                s_width=1.7,
                                s_height = 1.5,
                                filename="qm1/qm1.png",
                                h_fill="white",
                                h_color = stickerColor,
                                dpi = 600,
)

plot(final_res)
