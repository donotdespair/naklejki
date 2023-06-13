
rm(list=ls())

# Define subject colors
qaf1 = "#17252A"
qaf2 = "#2B7A28"
qaf3 = "#3AAFA9"
qaf4 = "#DEF2F1"
qaf5 = "#FEFFFF"

# library(FinTS)
# library(rugarch)
# library(normtest)
# library(HARModel)

# Forecasting log-returns of AORD using AR-GARCH models
######################################################
AORD        = read.csv("qaf1/AORD-clean.csv", header = TRUE)
AORD.dates  = AORD[,1]
AORD        = AORD[,2]
AORD.na     = is.na(AORD)
AORD        = AORD[!AORD.na]
AORD.dates  = as.character(AORD.dates[!AORD.na])
AORD.dates  = as.Date(AORD.dates, format = "%Y-%m-%d")

dlAORD      = 100*diff(log(AORD))
dlAORD      = na.omit(dlAORD)
dAORD.dates = AORD.dates[2:length(AORD.dates)]


gjr.m.model.specification = rugarch::ugarchspec(mean.model = list(armaOrder = c(7, 0), archm=TRUE, include.mean = TRUE),
                                       variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                                       distribution.model = "norm")
gjr.m.model.estimation    = rugarch::ugarchfit(spec=gjr.m.model.specification, data=dlAORD, solver = "solnp")

# # Volatility whiskers plot
# gjr.m.model.specification = ugarchspec(mean.model = list(armaOrder = c(7, 0), archm=TRUE, include.mean = TRUE),
#                                        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
#                                        distribution.model = "norm")
h         = 20
H         = 321
# gjr.f     = matrix(NA, H,h)
# colnames(gjr.f) = 1:h
# rownames(gjr.f) = 9269:(9269-H+1)
# pb <- txtProgressBar(max = H, style = 3)
# for (i in 1:H){
#   setTxtProgressBar(pb, i)
#   gjr.m.model     = ugarchfit(spec=gjr.m.model.specification, data=dlAORD[1:(9269-i)], solver = "solnp")
#   gjr.f[i,]       = sigma(ugarchforecast(gjr.m.model, n.ahead = h))
# }
# close(pb)
# save(gjr.f,file="qaf1/qaf1.RData")


load("qaf1/qaf1.RData")
col.sb4      = rgb(t(col2rgb(qaf2, alpha = F)), alpha=100, maxColorValue=255)

svg("qaf1/whiskers.svg", width=4*7, height=4*5)
plot(x=1:(321+h),y=c(dlAORD[8949:9269],rep(NA,h)), type="l", axes=FALSE, col=qaf3, ylab="", xlab="", lwd=4)
lines(x=1:321, y=as.vector(gjr.m.model.estimation@fit$sigma)[8949:9269], col=qaf2,lwd=8)
for (i in 1:H){
  lines(x=(322-i):(321-i+h), y=gjr.f[i,], lwd=8, col=col.sb4)
}
dev.off()

# Define colors
qaf1 = "#2B7A28"
stickerColor = qaf1

img <- magick::image_read_svg("qaf1/whiskers.svg", width = 1.66*1080, height = 1.66*800)
img |> magick::image_crop(geometry = "1600x1150+100+80")  -> img

final_res<- hexSticker::sticker(img, 
                                package = "qaf1", 
                                p_size = 60,
                                p_family = "sans",
                                p_y = 1.5,
                                p_color = stickerColor,
                                s_x = 1.0, 
                                s_y = 0.92, 
                                s_width=2,
                                s_height = 1.04,
                                filename="qaf1/qaf1.png",
                                h_fill="white",
                                h_color = stickerColor,
                                dpi = 600)

plot(final_res)
