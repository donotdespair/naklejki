
############################################################
# Reproduction of the IRF from Lütkepohl, Shang, Uzeda, Woźniak (2024)
############################################################

# sticker properties
############################################################
# Define colors
bspink = "#0056B9"
bsyell = "#FFD800"
bsyell_trans  = rgb(t(col2rgb(bsyell, alpha = F)), alpha=170, maxColorValue=255)

stickerColor = bspink

# impulse responses
#######################################################
library(bsvars)
load("bsvars/bsvars_irfs.rda")
irf_med       = apply(irfs, 1, median)
irf_hdi       = apply(irfs, 1, HDInterval::hdi, credMass = 0.7)
irf_hdi[2,15:19] = c(0.020, 0.0165, 0.013, 0.0093, 0.0075)

svg(file = "bsvars/irf_ukr.svg",
    width = 1 * 9,
    height = 1 * 6.5
)
par(
  bg = bspink,
  mar = c(2, 2, 0, 0)
)
graphics::plot(x = 1:length(irf_med), 
     y = irf_med,
     ylim = c(-0.065, 0.06),
     # ylim = range(irf_hdi),
     type = "l",
     col = bsyell,
     lwd = 32,
     ylab = "",
     xlab = "",
     lend = 2,
     axes = FALSE
)
polygon(
  x = c(1:length(irf_med), rev(1:length(irf_med))),
  y = c(irf_hdi[1,], rev(irf_hdi[2,])),
  col = bsyell_trans,
  border = NA
)
abline(
  v = 21.35,
  col = bspink,
  lwd = 30
)

ticks_vertical      = c(seq(from = 0, to = 5, by = 0.05),
                        seq(from = 10, to = 15, by = 0.05),
                        20) + 1
ticks_horizontal    = c(seq(from = -.057, to = 0, by = 0.0005),0,.057)
axis(1, 
     ticks_vertical, 
     rep("",length(ticks_vertical)), 
     col = bsyell, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
axis(2, 
     ticks_horizontal, 
     rep("", length(ticks_horizontal)), 
     col = bsyell, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
dev.off()

# image formattiing and including
img <- magick::image_read_svg("bsvars/irf_ukr.svg", width = 1 * 1080, height = 1 * 840)
# img |> magick::image_crop(geometry = "1450x950+200+240")  -> img

# font adjustments
## Loading Google fonts (http://www.google.com/fonts)
sysfonts::font_add_google("Baloo 2", "font_fam")
# various options I tried for the first argument above:
# "Quicksand" v
# "Comfortaa" vv
# "Rajdhani"
# "Montserrat Alternates" v
# "Mitr" v
# "Baloo 2" vv

## Automatically use showtext to render text for future devices
showtext::showtext_auto()


final_res <- hexSticker::sticker(img,
                                package = "bsvars",
                                p_size = 60,
                                p_family = "font_fam",
                                p_fontface = "bold",
                                p_y = 1.4,
                                p_color = bsyell,
                                s_x = 1,
                                s_y = 0.84,
                                s_width = 1.1,
                                s_height = 1.2,
                                filename = "bsvars/bsvars_ukr.png",
                                h_fill = bspink,
                                h_color = bsyell,
                                h_size = 1.3,
                                dpi = 600)

plot(final_res)

system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/")
system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/bsvars.github.io/")
system("cp bsvars/bsvars.png /Users/twozniak/Research/donotdespair/")
system("cp bsvars/bsvars.png /Users/twozniak/Research/bsvars/presentations/2024-05-bsvars-mcxs/")

# contribute to the README of the hexSticker on GH: https://github.com/GuangchuangYu/hexSticker?tab=readme-ov-file