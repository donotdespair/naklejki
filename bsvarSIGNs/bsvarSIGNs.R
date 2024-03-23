
############################################################
# Reproduction of the IRF from Anatolin-Diaz & Rubio-Ramirez (2018, AER)
############################################################

# estimate the model
############################################################
# devtools::install_github("bsvars/bsvarSIGNs")
library(bsvarSIGNs)

# upload data
data(oil)

# restrictions as in Antolín-Díaz & Rubio-Ramírez (2018)
sign_narrative = matrix(c(2, -1, 3, 2, 236, 0), ncol = 6)
sign_irf       = array(matrix(c(-1, -1, 1, 1, 1, 1, 1, -1, 1), nrow = 3),
                       dim = c(3, 3, 1))

# specify the model and set seed
set.seed(123)
specification  = specify_bsvarSIGN$new(oil,
                                       p              = 12,
                                       sign_irf       = sign_irf,
                                       sign_narrative = sign_narrative,
                                       max_tries      = 1e6,
                                       )
burn_in        = estimate(specification, S = 500)
posterior      = estimate(burn_in, S = 1000, thin = 5)

# save(post, spec, file = paste0("spartan/results/tax23nPM.rda"))

# sticker properties
############################################################
# Define colors
bsora  = "#E93CAC"
bsblu  = "#1E22AA"


# bsyell_trans  = rgb(t(col2rgb(bsyell, alpha = F)), alpha=170, maxColorValue=255)

stickerColor = bsblu

# impulse responses
#######################################################
irfs    = compute_irf(posterior, horizon = 20)
N       = 200
irfs    = irfs[,,, (dim(irfs)[4] - N):dim(irfs)[4]]

i = 3; j = 1

irfs_med = apply(irfs[i, j,,], 1, median)
which_med   = which.min(apply((irfs[i,j,,] - irfs_med)^2, 1, sum))



# figure
#######################################################
svg(file = "bsvarSIGNs/irf.svg",
    width = 1.2 * 9,
    height = 1 * 6.5
)
par(
  bg = bsblu,
  mar = c(2, 2, 0, 0)
)
plot.ts(
  irfs[i, j,, 1], 
  col = bsora,
  ylim = range(-9, 9),
  ylab = "",
  xlab = "",
  lend = 2,
  axes = FALSE
)
for (n in 1:ncol(irfs[i, j,,])) {
  lines(
    irfs[i, j,, n],
    col = bsora
  )
}
# lines(
#   irfs[i, j,, which_med],
#   col = bsora,
#   lwd = 16,
#   lend = 2
# )

ticks_vertical      = c(seq(from = 0, to = 5, by = 0.05),
                        seq(from = 10, to = 15, by = 0.05),
                        20) + 1
ticks_horizontal    = c(-9, 0, seq(from = 0, to = 9, by = 0.01))
axis(1, 
     ticks_vertical, 
     rep("",length(ticks_vertical)), 
     col = bsora, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
axis(2, 
     ticks_horizontal, 
     rep("", length(ticks_horizontal)), 
     col = bsora, 
     lwd = 12, 
     lwd.ticks = 12,
     tcl = -1
)
dev.off()

# image formattiing and including
img <- magick::image_read_svg("bsvarSIGNs/irf.svg", width = 2 * 1.2 * 1080, height = 2 * 1 * 840)
# img |> magick::image_crop(geometry = "1450x950+200+240")  -> img

# font adjustments
## Loading Google fonts (http://www.google.com/fonts)
sysfonts::font_add_google("Baloo 2", "font_fam")

## Automatically use showtext to render text for future devices
showtext::showtext_auto()


final_res <- hexSticker::sticker(img,
                                package = "bsvarSIGNs",
                                p_size = 80,
                                p_family = "font_fam",
                                p_fontface = "bold",
                                p_y = 1.38,
                                p_color = bsora,
                                s_x = 1,
                                s_y = 0.84,
                                s_width = 1.1,
                                s_height = 1.0,
                                filename = "bsvarSIGNs/bsvarSIGNs.png",
                                h_fill = bsblu,
                                h_color = bsora,
                                h_size = 1.3,
                                dpi = 1200)

plot(final_res)

# system("cp bsvarSIGNs/bsvarSIGNs.png /Users/twozniak/Research/bsvarSIGNs/")

# contribute to the README of the hexSticker on GH: https://github.com/GuangchuangYu/hexSticker?tab=readme-ov-file