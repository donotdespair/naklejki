
rm(list = ls())

library(hexSticker)

sticker_color = "darkorchid4"
fill_color    = "darkorchid1"

img <- magick::image_read_svg("just_hex/hydra.svg", width = 1.66*1080, height = 1.66*800)

final_res <- sticker(img, 
                     package = "", 
                     p_size = 4.2,
                     p_family = "sans",
                     p_fontface = "plain",
                     p_y = 1.4,
                     p_x = 0.62,
                     p_color = sticker_color,
                     s_x = 10.0, 
                     s_y = 0.96, 
                     s_width = 2,
                     s_height = 1.1,
                     url = "",
                     u_size = 5,
                     u_family = "sans",
                     u_color = sticker_color,
                     u_angle = 0,
                     u_x = 0.2,
                     u_y = 1.22,
                     filename = "just_hex/just_hex.png",
                     h_fill = fill_color,
                     h_color = sticker_color,
                     h_size = 2,
                     dpi = 600) 
plot(final_res)
