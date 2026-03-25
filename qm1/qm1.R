
rm(list=ls())
dev.off()
# Define subject colors
qm1 = "#000F46"
qm2 = "#46C8F0"
stickerColor = qm1

img <- magick::image_read("qm1/lr.png")

final_res<- hexSticker::sticker(img, 
                                package = "qm1", 
                                p_size = 60,
                                p_family = "sans",
                                p_y = 1.5,
                                p_color = stickerColor,
                                s_x = 1.0, 
                                s_y = 0.98, 
                                s_width=1.92,
                                s_height = 0.96,
                                filename="qm1/qm1.png",
                                h_fill="white",
                                h_color = stickerColor,
                                dpi = 600)

plot(final_res)
