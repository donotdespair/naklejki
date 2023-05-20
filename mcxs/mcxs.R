

############################################################
# Macroeconometrics: ECOM90007, ECOM40003
# prepared by Tomasz Woźniak
# R file for Lecture 6: Understanding Unit Rooters
############################################################

# Define colors
mcxs1  = "#05386B"
mcxs2  = "#379683"
mcxs3  = "#5CDB95"
mcxs4  = "#8EE4AF"
mcxs5  = "#EDF5E1"
purple = "#b02442"

############################################################
# Reproduction of the results from Sims, Uhlig (1991)
# Understanding Unit Rooters: A Helicopter Tour
############################################################
stickerColor = purple

library(plot3D)

set.seed(123456)                                # for the reproduction of the results

ar1.ols    = function(x){
  # a function to estimate the autoregressive coefficient of the AR(1) model 
  as.numeric(solve(t(x[1:(length(x)-1)])%*%(x[1:(length(x)-1)]))%*%t(x[1:(length(x)-1)])%*%(x[2:length(x)]))
}

S           = 50000                             # simulation size
T           = 100                               # dimention of the simulated data
sigma2      = 1                                 # fix the variance of the error term in the AR(1) process with zero constant term and autregressive coefficient rho

rho.grid    = seq(from=0.8, to=1.1, by=0.01)    # the grid of values of rho considered by Sims & Uhlig (1991)
R           = length(rho.grid)
wn          = matrix(rnorm(S*T, sd=sigma2),T,S)            # generate the S white noise processes
Y           = array(NA,c(T,S,R))
rho.bins    = c(-Inf,seq(from=0.795, to=1.105, by=0.01),Inf)    # the bounds for the bins of the histogram of rho.ols

pb <- txtProgressBar(max = length(rho.grid), style = 3)
for (r in 1:length(rho.grid)){
  setTxtProgressBar(pb, r)
  H               = diag(T)
  H[2:T,1:(T-1)]  = H[2:T,1:(T-1)] - rho.grid[r]*diag(T-1)
  Y[,,r]          = solve(H)%*%wn               # create the data according to the DGP: y_t = rho_r * y_{t-1} + wn_t
}
close(pb)

rho.ols     = apply(Y,2:3,ar1.ols)               # ols estimation

c.rho       = matrix(NA,length(rho.bins)-3,length(rho.bins)-2)
for (r in 1:R){
  hh        = hist(rho.ols[,r],breaks=rho.bins, plot=FALSE)
  c.rho[r,] = hh$counts[2:(length(rho.bins)-1)]
}
scaling     = 20
p.rho       = scaling*c.rho/sum(c.rho)

# jpeg(file="ur.jpg", width=2*1080,height=2*840)
pdf(file="mcxs/ur.pdf", width=1.66*9,height=1.66*7)
f3    = persp3D(x=rho.grid[1:21], y=rho.grid[1:31], z=p.rho[1:21,1:31], phi=12, theta=110, zlim=c(0,p.rho[21,21]), 
                xlab="", ylab="", zlab="", shade=.01, border="black", nticks=3,cex.lab=1.5, col="white", scale=FALSE, box=FALSE, bty="f")
f3.l1 = trans3d(x=rho.grid[1:21], y=rho.grid[1:21], z=diag(p.rho[1:21,1:21]), pmat=f3)
lines(f3.l1, lwd=1)
f3.l2 = trans3d(x=rho.grid[21], y=rho.grid[1:31], z=p.rho[21,1:31], pmat=f3)
lines(f3.l2, lwd=12, col=stickerColor)
f3.l3 = trans3d(x=rho.grid[21], y=rho.grid[21], z=c(0,p.rho[21,21]), pmat=f3)
lines(f3.l3, lwd=1)
f3.l4 = trans3d(x=rho.grid[1:21], y=rho.grid[21], z=rep(p.rho[21,21],2), pmat=f3)
lines(f3.l4, lwd=1)
f3.l5 = trans3d(x=rho.grid[1], y=rho.grid[21], z=c(0,p.rho[21,21]), pmat=f3)
lines(f3.l5, lwd=1)
f3.l6 = trans3d(x=rho.grid[1:21], y=rho.grid[21], z=rep(0,2), pmat=f3)
lines(f3.l6, lwd=1)
dev.off()


img <- magick::image_read("mcxs/ur.pdf")
img |> magick::image_crop(geometry = "930x630+90+90")  -> img

final_res<- hexSticker::sticker(img, package="mcxs", p_size=30,
                   p_family = "sans",
                   p_y = 1.5,
                   p_color = stickerColor,
                   s_x=0.95, s_y=0.8, 
                   s_width=2,
                   s_height = 0.96,
                   filename="mcxs/mcxs.png",h_fill="white",h_color = stickerColor)

plot(final_res)
