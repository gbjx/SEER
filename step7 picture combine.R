rm(list = ls())
library(magick)
img1 <-image_read("result/Relative importance of variables.tif")
img1 <-image_annotate(img1, "A", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img1)

img2 <- magick::image_read("result/number of variables.tif")
img2 <- image_annotate(img2, "B", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img2)

p1<-magick::image_append(c(img1, img2))
p1
ggplotify::as.ggplot(p1)
library(export)
graph2tif(file="result/fig1")

img3 <-image_read("result/logrank plot of trainset.tif")
img3 <-image_annotate(img3, "A", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img3)

img4 <- magick::image_read("result/logrank plot of testsetset.tif")
img4 <- image_annotate(img4, "B", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img4)

img5 <- magick::image_read("result/logrank plot of dataset.tif")
img5 <- image_annotate(img5, "C", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img5)

p2<-magick::image_append(c(img3, img4,img5))
p2
ggplotify::as.ggplot(p2)
library(export)
graph2tif(file="result/figs1")


img5 <-image_read("result/ROC TRAINSET.tif")
img5 <-image_annotate(img5, "A", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img5)

img6 <- magick::image_read("result/ROC testset.tif")
img6 <- image_annotate(img6, "B", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img6)

p3<-magick::image_append(c(img5, img6))
p3
ggplotify::as.ggplot(p3)
library(export)
graph2tif(file="result/fig3")


img7 <-image_read("result/3years calibration.tif")
img7 <-image_annotate(img7, "A", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img7)

img8 <- magick::image_read("result/5years calibration.tif")
img8 <- image_annotate(img8, "B", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img8)

img9 <- magick::image_read("result/cindex.tif")
img9 <- image_annotate(img9, "C", size = 66, gravity = "northwest", color = "black")
ggplotify::as.ggplot(img9)

p4<-magick::image_append(c(img7, img8,img9), stack = TRUE)
p4
ggplotify::as.ggplot(p4)
library(export)
graph2tif(file="result/fig4")
