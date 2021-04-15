
library(magick)

#animate 
imgs <- list.files("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/Crisis_steps", full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1,loop = 0)
img_animated
image_write(image = img_animated,path = "C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/Crisis_steps/animated.gif")


#animate 
imgs <- list.files("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/Modelling_depths", full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1,loop = 0)
img_animated
image_write(image = img_animated,path = "C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/Modelling_depths/animated.gif")
