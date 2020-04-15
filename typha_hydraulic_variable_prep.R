library(tidyverse)


#Depth

#generate depth data using the districution prvided in this text
#Asaeda, T., Fujino, T., & Manatunge, J. (2005). Morphological adaptations of emergent plants to water flow: 
#A case study with Typha angustifolia, Zizania latifolia and Phragmites australis. Freshwater Biology
# frm the text: The mean (±SD) depth at which T. angustifolia colonised (54.3 ± 9.3 cm, n = 67) 
set.seed(85436)
dpth <- rnorm(n=67, mean = 54.3, sd = 9.3)
hist(dpth)


pres <- ifelse(dpth >= 70 | dpth <= 40 , 0, 1)

dat <- data.frame(
  "depth_cm" = dpth,
  "occurrence" = pres
  )


