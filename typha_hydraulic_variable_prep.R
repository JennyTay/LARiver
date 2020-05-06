library(tidyverse)



######## Depth -------------------------------------------------

#generate depth data using the districution prvided in this text
#Asaeda, T., Fujino, T., & Manatunge, J. (2005). Morphological adaptations of emergent plants to water flow: 
# frm the text: The mean (±SD) depth at which T. angustifolia colonised (54.3 ± 9.3 cm, n = 67) 
set.seed(85436)
dpth <- rnorm(n=67, mean = 54.3, sd = 9.3)
hist(dpth)


pres <- ifelse(dpth >= 70 | dpth <= 40 , 0, 1)

dat <- data.frame(
  "depth_cm" = dpth,
  "occurrence" = pres,
  "season" = "summer",
  "location" = "Tokyo",
  "species" = "Typha angustifolia",
  "year" = "2003",
  "source" = "Asaeda, Takashi & Fujino, Takeshi & Manatunge, Jagath. (2005). Morphological adaptations of emergent plants to water flow: A case study with Typha angustifolia, Zizania latifolia and Phragmites australis. Freshwater Biology."
  )
str(dat)
dat$season <- as.character(dat$season)
dat$location <- as.character(dat$location)
dat$species <- as.character(dat$species)
dat$year <- as.numeric(dat$year)
dat$source <- as.character(dat$source)

#note, if there is a start and an end month, we will use the end month



#1

dpth1a <- read.csv("Typha_Depth_Data_Collection/grace_1985a.csv")
dpth1b <- read.csv("Typha_Depth_Data_Collection/grace_1985b.csv")
dpth1c <- read.csv("Typha_Depth_Data_Collection/grace_1985c.csv")

dpth1a <- dpth1a %>% 
  select( "state", "end.month", "year", "species", "water.depth..cm.", "seeds.that.germinate....", "source") %>% 
  rename(location = state,
         season = end.month, 
         depth_cm = water.depth..cm.,
         germination_perc = seeds.that.germinate....)

dpth1b <- dpth1b %>% 
  select("state", "end.month", "year", "species","water.depth..cm.", "seeds.that.germinated.and.survived....", "source") %>% 
  rename(location = state,
         season = end.month, 
         depth_cm = water.depth..cm.,
         seedling_survial_perc = seeds.that.germinated.and.survived....)

dpth1c <- dpth1c %>%
  select("location..state.", "time..end.month.", "time..year.", "species", "water.depth..cm.", "Plant.Mass..g.", "source") %>% 
  rename(location = location..state.,
         season = time..end.month.,
         year = time..year.,
         depth_cm = water.depth..cm.,
         plant_mass_g = Plant.Mass..g.)

dpth1 <- full_join(dpth1a, dpth1b, by = c("location", "season", "year", "depth_cm", "species", "source"))
dpth1 <- full_join(dpth1, dpth1c, by = c("location", "season", "year", "depth_cm", "species", "source"))
dpth1$season <- "summer"

dpth1 <- dpth1 %>% select(1:6, 8, 9, 7)

rm(dpth1a, dpth1b, dpth1c)

str(dpth1)

dpth1$location <- as.character(dpth1$location)
dpth1$year <- as.numeric(dpth1$year)
dpth1$species <- as.character(dpth1$species)
dpth1$germination_perc <- as.numeric(dpth1$germination_perc)
dpth1$seedling_survial_perc <- as.numeric(dpth1$seedling_survial_perc)
dpth1$source <- as.character(dpth1$source)


#2

dpth2 <- read.csv("Typha_Depth_Data_Collection/grace_wetzel_1981.csv")

dpth2 <- dpth2 %>% 
  select(-X) %>% 
  rename(density_ramet_per_m2 = density_ramet.m2.mean,
         density_ramet_per_m2_sd = density_ramet.m2.sd,
         source = X.1,
         season = month) %>% 
  filter(season == "september")
dpth2$season <- "summer"

dpth2$source <- "J_Grace, J. B., & Wetzel, R. G. (1981). Habitat Partitioning and Competitive Displacement in Cattails (Typha). Experimental Field Studies. American Naturalist, 118(4), 463.474."
dpth2$density_ramet_per_m2 <- as.numeric(dpth2$density_ramet_per_m2)
dpth2$depth_cm <- as.numeric(dpth2$depth_cm)


dpth2$species <- as.character(dpth2$species)
dpth2$density_ramet_per_m2 <- as.numeric(dpth2$density_ramet_per_m2) 
dpth2$depth_cm <- as.numeric(dpth2$depth_cm) 

#3

dpth3a <- read.csv("Typha_Depth_Data_Collection/grace_wetzel_1982a.csv")
dpth3b <- read.csv("Typha_Depth_Data_Collection/grace_wetzel_1982b.csv")
dpth3c <- read.csv("Typha_Depth_Data_Collection/grace_wetzel_1982c.csv")


dpth3a <- dpth3a %>% 
  select(-county) %>% 
  rename(location = state,
         season = month,
         depth_cm = water.depth..cm.,
         density_ramet_per_m2 = Density..ramets.m.2,
         dry_biomass_per_ramet_g = Biomass.production.ramet.g.ashfree.dry.weight,
         rhizomes_per_parent_ramet_num = Number.of.lateral.rhizomes.parent.ramet,
         dry_mean_rhizome_weight_g = Average.rhizome.weight..g.dry.weight,
         mean_rhizome_length_cm = Average.rhizome.length..cm,
         Root_biomass_perc_of_ramet = Root.biomass....of.ramet.weight)
dpth3a$depth_cm <- as.numeric(dpth3a$depth_cm)
dpth3a$density_ramet_per_m2 <- as.numeric(dpth3a$density_ramet_per_m2)
dpth3a$season <- "summer"


dpth3b <- dpth3b %>% 
  select(-county) %>% 
  rename(location = state,
         season = month,
         depth_cm = water.depth..cm.,
         leaf_ht_cm = Leaf.height..cm.,
         leaf_wt_g_per_dry_wt= Leaf.Weight..g.dry.weight,
         leaf_surfaceArea_cm2_per_cm_ht  = Leaf.surface.area..cm.2.cm.height,
         leaf_surfaceArea_cm2_per_gDryWeight = Leaf.surface.area..cm.2.g.dry.weight)
dpth3b$depth_cm <- as.numeric(dpth3b$depth_cm)
dpth3b$leaf_ht_cm <- as.numeric(dpth3b$leaf_ht_cm)
dpth3b$season <- "summer"


dpth3c <- dpth3c %>% 
  select(-location..county.) %>% 
  rename(location = location..state.,
         month = time..month.,
         year = time..year.,
         depth_cm = depth,
         leaf_ht_cm = Leaf.height..cm.,
         leaf_wt_g_per_dry_wt = Leaf.weight..g.dry.weight
          )
dpth3c$year <- as.numeric(dpth3c$year)
dpth3c$depth_cm <- as.numeric(dpth3c$depth_cm)
dpth3c$leaf_ht_cm <- as.numeric(dpth3c$leaf_ht_cm)

#we wont use dpth3c because its about seasonal changes and not about habitat suitability

dpth3 <- left_join(dpth3a, dpth3b, by = c("location", "season", "year", "species", "depth_cm", "source"))
dpth3 <- dpth3 %>% 
  select(1:11, 13:17, 12)

rm(dpth3a, dpth3b)

dpth3$species <- as.character(dpth3$species)
dpth3$location <- as.character(dpth3$location)
dpth3$year <- as.numeric(dpth3$yea)
dpth3$source <- as.character(dpth3$source)

#4

dpth4 <- read.csv("Typha_Depth_Data_Collection/grace_wetzel_1998.csv")


dpth4 <- dpth4 %>%
  rename(location = location..state.,
         year = time..year.,
         depth_cm = depth.range..cm.,
         relative_density_perc = percent.relative.density) %>% 
  separate(depth_cm, into = c("min_depth_cm", "max_depth_cm"), sep = "-")

str(dpth4)

dpth4$location <- as.character(dpth4$location)
dpth4$year <- as.numeric(dpth4$year)
dpth4$species <- as.character(dpth4$species)
dpth4$min_depth_cm <- as.numeric(dpth4$min_depth_cm)
dpth4$max_depth_cm <- as.numeric(dpth4$max_depth_cm)
dpth4$relative_density_perc <- as.numeric(dpth4$relative_density_perc)
dpth4$source <- as.character(dpth4$source)


#5


dpth5 <- read.csv("Typha_Depth_Data_Collection/miao_etal_1997.csv")
dpth5 <- dpth5 %>% 
  rename(year = time..year.,
         water_level_tolerance_m = water.level.tolerance..meters.)

dpth5$location <- as.character(dpth5$location)
dpth5$year <- as.numeric(dpth5$year)
dpth5$species <- as.character(dpth5$species)
dpth5$water_level_tolerance_m <- as.numeric(dpth5$water_level_tolerance_m )
dpth5$source <- as.character(dpth5$source)

#6

dpth6 <- read.csv("Typha_Depth_Data_Collection/waters_Shay_1992a.csv")

dpth6 <- dpth6 %>% 
  select(-time..start.month.) %>% 
  rename(season = time..end.month.,
         depth_cm = depth..cm.,
         shoot_ht_cm = shoot.height..cm.,
         shoot_dry_mass_g = shoot.dry.mass..g.,
         shoot_density_per_m2 = density..shoots.m.2.,
         shoot_density_per_m2_SD = SD......,
         stand_biomass_g_per_m2 = stand.biomass..g.m.2.)
dpth6$season <- "summer"

dpth6$location <- as.character(dpth6$location)
dpth6$year <- as.numeric(dpth6$year)
dpth6$depth_cm <- as.numeric(dpth6$depth_cm)
dpth6$species <- as.character(dpth6$species)
dpth6$shoot_density_per_m2 <- as.numeric(dpth6$shoot_density_per_m2)
dpth6$source <- as.character(dpth6$source)

#7

dpth7 <- read.csv("Typha_Depth_Data_Collection/waters_Shay_1992b.csv")

dpth7 <- dpth7 %>% 
  rename(season = time..month., 
         year = time..year., 
         depth_cm = water.depth..cm., 
         shoot_ht_cm = shoot.height..cm., 
         shoot_ht_SD = SD, 
         veg_mass_g = vegetation.mass..g., 
         veg_mass_SD = SD.1, 
         reproductive_mass_g = reproductive.mass..g., 
         reproductive_mass_SD = SD.2, 
         spike_length_cm = spike.length..cm., 
         spike_length_SD = SD.3, 
         spike_width_cm = spike.width..cm., 
         spike_width_SD = SD.4, 
         gap_length_cm = gap.length..cm., 
         gap_length_SD = SD.5)

dpth7$season <- "summer"

str(dpth7)

dpth7$location <- as.character(dpth7$location)
dpth7$species <- as.character(dpth7$species)
dpth7$year <- as.numeric(dpth7$year)
dpth7$depth_cm <- as.numeric(dpth7$depth_cm)
dpth7$source <- as.character(dpth7$source)


depth <- bind_rows(dat, dpth1, dpth2, dpth3, dpth4, dpth5, dpth6, dpth7)



######## Velocity -------------------------------------------------


######## Temperature -------------------------------------------------


######## Shear Stress -------------------------------------------------

