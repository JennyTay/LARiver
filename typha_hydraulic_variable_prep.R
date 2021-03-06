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
dat$year <- as.character(dat$year)
dat$source <- as.character(dat$source)


#charles jones thesis
#generate data from typha occurrence based on fig 3-4 and max vel reported on page 38
set.seed(85436)
dat1 <- rnorm(n=20, mean = 30, sd = 8)
hist(dat1)
pres <- ifelse(dat1 >= 111 | dat1 <= 0 , 0, 1)

dat1 <- data.frame(
  "depth_cm" = dat1,
  "occurrence" = pres,
  "season" = "summer",
  "location" = "Arizona",
  "species" = "Typha domingensis",
  "year" = "2002",
  "source" = "Jones, Charles E. 2003. Predicting Cattail responses to re-watering of a travertine stream: Decommissioning the Fossil Springs Dam. Masters Thesis. Northern Arizona University"
)

#make data for paired patches with no typha based on fig 3-4 and page 45 range
set.seed(854776)
dat2 <- rnorm(n=20, mean = 20, sd = 4)
hist(dat2)
pres <- 0

dat2 <- data.frame(
  "depth_cm" = dat2,
  "occurrence" = pres,
  "season" = "summer",
  "location" = "Arizona",
  "species" = "Typha domingensis",
  "year" = "2002",
  "source" = "Jones, Charles E. 2003. Predicting Cattail responses to re-watering of a travertine stream: Decommissioning the Fossil Springs Dam. Masters Thesis. Northern Arizona University"
)

dat1 <- rbind(dat1, dat2)
dat1$season <- as.character(dat1$season)
dat1$location <- as.character(dat1$location)
dat1$species <- as.character(dat1$species)
dat1$year <- as.character(dat1$year)
dat1$source <- as.character(dat1$source)



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
dpth1$year <- as.character(dpth1$year)
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
  filter(season == "september") %>% 
  mutate(season = "summer",
         occurrence = ifelse(density_ramet_per_m2 > 0 , 1, 0),
         source = "J_Grace, J. B., & Wetzel, R. G. (1981). Habitat Partitioning and Competitive Displacement in Cattails (Typha). Experimental Field Studies. American Naturalist, 118(4), 463.474."
      )

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
dpth3c$year <- as.character(dpth3c$year)
dpth3c$depth_cm <- as.numeric(dpth3c$depth_cm)
dpth3c$leaf_ht_cm <- as.numeric(dpth3c$leaf_ht_cm)

#we wont use dpth3c because its about seasonal changes and not about habitat suitability

dpth3 <- left_join(dpth3a, dpth3b, by = c("location", "season", "year", "species", "depth_cm", "source"))
dpth3 <- dpth3 %>% 
  select(1:11, 13:17, 12) %>% 
  mutate(occurrence = ifelse(density_ramet_per_m2 > 0 ,1, 0))

rm(dpth3a, dpth3b)

dpth3$species <- as.character(dpth3$species)
dpth3$location <- as.character(dpth3$location)
dpth3$year <- as.character(dpth3$yea)
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
dpth4$year <- as.character(dpth4$year)
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
dpth5$year <- as.character(dpth5$year)
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
         stand_biomass_g_per_m2 = stand.biomass..g.m.2.) %>% 
  mutate(season = "summer",
         occurrence = ifelse(shoot_ht_cm > 0 , 1, 0))


dpth6$location <- as.character(dpth6$location)
dpth6$year <- as.character(dpth6$year)
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
         gap_length_SD = SD.5) %>% 
  mutate(season = "summer",
         occurrence = ifelse(shoot_ht_cm > 0 , 1, 0))



str(dpth7)

dpth7$location <- as.character(dpth7$location)
dpth7$species <- as.character(dpth7$species)
dpth7$year <- as.character(dpth7$year)
dpth7$depth_cm <- as.numeric(dpth7$depth_cm)
dpth7$source <- as.character(dpth7$source)

#have this additional source, but not sure how to incorportate them because it doesnt have depth, just soil moisutre...
dpth8 <- read.csv("Typha_Depth_Data_Collection/TerHeerdtetal_2017_2.csv")
dpth9 <- read.csv("Typha_Depth_Data_Collection/TerHeerdtetal_2017_3.csv")


#create final depth file

depth <- bind_rows(dat, dat1, dpth1, dpth2, dpth3, dpth4, dpth5, dpth6, dpth7)
depth$species <- tolower(depth$species)
depth$species <- trimws(depth$species)
depth$species[depth$species == "typha glauca godr."] <- "typha glauca"

save(depth, file = "typha_depth.RData")
load("typha_depth.RData")


ref <- depth %>% 
  filter(!is.na(occurrence))
unique(ref$source)

######## Velocity -------------------------------------------------


#1

#make data from typha occurrence based on fig 3-4 and max vel reported on page 38
set.seed(85436)
vel <- rnorm(n=20, mean = 0.06, sd = 0.05)
hist(vel)
pres <- ifelse(vel >= 0.107 | vel <= 0 , 0, 1)

vel1 <- data.frame(
  "vel_m_s" = vel,
  "occurrence" = pres,
  "season" = "summer",
  "location" = "Arizona",
  "species" = "Typha domingensis",
  "year" = "2003",
  "source" = "Jones, Charles E. 2003. Predicting Cattail responses to re-watering of a travertine stream: Decommissioning the Fossil Springs Dam. Masters Thesis. Northern Arizona University"
)

#make data for paired patches with no typha based on fig 3-4 and page 45 range
set.seed(854776)
vel <- rnorm(n=20, mean = 0.82, sd = 0.4)
hist(vel)
pres <- 0

vel2 <- data.frame(
  "vel_m_s" = vel,
  "occurrence" = pres,
  "season" = "summer",
  "location" = "Arizona",
  "species" = "Typha domingensis",
  "year" = "2002",
  "source" = "Jones, Charles E. 2003. Predicting Cattail responses to re-watering of a travertine stream: Decommissioning the Fossil Springs Dam. Masters Thesis. Northern Arizona University"
)

vel <- rbind(vel1, vel2)

str(vel)
vel$season <- as.character(vel$season)
vel$location <- as.character(vel$location)
vel$species <- as.character(vel$species)
vel$year <- as.character(vel$year)
vel$source <- as.character(vel$source)



#2
vel1 <- read.csv("Typha_Velocity_Data Collection/Asaeda2005.csv")
vel1 <- vel1 %>% 
  select(2,3, 6, 8:10, 12, 14, 16, 18) %>% 
  rename(shoot_density_per_m2 = shoot_density_num_per_m2) %>% 
  filter(!is.na(year)) %>% 
  mutate(season = "summer",
         vel_m_s = 0.01*(velocity_cm_s),
         occurrence = ifelse(patchArea_m2 > 0 , 1, 0))


str(vel1)

vel1$location <- as.character(vel1$location)
vel1$species <- as.character(vel1$species)
vel1$year <- as.character(vel1$year)
vel1$source <- as.character(vel1$source)
vel1$Shoots_num <- as.numeric(vel1$Shoots_num)
vel1$Mean_shoot_diameter_mm_long <- as.numeric(vel1$Mean_shoot_diameter_mm_long)


#make final velocity data

velocity <- bind_rows(vel, vel1) %>% 
  select(location, year, season, species, occurrence, 
         vel_m_s, velocity_cm_s, 
         patchArea_m2,  Shoots_num, shoot_density_per_m2, 
         Mean_shoot_diameter_mm_long, Mean_shoot_diameter_mm_short, source)


save(velocity, file = "typha_velocity.RData")




######## Temperature -------------------------------------------------


#prepping data for temperature impacts on Typha

tmp1 <- read.csv("Typha_Temperature_Data_Collection/Lombardi_etal_1997.csv")
head(tmp1)
tmp1 <- tmp1 %>% 
  select(2, 3, 5, 8:11, 13) %>% 
  rename(location = location..country.)
names(tmp1) <- tolower(names(tmp1))


tmp2 <- read.csv("Typha_Temperature_Data_Collection/Morinaga1926.csv")
head(tmp2)
tmp2 <- tmp2 %>% 
  select(1, 2, 4:7, 10) %>% 
  mutate(temperature_range_C = high_temperature_C - low_temperature_C,
         temperature_type = "variable")
names(tmp2) <- tolower(names(tmp2))

tmp3 <- read.csv("Typha_Temperature_Data_Collection/Sifton_2011.csv")
tmp3 <- tmp3 %>% 
  select(2, 4:10, 12) %>% 
  mutate(low_temperature_C = temperature_C,
         high_temperature_C = temperature_C) %>% 
  select(-temperature_C)
names(tmp3) <- tolower(names(tmp3))

tmp4 <- read.csv("Typha_Temperature_Data_Collection/Bonnewell_1983.csv")
names(tmp4)
tmp4 <- tmp4 %>% 
  rename(location = location_where_seed_is_from,
         exp_type = location_of_experiment) %>% 
  mutate(low_temperature_C = temperature_C,
         high_temperature_C = temperature_C,
         temperature_range_c  = high_temperature_C - low_temperature_C,
         temperature_type = "constant") %>% 
  select(-temperature_C)
names(tmp4) <- tolower(names(tmp4))



tmp5 <- read.csv("Typha_Temperature_Data_Collection/TerHeerdtetal_2017_1.csv", nrows = 245)
#in this paper , they reported seeedling emergence, not germination, but we will consider this germination so it can be used with the other ppaers.
str(tmp5)
tmp5 <- tmp5 %>% 
  rename(low_temperature_C = minimum_temperature_C,
         high_temperature_C = maximum_temperature_C,
         germination_perc = seedling_emergence_perc,
         condition = moisture) %>% 
  mutate(temperature_type = "variable",
         temperature_range_C = high_temperature_C - low_temperature_C)

names(tmp5) <- tolower(names(tmp5))



tmp6 <- read.csv("Typha_Temperature_Data_Collection/TerHeerdtetal_2017_2.csv")
head(tmp6)
tmp6$low_temperature_C <- ifelse(tmp6$temperature == "cold", 3,
                                 ifelse(tmp6$temperature == "cool", 10,
                                 ifelse(tmp6$temperature == "intermediate", 15,
                                        ifelse(tmp6$temperature == "warm", 20, 30))))
tmp6$high_temperature_C <- ifelse(tmp6$temperature == "cold", 10,
                                 ifelse(tmp6$temperature == "cool", 20,
                                        ifelse(tmp6$temperature == "intermediate", 25,
                                               ifelse(tmp6$temperature == "warm", 30, 40))))
tmp6 <- tmp6 %>% 
  mutate(temperature_type = "variable",
         temperature_range_C = high_temperature_C - low_temperature_C) %>% 
  select(-temperature) %>% 
  rename(germination_perc = maximum_emergence_capacity_Emax,
         condition = moisture)

names(tmp6) <- tolower(names(tmp6))


tmp7 <- read.csv("Typha_Temperature_Data_Collection/TerHeerdtetal_2017_3.csv")
head(tmp7)
tmp7$low_temperature_C <- ifelse(tmp7$temperature == "cold", 3,
                                 ifelse(tmp7$temperature == "cool", 10,
                                        ifelse(tmp7$temperature == "intermediate", 15,
                                               ifelse(tmp7$temperature == "warm", 20, 30))))
tmp7$high_temperature_C <- ifelse(tmp7$temperature == "cold", 10,
                                  ifelse(tmp7$temperature == "cool", 20,
                                         ifelse(tmp7$temperature == "intermediate", 25,
                                                ifelse(tmp7$temperature == "warm", 30, 40))))
tmp7 <- tmp7 %>% 
  mutate(temperature_type = "variable",
         temperature_range_C = high_temperature_C - low_temperature_C) %>% 
  select(-temperature)  %>% 
  rename(seedling_survial_perc = seedling_emergence_and_survival_perc,
         condition = moisture)

names(tmp7) <- tolower(names(tmp7))



temp <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7)
temp$condition <- trimws(temp$condition)
temp$species <- tolower(temp$species)
temp <- temp %>% 
  mutate(temp_midRng_c = (low_temperature_c+high_temperature_c) / 2)

save(temp, file = "typha_temp.RData")

