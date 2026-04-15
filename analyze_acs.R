


# load libraries
library(dplyr)
library(ggplot2)
library(here)
library(patchwork)

# set working directory to folder file is in
setwd(here::here())

# create a vector with the DMV metro area counties:
dmv_counties <-
  c(
  "Alexandria city, Virginia",
  "Arlington County, Virginia", 
  "Charles County, Maryland",
  "Clarke County, Virginia",
  "Culpeper County, Virginia",
  "Fairfax County, Virginia", "Fairfax city, Virginia",
  "Falls Church city, Virginia",
  "Fauquier County, Virginia",
  "Fredericksburg city, Virginia",
  "Frederick County, Maryland",
  "Loudoun County, Virginia",
  "Madison County, Virginia",
  "Manassas Park city, Virginia", "Manassas city, Virginia",
  "Montgomery County, Maryland",
  "Prince George's County, Maryland",
  "Prince William County, Virginia",
  "Rappahannock County, Virginia",
  "Spotsylvania County, Virginia",
  "Stafford County, Virginia",
  "Warren County, Virginia",
  "District of Columbia, District of Columbia"
)

# load csv files for the 2020-2024 and 2010-2014 5-year ACS, then
# subset to the counties in the DMV metro area:
# https://en.wikipedia.org/wiki/Washington_metropolitan_area, then
# select only variables we need (see code book for variable names), then
# calculate and rename key variables
df14 <- 
  read.csv(file = "nhgis0105_csv/nhgis0105_ds207_20145_county.csv") %>%
  select(
    YEAR, STUSAB, STATE, COUNTY, NAME_E, starts_with("ABZ")
  ) %>%
  filter(NAME_E %in% dmv_counties) %>%
  group_by(YEAR, STATE) %>%
  reframe(
    n_utility_gas = sum(ABZEE003 + ABZEE013),
    n_elec_or_sol = sum(ABZEE005 + ABZEE009 + ABZEE015 + ABZEE019),
    n_tot         = sum(ABZEE001),
    pct_gas       = n_utility_gas / n_tot * 100,
    pct_elec      = n_elec_or_sol / n_tot * 100
  ) 

df24 <- 
  read.csv(file = "nhgis0105_csv/nhgis0105_ds273_20245_county.csv") %>%
  select(
    YEAR, STUSAB, STATE, NAME_E, starts_with("AVF")
  ) %>%
  filter(NAME_E %in% dmv_counties) %>%
  group_by(YEAR, STATE) %>%
  reframe(
    n_utility_gas = sum(AVF5E003 + AVF5E013),
    n_elec_or_sol = sum(AVF5E005 + AVF5E009 + AVF5E015 + AVF5E019),
    n_tot         = sum(AVF5E001),
    pct_gas       = n_utility_gas / n_tot * 100,
    pct_elec      = n_elec_or_sol / n_tot * 100
  )

# bind the two data sets into one:
df <- bind_rows(df14, df24)

# load the AHRI data, downloaded from the Rocky Mountain Inst.
# https://rmi.org/insight/tracking-the-heat-pump-water-heater-market-in-the-united-states/
# this shows the annual # of units shipped (in millions of units), from 2004 to 2025
ahri <- 
  read.csv("ahri_data.csv") %>%
  tidyr::pivot_longer(cols = c("Gas.Furnaces", "Heat.Pumps")) %>%
  mutate(name = ifelse(name=="Heat.Pumps", "Heat pumps", "Gas furnaces"))
  

# plot bar charts!
p3 <-
  ggplot(ahri, aes(x=Year)) +
  geom_line(aes(y=value, color=name), linewidth = 1.3) +
  theme_minimal() +
  theme(
    legend.position = c(.75, .2),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("Gas furnaces" = "#9db2a3", 
                                "Heat pumps" = "#0f9535")) +
  xlab('') +
  ylab('Millions of units shipped / year') +
  ggtitle("Nationally, more heat pump units are being shipped...")


p1 <-
  ggplot(df) +
  geom_bar(
    aes(y=pct_elec, x=YEAR), 
    stat="identity", position="dodge", fill="#0f9535") +
  facet_wrap(~STATE) +
  theme_minimal() +
  ylab("% heating w/ electricity") + 
  xlab("") +
  ylim(c(0, 60)) +
  theme(axis.text.x = element_blank()) +
  ggtitle("... and more DMV'ers are installing electric heat pumps")

p2 <-
  ggplot(df) +
  geom_bar(
    aes(y=pct_gas, x=YEAR), 
    stat="identity", position="dodge", fill="#9db2a3") +
  facet_wrap(~STATE) +
  theme_minimal() +
  ylab("% heating w/ utility gas") + 
  xlab("") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )


  

p3 + p1 / p2



