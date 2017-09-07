##This script, along with the data in the CCU 15 Replication Files will recreate the analysis for 
##GLP's 2015 Competitive City Update

#While divided into sections for convenience, sections often depend on data and calculations 
#loaded in earlier sections. Thus, attempting to go out of order will cause the code to not work

#loading libraries needed for the analysis. If you have not installed them you will need to first use
#install.packages() 
library(dplyr)
library(ggplot2)
library(classInt)
library(ggthemes)
library(reshape2)
library(ineq)

#setting the working directory
setwd("C:/Users/natek/OneDrive/GLP/CCU 15 Replication Files/")

################################
##Louisville Neighborhood Data##
################################

##choosing your neighborhood areas
#the csv "census tract neighborhoods" was used to create the report
#the csv "tract_to_market_area" was used in the appendix. The line to use this is commented out in this code.
#the line to write out the alternate neighborhood data is also commented out. 

tract_neigh<-read.csv("./input data/Neighborhood IDs/census tract neighborhoods.csv",header=T)
#tract_neigh<-read.csv("./input data/Neighborhood IDs/tract_to_market_area.csv",header=T)
#tract_neigh = tract_neigh %>% rename(Id2 = GEOID10, Neighborhood = MARKET_AREA)

##Now each data set is read in, transformed, and neighborhood values calculated. 

#education
ed_data<-read.csv("./input data/JC Tracts/ACS_14_5YR_B23006_with_ann.csv",header=T, skip=1)
ed_data<-full_join(ed_data, tract_neigh, by = "Id2")
ed_data<-select(ed_data, Id, Id2, Neighborhood, Geography, population = Estimate..Total., 
                less_than_hs = Estimate..Less.than.high.school.graduate.,
                bachelors_up = Estimate..Bachelor.s.degree.or.higher.)
ed_data<-mutate(ed_data, bach_percent = bachelors_up/population,
                less_than_hs_percent = less_than_hs/population)

#note: I create new data at the neighborhood level. Later the census tract level will also be used
n_ed_data<-ed_data %>%
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population), 
         bach_pct = bachelors_up/population,
         hs_pct = less_than_hs/population,
         bach_wgt = bach_pct*wgt,
         hs_wgt = hs_pct*wgt) %>%
  summarise(bach_nh = sum(bach_wgt),
            hs_nh = sum(hs_wgt))

##earnings
earn_data<-read.csv("./input data/JC Tracts/ACS_14_5YR_S2001_with_ann.csv",header=T, skip=1)
earn_data<-full_join(earn_data, tract_neigh, by = "Id2")

earn_data<-select(earn_data, Id2, Geography, Neighborhood, population = Total..Estimate..Population.16.years.and.over.with.earnings, 
                  earnings = Total..Estimate..Median.earnings..dollars.)
earn_data$num_earnings = as.numeric(as.character(earn_data$earnings))

n_earn_data<-earn_data %>%
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         earnings_wgt=num_earnings*wgt)%>%
  summarise(earnings_nh = sum(earnings_wgt))

##insurance
health_data<-read.csv("./input data/JC Tracts/ACS_14_5YR_S2701_with_ann.csv",header=T, skip=1)
health_data<-full_join(health_data, tract_neigh, by = "Id2")
health_data<-health_data %>% 
  select(Id2, Geography, Neighborhood, population = Total..Estimate..Total.civilian.noninstitutionalized.population, 
         uninsured = Percent.Uninsured..Estimate..Total.civilian.noninstitutionalized.population)

health_data$uninsured = as.numeric(as.character(health_data$uninsured))

n_health_data<- health_data %>%
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         uninsured_wgt = uninsured*wgt)%>%
  summarise(uninsured_nh = sum(uninsured_wgt),
            population = sum(population)) #this is the one data set where I keep population

##low income
low_inc_data<-read.csv("./input data/JC Tracts/ACS_14_5YR_C17002_with_ann.csv",header=T, skip=1)
low_inc_data<-full_join(low_inc_data, tract_neigh, by = "Id2")
low_inc_data<-low_inc_data %>%
  select(Id2, Geography, Neighborhood, population = Estimate..Total., 
         less_than_50 = Estimate..Total....Under..50,
         less_than_1 = Estimate..Total.....50.to..99,
         less_than_1.25 = Estimate..Total....1.00.to.1.24,
         less_than_1.5 = Estimate..Total....1.25.to.1.49)%>%
  mutate(under_150 = (less_than_50+less_than_1+less_than_1.25+less_than_1.5)/population)

n_low_inc_data <- low_inc_data %>%
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         low_inc_wgt = under_150*wgt)%>%
  summarise(low_inc_nh = sum(low_inc_wgt))

##unemployment
unemp_data<-read.csv("./input data/JC Tracts/ACS_14_5YR_S2301_with_ann.csv",header=T, skip=1)
unemp_data<-full_join(unemp_data, tract_neigh, by = "Id2")
unemp_data<- unemp_data %>% 
  select(Id2, Geography, Neighborhood, 
         population = Total..Estimate..Population.16.years.and.over,
         unemployed = Unemployment.rate..Estimate..Population.16.years.and.over)
unemp_data$unemployed = as.numeric(as.character(unemp_data$unemployed))

n_unemp_data <- unemp_data %>%
  group_by(Neighborhood) %>%
  mutate(wgt = population/sum(population),
         unemp_wgt = unemployed*wgt) %>%
  summarise(unemp_nh = sum(unemp_wgt))

##race
race_data<-read.csv("./input data/JC Tracts/ACS_14_5YR_B02001_with_ann.csv",header=T, skip=1)
race_data = full_join(race_data, tract_neigh, by = "Id2")
race_data = race_data %>%
  select(Id2, Geography, Neighborhood, population = Estimate..Total.,
         black_population = Estimate..Total....Black.or.African.American.alone)%>%
  mutate(pct_black = black_population/population)

n_race_data = race_data %>%
  group_by(Neighborhood)%>%
  mutate(wgt = population/sum(population),
         pct_black_wgt = pct_black*wgt)%>%
  summarise(pct_black_nh = sum(pct_black_wgt))

##child poverty
cp_data<-read.csv("./input data/JC Tracts/ACS_14_5YR_B17024_with_ann.csv",header=T, skip=1)
cp_data = full_join(cp_data, tract_neigh, by = "Id2")
cp_data = cp_data %>%
  mutate(li_6_pop = Estimate..Under.6.years....Under..50 + 
           Estimate..Under.6.years.....50.to..74 +
           Estimate..Under.6.years.....75.to..99 +
           Estimate..Under.6.years....1.00.to.1.24 +
           Estimate..Under.6.years....1.25.to.1.49,
         li_11_pop = Estimate..6.to.11.years....Under..50 +
           Estimate..6.to.11.years.....50.to..74 +
           Estimate..6.to.11.years.....75.to..99 +
           Estimate..6.to.11.years....1.00.to.1.24 +
           Estimate..6.to.11.years....1.25.to.1.49,
         li_17_pop = Estimate..12.to.17.years....Under..50 +
           Estimate..12.to.17.years.....50.to..74 +
           Estimate..12.to.17.years.....75.to..99 +
           Estimate..12.to.17.years....1.00.to.1.24 +
           Estimate..12.to.17.years....1.25.to.1.49,
         li_child_pop = li_6_pop + li_11_pop + li_17_pop,
         child_pop = Estimate..Under.6.years. + 
           Estimate..6.to.11.years. +
           Estimate..12.to.17.years.,
         child_li_rate = li_child_pop/child_pop) %>%
  select(Id2, Neighborhood, child_li_rate, child_pop)

n_cp_data = cp_data %>%
  group_by(Neighborhood)%>%
  mutate(wgt = child_pop/sum(child_pop),
         child_li_rate_wgt = child_li_rate*wgt)%>%
  summarise(child_li_nh = sum(child_li_rate_wgt))


##joining into one database
data = full_join(n_earn_data, n_ed_data, by = "Neighborhood")
data = full_join(data, n_health_data, by = "Neighborhood")
data = full_join(data, n_low_inc_data, by = "Neighborhood")
data = full_join(data, n_unemp_data, by = "Neighborhood")
data = full_join(data, n_race_data, by = "Neighborhood")
data = full_join(data, n_cp_data, by = "Neighborhood")

data = data %>%
  rename(low_income = low_inc_nh, unemployed = unemp_nh, no_hs_degree = hs_nh, 
         bachelors_degree = bach_nh, uninsured = uninsured_nh, 
         median_earnings = earnings_nh, percent_black = pct_black_nh)%>%
  mutate(low_income = low_income*100, bachelors_degree = bachelors_degree*100, 
         no_hs_degree = no_hs_degree*100, percent_black = percent_black*100,
         child_low_income = child_li_nh*100)

##using z-scores to identify MPI neighborhoods
NormZ<-function(x){
  (x-mean(x))/sd(x)
}

options(scipen = 999)

data = data %>%
  filter(Neighborhood != "Airport") %>%
  mutate(z_low_income = NormZ(low_income),
         z_unemployed = NormZ(unemployed),
         z_no_hs_degree = NormZ(no_hs_degree),
         z_uninsured = NormZ(uninsured),
         z_overall = (z_low_income+z_unemployed+z_no_hs_degree+z_uninsured)/4)%>%
  arrange(-z_overall)

#Life expectancy data comes from the Louisville Center for Health Equity.
#I transcribed it to a csv file from their report PDF
#This will not work with the alternate neighborhood areas, no life expectancy data is available
n_life_expectancy = read.csv(file = "./input data/neighborhood abbreviations and life expectancy.csv", header= T)
data = full_join(data, n_life_expectancy, by = "Neighborhood")

#writing out the data
write.csv(data, file = "./output data/Neighborhood Area MPI.csv")
#write.csv(data[1:20,], file = "./output data/Alternate Neighborhood Area MPI.csv")

#At this point, one can join with the neighborhood shapefile to map, or use as is
#The neighborhood data will be used again in Imagining a Better Louisville

#####################
##Census Tract Data##
#####################

tract_data = full_join(earn_data, ed_data, by = "Id2")
tract_data = full_join(tract_data, health_data, by = "Id2")
tract_data = full_join(tract_data, low_inc_data, by = "Id2")
tract_data = full_join(tract_data, unemp_data, by = "Id2")
tract_data = full_join(tract_data, race_data, by = "Id2")
tract_data = full_join(tract_data, cp_data, by = "Id2")
tract_data = full_join(tract_data, tract_neigh, by = "Id2") #not necessary, but useful to ID tracks

tract_data = tract_data %>%
  rename(low_income = under_150, no_hs_degree = less_than_hs_percent, 
         bachelors_degree = bach_percent, 
         median_earnings = num_earnings, percent_black = pct_black,
         children_low_income = child_li_rate)%>%
  mutate(low_income = low_income*100, bachelors_degree = bachelors_degree*100, 
         no_hs_degree = no_hs_degree*100, percent_black = percent_black*100,
         children_low_income = children_low_income*100)


tract_data = tract_data[1:190,] #I'm dropping the Airport tract. It's zeroes throw things off. 

tract_data = tract_data %>%
  mutate(z_low_income = NormZ(low_income),
         z_unemployed = NormZ(unemployed),
         z_no_hs_degree = NormZ(no_hs_degree),
         z_uninsured = NormZ(uninsured),
         z_overall = (z_low_income+z_unemployed+z_no_hs_degree+z_uninsured)/4)%>%
  arrange(-z_overall)
tract_data = tract_data %>%
  select(census_tract = Id, Neighborhood = Neighborhood.x, population = population.x.x, poverty_z_score = z_overall, no_hs_degree, low_income, children_low_income, bachelors_degree, percent_black, unemployed, uninsured, median_earnings)

write.csv(tract_data, file = "./output data/Tract Area MPI.csv")

#histogram of population by MPI score
color_hist = c("#D7191C", "#EA633E", "#FDAE61",
               "#FED791", "#FFFFC0", "#D3EC95",
               "#A6D96A", "#60B855", "#1A9641") #I chose these colors to match maps made in QGIS
p = ggplot(tract_data, aes(poverty_z_score*-1, weight = population)) + 
  geom_histogram(breaks = seq(-3.0, 1.5, by = .5), 
                 fill = I(color_hist),
                 col = I("black"))
p = p+theme_tufte()+labs(title = "Population by Poverty Index Value", x= "Poverty Index", y = "Population\n")
p

#getting the numbers from the histogram
pg<-ggplot_build(p)
hist_data <- pg$data[[1]]
hist_data$count

#graphing MPI and Race at the Tract Level
cor.test(tract_data$percent_black, tract_data$poverty_z_score) #.66, p-value = <.00001

p<-ggplot(tract_data, aes(x=percent_black,y=poverty_z_score))
p<-p+geom_smooth(method="lm",se=FALSE, color="blue", size=.5)
p<-p+geom_point()
p<-p+theme_bw()
p<-p+labs(title="Multidimensional Poverty and Race",x="Percent Black",
          y="Poverty Index")
p<-p+annotate("text", x=85, y = -.6, label = "correlation: 0.66")
p<-p+annotate("text", x=86, y = -.85, label = "p-value: <. 00001")
p

###########################################################
##Difference Between Poorest and Least Poor Neighborhoods##
###########################################################

##summing up the neighborhoods 
 
##poorest neighborhoods
mpi_nh = data %>%
  filter(z_overall > 1) %>%
  mutate(wgt = population/sum(population),
         earnings_wgt = median_earnings*wgt,
         bachelors_wgt = bachelors_degree*wgt,
         no_hs_degree_wgt = no_hs_degree*wgt,
         low_income_wgt = low_income*wgt,
         unemp_wgt = unemployed*wgt,
         uninsured_wgt = uninsured*wgt,
         le_wgt = life_expectancy*wgt,
         child_li_wgt = child_low_income*wgt)
poorest_four = c(sum(mpi_nh$low_income_wgt),
sum(mpi_nh$unemp_wgt),
sum(mpi_nh$uninsured_wgt),
sum(mpi_nh$no_hs_degree_wgt),
sum(mpi_nh$bachelors_wgt),
sum(mpi_nh$earnings_wgt),
sum(mpi_nh$le_wgt),
sum(mpi_nh$population),
sum(mpi_nh$child_li_wgt))

#Louisville
all_nh = data %>%
  mutate(wgt = population/sum(population),
         earnings_wgt = median_earnings*wgt,
         bachelors_wgt = bachelors_degree*wgt,
         no_hs_degree_wgt = no_hs_degree*wgt,
         low_income_wgt = low_income*wgt,
         unemp_wgt = unemployed*wgt,
         uninsured_wgt = uninsured*wgt,
         le_wgt = life_expectancy*wgt,
         child_li_wgt = child_low_income*wgt)
Louisville_column = c(avg_low_income = sum(all_nh$low_income_wgt),
avg_unemp = sum(all_nh$unemp_wgt),
avg_uninsured = sum(all_nh$uninsured_wgt),
avg_no_hs_degree = sum(all_nh$no_hs_degree_wgt),
avg_bachelors = sum(all_nh$bachelors_wgt),
avg_earnings = sum(all_nh$earnings_wgt),
avg_le = sum(all_nh$le_wgt),
avg_child_li = sum(all_nh$child_li_wgt),
sum(all_nh$population))
lville_child_pop = sum(cp_data$child_pop)

#Highest Neighborhoods
top_nh = data %>%
  filter(z_overall < -1) %>%
  mutate(wgt = population/sum(population),
         earnings_wgt = median_earnings*wgt,
         bachelors_wgt = bachelors_degree*wgt,
         no_hs_degree_wgt = no_hs_degree*wgt,
         low_income_wgt = low_income*wgt,
         unemp_wgt = unemployed*wgt,
         uninsured_wgt = uninsured*wgt,
         le_wgt = life_expectancy*wgt,
         child_li_wgt = child_low_income*wgt)
least_poor_four = c(sum(top_nh$low_income_wgt),
sum(top_nh$unemp_wgt),
sum(top_nh$uninsured_wgt),
sum(top_nh$no_hs_degree_wgt),
sum(top_nh$bachelors_wgt),
sum(top_nh$earnings_wgt),
sum(top_nh$le_wgt),
sum(top_nh$population),
sum(top_nh$child_li_wgt))

name_column = c("Low Income","Unemployed", "Uninsured", "No High School Diploma",
                "Bachelor's Degree", "Median Earnings", "Life Expectancy",
                "Population", "Low Income Children")

neighborhood_comparison_data = data.frame(cbind(name_column, poorest_four, 
                                                Louisville_column, least_poor_four))
names(neighborhood_comparison_data) = c("Indicator", "Poorest 4 Neighborhoods", 
                                        "Citywide Average", "Least Poor 4 Neighborhoods")
row.names(neighborhood_comparison_data) = NULL

write.csv(neighborhood_comparison_data, file = "./output data/Poorest and Least Poor Neighborhoods.csv")

##graphing MPI and Race
p<-ggplot(data, aes(x=percent_black,y=z_overall))
p<-p+geom_smooth(method="lm",se=FALSE, color="blue", size=.5)
p<-p+geom_text(aes(label=data$sc_abbr),fontface="plain", color="black")
#p<-p+geom_abline(intercept = 0, slope =1, color ="black", linetype = "dotted")
p<-p+theme_bw()
p<-p+labs(title="Multidimensional Poverty and Race",x="Percent Black",
          y="Poverty Index")
p<-p+annotate("text", x=83.5, y = -.6, label = "correlation: .75")
p<-p+annotate("text", x=83.9, y = -.85, label = "p-value: <.0001")
p

cor.test(data$percent_black, data$z_overall) #.74, p-value = <.0001

##testing gini coefficients and drawing Lorenz curves
library(ineq)
ineq(race_data$pct_black, type = "Gini")
ineq(race_data$black_population, type = "Gini")

l_curve = Lc(race_data$black_population)
l_curve_df = data.frame(l_curve$p, l_curve$L)

plot(Lc(race_data$black_population), col="darkred", lwd = 2,
     main = "Lorenz Curve of Racial Segregation", xlab = "Percent of Census Tracts",
     ylab = "Percent of Total Black Population")


#################################
##Imagining a Better Lousiville##
#################################

##calculating citywide averages
avg_low_income = sum(all_nh$low_income_wgt)
avg_unemp = sum(all_nh$unemp_wgt)
avg_uninsured = sum(all_nh$uninsured_wgt)
avg_no_hs_degree = sum(all_nh$no_hs_degree_wgt)
avg_bachelors = sum(all_nh$bachelors_wgt)
avg_earnings = sum(all_nh$earnings_wgt)
avg_le = sum(all_nh$le_wgt)
avg_child_li = sum(all_nh$child_li_wgt)

##replacing bottom 4 (over 1 on MPI index) with citywide average
data$th_earnings = data$median_earnings
data$th_earnings[data$z_overall > 1] = avg_earnings
data$th_uninsured = data$uninsured
data$th_uninsured[data$z_overall >1] = avg_uninsured
data$th_bach = data$bachelors_degree
data$th_bach[data$z_overall >1] = avg_bachelors
data$th_le = data$life_expectancy
data$th_le[data$z_overall >1] = avg_le
data$th_low_income = data$low_income
data$th_low_income[data$z_overall >1] = avg_low_income
data$th_unemp = data$unemployed
data$th_unemp[data$z_overall >1] = avg_unemp
data$th_no_hs = data$no_hs_degree
data$th_no_hs[data$z_overall >1] = avg_no_hs_degree
data$th_child_low_income = data$child_low_income
data$th_child_low_income[data$z_overall > 1] = avg_child_li

thought_data = data %>%
  mutate(wgt = population/sum(population),
         earnings_wgt = th_earnings*wgt,
         bachelors_wgt = th_bach*wgt,
         uninsured_wgt = th_uninsured*wgt,
         life_expectancy_wgt = th_le*wgt,
         low_income_wgt = th_low_income*wgt,
         unemp_wgt = th_unemp*wgt,
         no_hs_wgt = th_no_hs*wgt,
         child_li_wgt = th_child_low_income*wgt)

th_avg_earnings = sum(thought_data$earnings_wgt)
th_avg_bachelors = sum(thought_data$bachelors_wgt)
th_avg_uninsured = sum(thought_data$uninsured_wgt)
th_avg_life_expectancy = sum(thought_data$life_expectancy_wgt)
th_avg_low_income = sum(thought_data$low_income_wgt)
th_avg_unemp = sum(thought_data$unemp_wgt)
th_avg_no_hs = sum(thought_data$no_hs_wgt)
th_avg_child_li = sum(thought_data$child_li_wgt)

current_column = c(avg_earnings, avg_bachelors, avg_uninsured, avg_le,
                   avg_low_income, avg_unemp, avg_no_hs_degree, avg_child_li)
thought_column = c(th_avg_earnings, th_avg_bachelors, th_avg_uninsured, th_avg_life_expectancy,
                   th_avg_low_income, th_avg_unemp, th_avg_no_hs, th_avg_child_li)
te_names_column = c("Median Earnings", "Bachelor's Degrees", "Uninsured", "Life Expectancy",
                    "Low Income", "Unemployment", "No High School Diploma", "Low Income Children")

extra_bach = sum(ed_data$population)*((th_avg_bachelors - avg_bachelors)/100)
extra_earnings_pc = th_avg_earnings - avg_earnings
extra_earnings = extra_earnings_pc*(sum(earn_data$population))
extra_insured = sum(data$population)*((avg_uninsured-th_avg_uninsured)/100)
extra_life = sum(data$population)*(th_avg_life_expectancy-avg_le)
extra_income = sum(data$population)*((avg_low_income - th_avg_low_income)/100)
extra_emp = sum(unemp_data$population)*((avg_unemp-th_avg_unemp)/100)
extra_hs = sum(ed_data$population)*((avg_no_hs_degree - th_avg_no_hs)/100)
fewer_child_li = ((avg_child_li - th_avg_child_li)/100)*lville_child_pop

benefits_column = c(extra_earnings_pc, extra_bach, extra_insured, extra_life, extra_income,
                    extra_emp, extra_hs, fewer_child_li)

thought_exp_output = data.frame(cbind(te_names_column, current_column, thought_column, benefits_column))
names(thought_exp_output) = c("Indicator", "Current Citywide", "Possible Citywide", "Total Benefits")

write.csv(thought_exp_output, file = "./output data/Thought Experiment Results.csv")


########################
##Concentration of MPI##
########################


pc_ed_data<-read.csv("input data/Peer City Tracts/ACS_14_5YR_B23006_with_ann.csv",header=T, skip=1)
pc_ed_data<-select(pc_ed_data, Id2, Geography, population = Estimate..Total., 
                less_than_hs = Estimate..Less.than.high.school.graduate.,
                bachelors_up = Estimate..Bachelor.s.degree.or.higher.)
pc_ed_data<-mutate(pc_ed_data, bach_percent = bachelors_up/population,
                less_than_hs_percent = less_than_hs/population)


pc_earn_data<-read.csv("input data/Peer City Tracts/ACS_14_5YR_S2001_with_ann.csv",header=T, skip=1)
pc_earn_data<-select(pc_earn_data, Id2, Geography, population = Total..Estimate..Population.16.years.and.over.with.earnings, 
                  earnings = Total..Estimate..Median.earnings..dollars.)
pc_earn_data$num_earnings = as.numeric(as.character(pc_earn_data$earnings))

pc_health_data<-read.csv("input data/Peer City Tracts/ACS_14_5YR_S2701_with_ann.csv",header=T, skip=1)
pc_health_data<-pc_health_data %>% 
  select(Id2, Geography, population = Total..Estimate..Total.civilian.noninstitutionalized.population, 
         uninsured = Percent.Uninsured..Estimate..Total.civilian.noninstitutionalized.population)
pc_health_data$uninsured = as.numeric(as.character(pc_health_data$uninsured))

pc_low_inc_data<-read.csv("input data/Peer City Tracts/ACS_14_5YR_C17002_with_ann.csv",header=T, skip=1)
pc_low_inc_data<-pc_low_inc_data %>%
  select(Id2, Geography, population = Estimate..Total., 
         less_than_50 = Estimate..Total....Under..50,
         less_than_1 = Estimate..Total.....50.to..99,
         less_than_1.25 = Estimate..Total....1.00.to.1.24,
         less_than_1.5 = Estimate..Total....1.25.to.1.49)%>%
  mutate(under_150 = (less_than_50+less_than_1+less_than_1.25+less_than_1.5)/population)

pc_unemp_data<-read.csv("input data/Peer City Tracts/ACS_14_5YR_S2301_with_ann.csv",header=T, skip=1)
pc_unemp_data<- pc_unemp_data %>% 
  select(Id2, Geography,  
         population = Total..Estimate..Population.16.years.and.over,
         unemployed = Unemployment.rate..Estimate..Population.16.years.and.over)
pc_unemp_data$unemployed = as.numeric(as.character(pc_unemp_data$unemployed))

pc_race_data<-read.csv("input data/Peer City Tracts/ACS_14_5YR_B02001_with_ann.csv",header=T, skip=1)
pc_race_data = pc_race_data %>%
  select(Id, Id2, Geography, population = Estimate..Total.,
         black_population = Estimate..Total....Black.or.African.American.alone)%>%
  mutate(pct_black = black_population/population)

pc_data = full_join(pc_earn_data, pc_ed_data, by = "Id2")
pc_data = full_join(pc_data, pc_health_data, by = "Id2")
pc_data = full_join(pc_data, pc_low_inc_data, by = "Id2")
pc_data = full_join(pc_data, pc_unemp_data, by = "Id2")
pc_data = full_join(pc_data, pc_race_data, by = "Id2")

pc_data = pc_data %>%
  rename(low_income = under_150, no_hs_degree = less_than_hs_percent, 
         bachelors_degree = bach_percent, 
         median_earnings = num_earnings, percent_black = pct_black)%>%
  mutate(low_income = low_income*100, bachelors_degree = bachelors_degree*100, 
         no_hs_degree = no_hs_degree*100, percent_black = percent_black*100)

##creating FIPS codes by taking only first 5 digits of tract codes
FIPS = substr(pc_data$Id2, 1,5)
FIPS = as.numeric(FIPS)
pc_data = cbind(pc_data, FIPS)
pc_data$FIPS[pc_data$FIPS == 10730] <- 1073
pc_data$FIPS[pc_data$FIPS == 29189] <- 0 #setting both St. Louis county and city to FIPS = 0
pc_data$FIPS[pc_data$FIPS == 29510] <- 0 
pc_data = subset(pc_data, population.x != 0)
pc_data = subset(pc_data, population.x.x != 0)
pc_data = subset(pc_data, population.y.y !=0)

pc_mpi_data = pc_data %>%
  group_by(FIPS)%>%
  mutate(z_low_income = NormZ(low_income),
         z_unemployed = NormZ(unemployed),
         z_no_hs_degree = NormZ(no_hs_degree),
         z_uninsured = NormZ(uninsured),
         z_overall = (z_low_income+z_unemployed+z_no_hs_degree+z_uninsured)/4)

##only the data needed for next part
mpi_data = pc_mpi_data %>%
  select(Id2, FIPS, mpi = z_overall, percent_black, population = population.x.x, black_population)

##IDing mpi poor neighborhoods and extreme mpi poor neighborhoods
mpi_data$mpi_nh = NA
mpi_data$mpi_nh = "no"
mpi_data$mpi_nh[mpi_data$mpi > 1] = "yes"
mpi_data$mpi_nh = as.factor(mpi_data$mpi_nh)
mpi_data$ex_mpi_nh = NA
mpi_data$ex_mpi_nh = "no"
mpi_data$ex_mpi_nh[mpi_data$mpi > 2.5] = "yes"
mpi_data$ex_mpi_nh = as.factor(mpi_data$ex_mpi_nh)

##grouping by cities
mpi_city = mpi_data %>%
  group_by(FIPS) %>%
  dplyr::summarize(pop_mpi = sum(population[mpi_nh == "yes"]),
                   pop_ex_mpi = sum(population[ex_mpi_nh == "yes"]),
                   population = sum(population),
                   racial_correlation = cor(mpi, percent_black),
                   segregation = ineq(black_population, type = "Gini")
  ) 

mpi_city = mpi_city %>%
  mutate(percent_in_mpi = pop_mpi/population,
         percent_in_ex_mpi = pop_ex_mpi/population) %>%
  arrange(percent_in_mpi)

##mergine with city IDs
name_data = read.csv("input data/GLP basic county ids.csv",header=TRUE)
name_data = name_data %>% filter(year == 2014 & Current.Peer == 1 & FIPS !=29189 & FIPS !=29510)
mpi_graph = full_join(mpi_city, name_data, by = "FIPS")

#a function used to make rankings graphs with natural breaks
rank_and_nb_group<-function(df, var, order="Descending",
                            plot_title="",y_title="", round_length = 1){
  df$var <- df[[var]]
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$City)
  d.graph<-cbind(d.rank,names)
  
  breaks<-classIntervals(d.graph$var,3,style="jenks")
  d.graph$color<-NA
  d.graph$color[d.graph$var<=breaks$brks[2]]<-"green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]]<-"yellow"
  d.graph$color[d.graph$var>breaks$brks[3]]<-"red"
  d.graph$round<-format(round(d.graph$var,round_length),nsmall=round_length)
  d.graph$textfont<-"plain"
  d.graph$textfont[d.graph$City=="Louisville"]<-"bold"
  d.graph$linecolor<-"white"
  d.graph$linecolor[d.graph$City=="Louisville"]<-"black"
  
  p<-ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                             y=var,fill=factor(color)))+guides(fill=FALSE)
  p<-p+geom_bar(stat="identity",color=rev(d.graph$linecolor))+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p<-p+scale_fill_manual(values=c("green3","red2","yellow2"))
  }
  if(order=="Descending"){
    p<-p+scale_fill_manual(values=c("red2","green3","yellow2"))
  }
  p<-p+theme(axis.text.y=element_text(hjust=0,face=rev(d.graph$textfont),
                                      size=12))
  p<-p+theme(axis.ticks=element_blank(),axis.text.x=element_blank())
  p<-p+geom_text(aes(label=round),hjust=1.1,size=5,fontface="bold")
  p<-p+labs(title=plot_title,x="",
            y=y_title)
  p<-p+theme(plot.title=element_text(color="black",size=18,face="bold",hjust=.5,
                                     margin=margin(b=10,unit="pt")))
  p
}


mpi_graph$mpi_nh = mpi_graph$percent_in_mpi*100
mpi_nh_plot<-rank_and_nb_group(mpi_graph, "mpi_nh", plot_title = "Population in Multi-Dimensionally Poor Areas", y_title = "Percent", order = "Ascending")
mpi_nh_plot

mpi_graph$ex_mpi_nh = mpi_graph$percent_in_ex_mpi*100
ex_mpi_nh_plot<-rank_and_nb_group(mpi_graph, "ex_mpi_nh", plot_title = "Population in Extremely Multi-Dimensionally Poor Areas", y_title = "Percent", order = "Ascending")
ex_mpi_nh_plot+theme(plot.title  = element_text(size = 12))

race_cor_plot<-rank_and_nb_group(mpi_graph, "racial_correlation", plot_title = "Correlation between Percent Black and MPI", y_title = "", order = "Ascending", round_length = 2)
race_cor_plot

segregation_plot<-rank_and_nb_group(mpi_graph, "segregation", plot_title = "Segregation of Black Residents", y_title = "", order = "Ascending", round_length = 2)
segregation_plot

##facet plot showing distribution in all peer cities
pc_mpi_data$city = as.factor(mpi_data$FIPS)
pc_mpi_data$city = recode_factor(pc_mpi_data$city, 
                              '47037' = "1. Nashville",
                              '47157' = "2. Memphis",
                              '39049' = "3. Columbus",
                              '26081' = "4. Grand Rapids",
                              '0' = "5. St. Louis",
                              '47093' = "6. Knoxville",
                              '39061' = "7. Cincinatti",
                              '40109' = "8. Oklahoma City",
                              '29095' = "9. Kansas City",
                              '1073' = "10. Birmingham",
                              '18097' = "11. Indianapolis",
                              '45045' = "12. Greenville",
                              '37081' = "13. Greensboro",
                              '40143' = "14. Tulsa",
                              '21111' = "15. Louisivlle",
                              '37119' = "16. Charlotte",
                              '31055' = "17. Omaha")

mpi_data = pc_mpi_data %>% 
  group_by(FIPS) %>%
  mutate(weight = population.x.x/sum(population.x.x))

color_hist = c("#D7191C", "#EA633E", "#FDAE61",
               "#FED791", "#FFFFC0", "#D3EC95",
               "#A6D96A", "#60B855", "#1A9641")
facet_color = rep(color_hist, 17)

##population by poverty index value
p = ggplot(mpi_data, aes(z_overall*-1, weight = population.x.x)) + 
  geom_histogram(breaks = seq(-3.0, 1.5, by = .5), 
                 fill = I(facet_color),
                 col = I("black"))
p = p+labs(title = "Population by Poverty Index Value", x= "Poverty Index", y = "Population\n")
facet_plot = p+ facet_wrap(~city, ncol=6)
facet_plot


##percent instead of overall population
p = ggplot(mpi_data, aes(z_overall*-1, weight = weight*100)) + 
  geom_histogram(breaks = seq(-3.0, 1.5, by = .5), 
                 fill = I(facet_color),
                 col = I("black"))
p = p+labs(title = "Percent of Population by Poverty Index Value", x= "Poverty Index", y = "Percent\n")
facet_plot = p+ facet_wrap(~city, ncol=6)
facet_plot



###############################################
##Peer City Rankings from a Better Louisville##
###############################################
earnings_d<-read.csv("./input data/City Level Data/ACS_14_5YR_S2001_with_ann_county.csv",header=TRUE,skip=1)
insurance_d<-read.csv("./input data/City Level Data/ACS_14_5YR_S2701_with_ann_county.csv",header=TRUE,skip=1)
educ_d<-read.csv("./input data/City Level Data/ACS_14_5YR_B23006_with_ann_county.csv",header=TRUE,skip=1)
low_inc_d<-read.csv("./input data/City Level Data/ACS_14_5YR_C17002_with_ann.csv",header=T, skip=1)
unemp_d<-read.csv("./input data/City Level Data/ACS_14_5YR_S2301_with_ann.csv",header=T, skip=1)
child_li_d<-read.csv("./input data/City Level Data/ACS_14_5YR_B17024_with_ann.csv",header=T, skip=1)

earnings_d = rename(earnings_d, FIPS = Id2)
insurance_d = rename(insurance_d, FIPS = Id2)
educ_d = rename(educ_d, FIPS = Id2)
low_inc_d = rename(low_inc_d, FIPS = Id2)
unemp_d = rename(unemp_d, FIPS = Id2)
child_li_d = rename(child_li_d, FIPS = Id2)

pull_peers_FIPS<-function(data){
  all.peers<-subset(data, data$FIPS == 1073 | data$FIPS == 37119
                    |data$FIPS == 39061 | data$FIPS == 39049
                    |data$FIPS == 26081 |data$FIPS == 37081
                    |data$FIPS == 45045 |data$FIPS == 18097
                    |data$FIPS == 29095 |data$FIPS == 47093
                    |data$FIPS == 21111 |data$FIPS == 47157
                    |data$FIPS == 47037 |data$FIPS == 40109
                    |data$FIPS == 31055 |data$FIPS == 29189
                    |data$FIPS == 29510 |data$FIPS == 40143
                    |data$FIPS == 12031 |data$FIPS == 37183
                    |data$FIPS == 39113 |data$FIPS == 51760)
  all.peers$baseline<-1
  all.peers$current<-1
  all.peers$baseline[all.peers$FIPS==26081|all.peers$FIPS==29189
                     |all.peers$FIPS==29510|all.peers$FIPS==40109
                     |all.peers$FIPS==40143|all.peers$FIPS==45045
                     |all.peers$FIPS==47093]<-0
  all.peers$current[all.peers$FIPS== 12031|all.peers$FIPS==37183|
                      all.peers$FIPS==39113|all.peers$FIPS==51760]<-0
  all.peers
}

earnings_d = pull_peers_FIPS(earnings_d)
insurance_d = pull_peers_FIPS(insurance_d)
educ_d = pull_peers_FIPS(educ_d)
low_inc_d = pull_peers_FIPS(low_inc_d)
unemp_d = pull_peers_FIPS(unemp_d)
child_li_d = pull_peers_FIPS(child_li_d)



earnings_d = earnings_d %>% 
  filter(current == 1) %>% 
  select(FIPS, earnings = Total..Estimate..Median.earnings..dollars.)

insurance_d = insurance_d %>% 
  filter(current == 1) %>%
  select(FIPS, uninsured = Percent.Uninsured..Estimate..Total.civilian.noninstitutionalized.population)

educ_d = educ_d %>% 
  filter(current ==1) %>%
  select(FIPS, bachelors_up = Estimate..Bachelor.s.degree.or.higher.,
         no_hs = Estimate..Less.than.high.school.graduate.,
         population = Estimate..Total.) %>%
  mutate(bach_percent = bachelors_up/population,
         no_hs_percent = no_hs/population) %>%
  select(FIPS, bach_percent, no_hs_percent)

low_inc_d<-low_inc_d %>%
  filter(current ==1) %>%
  select(FIPS, Geography, population = Estimate..Total., 
         less_than_50 = Estimate..Total....Under..50,
         less_than_1 = Estimate..Total.....50.to..99,
         less_than_1.25 = Estimate..Total....1.00.to.1.24,
         less_than_1.5 = Estimate..Total....1.25.to.1.49)%>%
  mutate(under_150 = (less_than_50+less_than_1+less_than_1.25+less_than_1.5)/population)%>%
  select(FIPS, under_150)

unemp_d<- unemp_d %>% 
  filter(current == 1)%>%
  select(FIPS,
         unemployed = Unemployment.rate..Estimate..Population.16.years.and.over)
unemp_d$unemployed = as.numeric(as.character(unemp_d$unemployed))

child_li_d = child_li_d %>%
  filter(current == 1) %>%
  mutate(li_6_pop = Estimate..Under.6.years....Under..50 + 
           Estimate..Under.6.years.....50.to..74 +
           Estimate..Under.6.years.....75.to..99 +
           Estimate..Under.6.years....1.00.to.1.24 +
           Estimate..Under.6.years....1.25.to.1.49,
         li_11_pop = Estimate..6.to.11.years....Under..50 +
           Estimate..6.to.11.years.....50.to..74 +
           Estimate..6.to.11.years.....75.to..99 +
           Estimate..6.to.11.years....1.00.to.1.24 +
           Estimate..6.to.11.years....1.25.to.1.49,
         li_17_pop = Estimate..12.to.17.years....Under..50 +
           Estimate..12.to.17.years.....50.to..74 +
           Estimate..12.to.17.years.....75.to..99 +
           Estimate..12.to.17.years....1.00.to.1.24 +
           Estimate..12.to.17.years....1.25.to.1.49,
         li_child_pop = li_6_pop + li_11_pop + li_17_pop,
         child_pop = Estimate..Under.6.years. + 
           Estimate..6.to.11.years. +
           Estimate..12.to.17.years.,
         child_li_rate = li_child_pop/child_pop) %>%
  select(FIPS, child_li_rate)


##weight St. Louis city and county into one
earnings_value = earnings_d$earnings[earnings_d$FIPS == 29189]*.758 + earnings_d$earnings[earnings_d$FIPS == 29510]*.242
earnings_row = c("0", earnings_value)
earnings_d = rbind(earnings_d, earnings_row)
earnings_d = earnings_d %>% filter(FIPS != 29189 & FIPS != 29510)

insurance_value = insurance_d$uninsured[insurance_d$FIPS == 29189]*.758 + insurance_d$uninsured[insurance_d$FIPS == 29510]*.242
insurance_row = c("0", insurance_value)
insurance_d = rbind(insurance_d, insurance_row)
insurance_d = insurance_d %>% filter(FIPS != 29189 & FIPS != 29510)

ed_value = educ_d$bach_percent[educ_d$FIPS == 29189]*.758 + educ_d$bach_percent[educ_d$FIPS == 29510]*.242
ed_value_hs = educ_d$no_hs_percent[educ_d$FIPS == 29189]*.758 + educ_d$no_hs_percent[educ_d$FIPS == 29510]*.242
educ_row = c("0", ed_value, ed_value_hs)
educ_d = rbind(educ_d, educ_row)
educ_d = educ_d %>% filter(FIPS !=29189 & FIPS != 29510)

low_inc_value = low_inc_d$under_150[low_inc_d$FIPS == 29189]*.758 + low_inc_d$under_150[low_inc_d$FIPS == 29510]*.242
low_inc_row = c("0", low_inc_value)
low_inc_d = rbind(low_inc_d, low_inc_row)
low_inc_d = low_inc_d %>% filter(FIPS != 29189 & FIPS != 29510)

unemp_value = unemp_d$unemployed[unemp_d$FIPS == 29189]*.758 + unemp_d$unemployed[unemp_d$FIPS == 29510]*.242
unemp_row = c("0", unemp_value)
unemp_d = rbind(unemp_d, unemp_row)
unemp_d = unemp_d %>% filter(FIPS != 29189 & FIPS != 29510)

child_li_value = child_li_d$child_li_rate[child_li_d$FIPS == 29189]*.758 + child_li_d$child_li_rate[child_li_d$FIPS == 29510]*.242
child_row = c("0", child_li_value)
child_li_d = rbind(child_li_d, child_row)
child_li_d = child_li_d %>% filter(FIPS != 29189 & FIPS != 29510)


educ_d = educ_d %>% arrange(no_hs_percent)
low_inc_d = low_inc_d %>% arrange(under_150)
unemp_d$unemployed = as.numeric(unemp_d$unemployed)
unemp_d = unemp_d %>% arrange(unemployed)

name_data = read.csv("./input data/GLP basic county ids.csv",header=TRUE)
name_data = name_data %>% filter(year == 2014 & Current.Peer == 1 & FIPS !=29189 & FIPS !=29510)
name_data$FIPS = as.character(name_data$FIPS)

earnings_d = full_join(earnings_d, name_data, by = "FIPS")
educ_d = full_join(educ_d, name_data, by = "FIPS")
insurance_d = full_join(insurance_d, name_data, by = "FIPS")
unemp_d = full_join(unemp_d, name_data, by = "FIPS")
low_inc_d = full_join(low_inc_d, name_data, by = "FIPS")
child_li_d = full_join(child_li_d, name_data, by = "FIPS")

##Graphing function
rank_and_nb_group<-function(df, var, order="Descending",
                            plot_title="",y_title="", round_length = 1){
  df$var <- df[[var]]
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$City)
  d.graph<-cbind(d.rank,names)
  
  breaks<-classIntervals(d.graph$var,3,style="jenks")
  d.graph$color<-NA
  d.graph$color[d.graph$var<=breaks$brks[2]]<-"green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]]<-"yellow"
  d.graph$color[d.graph$var>breaks$brks[3]]<-"red"
  d.graph$round<-format(round(d.graph$var,round_length),nsmall=round_length)
  d.graph$textfont<-"plain"
  d.graph$textfont[d.graph$City=="Louisville"]<-"bold"
  d.graph$linecolor<-"white"
  d.graph$linecolor[d.graph$City=="Louisville"]<-"black"
  
  p<-ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                             y=var,fill=factor(color)))+guides(fill=FALSE)
  p<-p+geom_bar(stat="identity",color=rev(d.graph$linecolor))+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p<-p+scale_fill_manual(values=c("green3","red2","yellow2"))
  }
  if(order=="Descending"){
    p<-p+scale_fill_manual(values=c("red2","green3","yellow2"))
  }
  p<-p+theme(axis.text.y=element_text(hjust=0,face=rev(d.graph$textfont),
                                      size=12))
  p<-p+theme(axis.ticks=element_blank(),axis.text.x=element_blank())
  p<-p+geom_text(aes(label=round),hjust=1.1,size=5,fontface="bold")
  p<-p+labs(title=plot_title,x="",
            y=y_title)
  p<-p+theme(plot.title=element_text(color="black",size=18,face="bold",hjust=.5,
                                     margin=margin(b=10,unit="pt")))
  p
}

educ_d$bach_percent = as.numeric(educ_d$bach_percent)*100
educ_plot<-rank_and_nb_group(educ_d, "bach_percent", plot_title = "Working Age Population with a Bachelor's Degree", y_title = "Percent")
educ_plot

educ_d$no_hs_percent = as.numeric(educ_d$no_hs_percent)*100
hs_plot = rank_and_nb_group(educ_d, "no_hs_percent", order = "Ascending", plot_title = "Percent Without a High School Degree")
hs_plot

insurance_d$uninsured = as.numeric(insurance_d$uninsured)
insurance_plot<-rank_and_nb_group(insurance_d, "uninsured", order = "Ascending", plot_title = "Uninsured", y_title = "Percent")
insurance_plot

earnings_d$earnings = as.numeric(earnings_d$earnings)
earnings_plot<-rank_and_nb_group(earnings_d, "earnings", plot_title = "Median Earnings", y_title = "Dollars Per Year", round_length=0)
earnings_plot

low_inc_d$under_150 = as.numeric(low_inc_d$under_150)*100
low_inc_plot = rank_and_nb_group(low_inc_d, "under_150", order = "Ascending",plot_title = "Low Income Population", y_title = "Percent")
low_inc_plot

unemp_d$unemployed = as.numeric(unemp_d$unemployed)
unemp_plot = rank_and_nb_group(unemp_d, "unemployed", order = "Ascending", plot_title = "Unemployment Rate", y_title = "Percent")
unemp_plot

child_li_d$child_li_rate = as.numeric(child_li_d$child_li_rate)*100
child_li_plot = rank_and_nb_group(child_li_d, "child_li_rate", order = "Ascending", plot_title = "Low Income Children", y_title = "Percent")
child_li_plot

##Thought Experimenting
educ_d_t = educ_d
earnings_d_t = earnings_d
insurance_d_t = insurance_d
low_inc_d_t = low_inc_d
unemp_d_t = unemp_d
child_li_d_t = child_li_d


educ_d_t$bach_percent[educ_d_t$FIPS == 21111] = th_avg_bachelors
educ_d_t$no_hs_percent[educ_d_t$FIPS == 21111] = th_avg_no_hs
earnings_d_t$earnings[earnings_d_t$FIPS == 21111] = th_avg_earnings
insurance_d_t$uninsured[earnings_d_t$FIPS == 21111] = th_avg_uninsured
low_inc_d_t$under_150[low_inc_d_t$FIPS == 21111] = th_avg_low_income
unemp_d_t$unemployed[unemp_d_t$FIPS == 21111] = th_avg_unemp
child_li_d_t$child_li_rate[child_li_d_t$FIPS == 21111] = th_avg_child_li

##Graphing the thought experiment
educ_plot_t<-rank_and_nb_group(educ_d_t, "bach_percent", plot_title = "Possible Working Age Population with a Bachelor's Degree", y_title = "Percent")
educ_plot_t

hs_plot_t = rank_and_nb_group(educ_d_t, "no_hs_percent", order = "Ascending", plot_title = "Possible Percent Without a High School Degree")
hs_plot_t

insurance_plot_t<-rank_and_nb_group(insurance_d_t, "uninsured", order = "Ascending", plot_title = "Possible Uninsured", y_title = "Percent")
insurance_plot_t

earnings_plot_t<-rank_and_nb_group(earnings_d_t, "earnings", plot_title = "Possible Median Earnings", y_title = "Dollars Per Year", round_length=0)
earnings_plot_t

low_inc_plot_t = rank_and_nb_group(low_inc_d_t, "under_150", order = "Ascending",plot_title = "Possible Low Income Population", y_title = "Percent")
low_inc_plot_t

unemp_plot_t = rank_and_nb_group(unemp_d_t, "unemployed", order = "Ascending", plot_title = "Possible Unemployment Rate", y_title = "Percent")
unemp_plot_t

child_li_plot_t = rank_and_nb_group(child_li_d_t, "child_li_rate", order = "Ascending", plot_title = "Possible Low Income Children", y_title = "Percent")
child_li_plot_t
