################################################################################
########### Code to Create a Shiny App of Bellevue, WA Demographics ############
################################################################################

# 1st clear the working memory, reset R's memory if desired Ctrl+Shift+F10, check
# to see if the required packages exist locally & load if not, then load the 
#libraries needed, & set a few global options 
# directory.

# List of required package names
packages <- c("tidyverse","sf","tigris","tidycensus","plotly","readxl","scales",
              "survey","srvyr","segregation")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

rm(list = ls())         # For clearing all objects from working memory
library(tidyverse)      # For because it's awesome
library(sf)             # For loading/manipulating shapefiles
library(tigris)         # For downloading Census TIGER shapefiles/erasing water
library(tidycensus)     # For downloading/working with ACS data via API
library(plotly)         # For interactive figures
library(readxl)         # For loading in Excel workbooks
library(scales)         # For formatting numbers/data labels
library(survey)         # For working with survey data
library(srvyr)          # For using tidy commands with survey data
library(segregation)    # For computing measures of geographic dissimilarity

options(scipen = 100)   # To turn off scientific notation
options(timeout = max(1000, getOption("timeout")))  # Increase R timeout time

# Set the working directory to somewhere local on your machine where you'd like
# store created files/environment to use as a part of the app.
directory <- paste0("YOUR DIRECTORY HERE")
dir.create(directory)
setwd(directory)
rm(directory)



# Set a global for the current ACS survey year. Update/double check each data 
# pull in periods when the 5-year & 1-year data are different vintages.
syear <- 2023

# Load in all of the available variables
acs1yr <- load_variables(syear,"acs1")
acs1sub <- load_variables(syear,"acs1/subject")
acs1pro <- load_variables(syear,"acs1/profile")  
decvars <- load_variables(2000,"sf3")
acs5yr <- load_variables(2022,"acs5")
acs5sub <- load_variables(2022,"acs5/subject")
acs5pro <- load_variables(2022,"acs5/profile")  



# Hexcodes compliant with the City of Bellevue's color style guide
hexcodes <- c("#006598","#C16623","#3B6D64","#4B9DA5","#660C53","#4E842A",
              "#164356","#B19102","#1C0E70","#9E1B18")










################################################################################
######### First Step: Prepare Each of the Plots to Include in the App ##########
################################################################################

# Download Bellevue population data from the WA Office of Financial Management 
# for each year from the present back through 1968. The raw OFM file has some 
# extraneous columns & weird formatting. The code below cleans the data up a 
# little bit and drop all of the non-Bvue area data.

# Save the url where the April 1 historical population data are on the web & 
# create a tempfile to store the data at.
url <- paste0("https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/",
              "ofm_april1_postcensal_estimates_pop_1960-present.xlsx")
tempdat <- tempfile()
download.file(url, tempdat, mode="wb")

# Import the data from the tempfile & clean them up a little bit
ofmpop1960<-read_excel(path=tempdat,sheet="Population",skip=3) %>%
  rename("1970 Census Count of Total Population"=`\r\n1970 \r\nCensus Count of Total Population`,
         "1980 Census Count of Total Population"=`\r\n1980 \r\nCensus Count of Total Population`,
         "1990 Census Count of Total Population"=`\r\n1990 \r\nCensus Count of Total Population`,
         "2000 Census Count of Total Population"=`\r\n2000 \r\nCensus Count of Total Population`,
         "2010 Census Count of Total Population"=`\r\n2010 \r\nCensus Count of Total Population`) %>%
  filter(Jurisdiction=="Bellevue") %>% 
  select_if(~ !any(is.na(.))) %>%
  select("County":"Jurisdiction",
         "1968 Postcensal Estimate of Total Population":"1979 Postcensal Estimate of Total Population",
         "1980 Census Count of Total Population":"1989 Postcensal Estimate of Total Population",
         "1990 Census Count of Total Population":"1999 Postcensal Estimate of Total Population",
         "2000 Census Count of Total Population":"2009 Postcensal Estimate of Total Population",
         "2010 Census Count of Total Population":"2024 Postcensal Estimate of Total Population") %>%
  pivot_longer(cols="1968 Postcensal Estimate of Total Population":"2024 Postcensal Estimate of Total Population",
               names_to="year",values_to="population") %>%
  filter(!population %in% c("*","$")) %>%
  separate(year,into=c("year","program"),sep= " ") %>%
  mutate("year"= as.double(year),
         "population"= as.double(population)) %>%
  mutate("population2"=case_when(year %in% c(1968,1970,1980,1990,2000,2010,2024)~
                                   population,
                                 TRUE~NA))

# Now plot the data & convert to an interactive plotly figure.
popplot <- ggplot(data=ofmpop1960,
                  aes(x=`year`,y=`population`,group=1,
                      text=paste("Estimate:",scales::comma(`population`)))) +
  geom_line(lwd=2,color="#006598") + 
  geom_point(aes(x=`year`,y=`population2`),color="#C16623",size=5) +
  geom_text(aes(x=`year`,y=`population2`),label=comma(ofmpop1960$`population2`),
            nudge_x=2.5,nudge_y=-5000,color="#006598") +
  theme_minimal() + 
  theme(legend.title=element_blank()) + 
  labs(x="",y="",title="") +
  scale_y_continuous(breaks=c(0,25000,50000,75000,100000,125000,150000), 
                     limits=c(0,165000),
                     labels=c("0","25k","50k","75k","100k","125k","150k")) +
  scale_x_continuous(breaks=c(1968,1980,1990,2000,2010,2024)) +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5), 
        legend.position="none",
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans"))  
popplotly <- ggplotly(popplot,tooltip="text")
popplotly

# Now clean up the working memory a bit
rm(ofmpop1960,popplot,popdl,tempdat,url)










# Now get data on the age distribution in Bellevue broken down by sex in the most
# current year of data available. First, pull the data labels for Subject Table 
# S0101 from the variable list & retain the labels of interest.
agelabs <- acs1sub %>%
  filter(str_detect(name,"S0101")) %>%
  separate(col="label",into=c("col1","sex","co3","col4","age"),sep="!!") %>%
  filter(sex %in% c("Male","Female"),
         age %in% c("Under 5 years","5 to 9 years","10 to 14 years","15 to 17 years",
                       "18 to 24 years","25 to 29 years","30 to 34 years",
                       "35 to 39 years","40 to 44 years","45 to 49 years",
                       "50 to 54 years","55 to 59 years","60 to 64 years",
                       "65 years and over")) %>%
  select("variable"="name",
         "sex","age")

# Now load the data, subset them to Bellevue, add join with the labels.
agedat <- get_acs(geography="place",state="WA",year=syear,survey="acs1",
                  table = "S0101") %>%
  filter(GEOID=="5305210") %>%
  right_join(.,agelabs) %>%
  select(-"variable")

# Now condense some of the age categories to make the figure a little less 
# chaotic/more user friendly
agedat2 <- agedat %>%
  rename("e"="estimate") %>%
  pivot_wider(names_from="age",values_from= c("e","moe"),names_vary="slowest") %>%
  mutate("e_<10"=`e_Under 5 years`+`e_5 to 9 years`,
         "moe_<10"=sqrt((`moe_Under 5 years`^2)+(`moe_5 to 9 years`^2)),
         
         "e_10-17"=`e_10 to 14 years`+`e_15 to 17 years`,
         "moe_10-17"=sqrt((`moe_10 to 14 years`^2)+(`moe_15 to 17 years`^2)),
         
         "e_18-29"=`e_18 to 24 years`+`e_25 to 29 years`,
         "moe_18-29"=sqrt((`moe_18 to 24 years`^2)+(`moe_25 to 29 years`^2)),
         
         "e_30-44"=`e_30 to 34 years`+`e_35 to 39 years`+`e_40 to 44 years`,
         "moe_30-44"=sqrt((`moe_30 to 34 years`^2)+(`moe_35 to 39 years`^2)+
                            (`moe_40 to 44 years`^2)),
         
         "e_45-64"=`e_45 to 49 years`+`e_50 to 54 years`+`e_55 to 59 years`+
           `e_60 to 64 years`,
         "moe_45-64"=sqrt((`moe_45 to 49 years`^2)+(`moe_50 to 54 years`^2)+
                            (`moe_55 to 59 years`^2)+(`moe_60 to 64 years`^2))) %>%
  select("GEOID":"sex","e_<10":"moe_45-64",
         "e_65+"="e_65 years and over",
         "moe_65+"="moe_65 years and over") %>%
  pivot_longer(cols="e_<10":"moe_65+",names_to="variable",values_to="value") %>%
  separate(col="variable",into=c("type","age"),sep="_") %>%
  pivot_wider(names_from="type",values_from="value")

# Now create a pyramid plot of age broken down by sex with error bars. Then use  
# the plotly package to make it interactive. Then clean up the working memory.
ageplot <- 
  ggplot(agedat2,aes(x=`age`,y=`e`,fill=`sex`,
                     ymin=`e`-`moe`,ymax=`e`+`moe`,                                       
                     text=paste("Estimate:",scales::comma(`e`),
                                "<br>",
                                "MOE: ±",scales::comma(round(`moe`,0)))
  )) +                          
  geom_bar(position=position_dodge(),stat="identity") +                                                
  geom_errorbar(position=position_dodge(.9),width=0.1,lwd=1.2,linetype=2,color="#9E1B18") +                       
  scale_y_continuous(breaks=c(0,3000,6000,9000,12000,15000,18000,21000,24000), 
                     labels=c("0","3k","6k","9k","12k","15k","18k","21k","24k")) + 
  labs(x="",y="",title="",fill="Sex") +                                               
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(),                                           
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5),
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans")) +   
  scale_fill_manual(values = c("#4E842A","#006598"))                                 
ageplotly <- ggplotly(ageplot,tooltip="text")
ageplotly
rm(agelabs,agedat,agedat2,ageplot)









# Now let's put some data together on the racial/ethnic diversity in Bellevue by
# Census Tract. To start, get all the 2020 blocks in Bellevue. 
bvue_blocks <- read_csv2(paste("https://www2.census.gov/geo/maps/DC2020/DC20BLK/",
                               "st53_wa/place/p5305210_bellevue/",
                               "DC20BLK_P5305210_BLK2MS.txt",sep="")) %>%
  mutate("GEOID"=as.character(FULLCODE)) %>%
  select(GEOID)

# Now get 2020 block population data for King County, subset to Bvue blocks, & 
# then to those that had anyone living in them.
bvue_blks <- get_decennial(geography="block",state="WA",county="033",year=2020,
                           sumfile="pl",
                           variables = c("pop"  = "P1_001N")) %>%
  rename("pop"=value) %>%
  select("GEOID","pop") %>%
  right_join(.,bvue_blocks) %>%
  filter(pop>0)

# Now summarize things to the tract level retaining the number of Bvue blocks 
# within each Tract & their population.
bvue_tracts <- bvue_blks %>%
  mutate("GEOID"=substr(GEOID,1,11)) %>%
  group_by(GEOID) %>%
  summarise("blks"=n(),
            "pop"=sum(pop)) %>%
  ungroup()

# Now let's get race/ethnicity by tract across KC, subset to Bvue tracts, then
# condense some of the categories. 
bvue_tracts_re <- get_acs(geography="tract",state="WA",county="033",year=2022,
                          survey="acs5",
                          variables = c("Total Population"= "B01001_001",
                                        "White"           = "DP05_0079P",
                                        "Black"           = "DP05_0080P",
                                        "Asian"           = "DP05_0082P",
                                        "Latino"          = "DP05_0073P",
                                        "Other"           = "DP05_0081P",
                                        "Other"           = "DP05_0083P",
                                        "Other"            = "DP05_0084P",
                                        "Multiracial"     = "DP05_0085P")) %>%
  rename("e"=estimate) %>%
  right_join(.,bvue_tracts) %>%
  group_by(GEOID,variable) %>%
  summarise("e"=sum(e),
            "moe"=sqrt(sum(moe^2)),
            "moe"=round(moe,1)) %>%
  ungroup() 

# Now we're going to compute each tract's block level dissimilarity score in terms
# NH white vs. other. First get the data for KC, subset to Bvue blocks, condense
# some of the categories & create a tract id.
bvue_blksdat <- get_decennial(geography="block",state="WA",county="033",year=2020,
                              sumfile="pl",
                              variables = c("White"  = "P2_005N",
                                            "BIPOC"  = "P2_006N",
                                            "BIPOC"  = "P2_008N",
                                            "BIPOC" = "P2_002N",
                                            "BIPOC"  = "P2_007N",
                                            "BIPOC"  = "P2_009N",
                                            "BIPOC"    = "P2_010N",
                                            "BIPOC"   = "P2_011N")) %>%
  right_join(.,bvue_blks) %>%
  rename("e"=value) %>%
  group_by(GEOID,variable) %>%
  summarise("e"=sum(e)) %>%
  ungroup() %>%
  mutate("tract"=substr(GEOID,1,11)) 

# Now compute the dissimilarity score within each tract.
bvuedis <- bvue_blksdat %>%
  group_by(tract) %>%
  group_modify(~
                 dissimilarity(.x,group="variable",unit="GEOID",weight="e")
  ) %>% 
  mutate("variable"="Dissimilarity",
         "e"=round(est,3)) %>%
  select("GEOID"="tract",
         "variable","e")

# Now bind the dissimilarity score to the race/ethnicity data for Bellevue 
bvue_tracts_dat <- bind_rows(bvue_tracts_re,bvuedis)

# Now get the map geometries for creating a visualization of the data in the app &
# clean up the working memory a bit.
bvue_tracts_map <- tracts(state="WA",county="033",year=2020) %>%
  select("GEOID",
         "NAME"="NAMELSAD") %>%
  right_join(.,bvue_tracts_dat) %>%
  erase_water(.,area_threshold=.6,year=2020) %>%
  st_transform(.,4326)
rm(bvue_blocks,bvue_blks,bvue_tracts,bvue_tracts_re,bvue_blksdat,bvuedis,
   bvue_tracts_dat)










# Now get the data on race& age for Bellevue. First pull the data labels from 
# the variable list, then loop over the desired tables to pull all the data
# in one set of code.
rcagelabs <- acs5yr %>%
  filter(str_detect(name,"B01001")) %>%
  filter(!str_detect(name,"B01001_")) %>%
  select(-geography) %>%
  rename("variable"="name")
acstables <- c("B01001B","B01001C","B01001D","B01001E","B01001F",
               "B01001G","B01001H","B01001I")

rcagedat <- map_dfr(acstables, ~
                      get_acs(geography="place",state="WA",year=syear,survey="acs5",
                    table = .x)) %>%
  filter(GEOID=="5305210") %>%
  left_join(.,rcagelabs) %>%
  separate(col="label",into = c("col1","col2","sex","age"),sep="!!") %>%
  mutate("sex"=str_replace(sex,":",""),
         "race"=str_replace(concept,"Sex by Age \\(",""),
         "race"=str_replace(race,"\\)","")) %>%
  select(-c("variable","col1","col2","concept")) %>%
  filter(!is.na(sex) & !is.na(age))

# Now condense some of the categories to make the figure a little less chaotic &
# format the data for graphing.
rcagedat2 <- rcagedat %>%
  rename("e"=estimate) %>%
  pivot_wider(names_from="age",values_from= c("e","moe"),names_vary="slowest") %>%
  mutate("e_<10"=`e_Under 5 years`+`e_5 to 9 years`,
         "moe_<10"=sqrt((`moe_Under 5 years`^2)+(`moe_5 to 9 years`^2)),
         
         "e_10-17"=`e_10 to 14 years`+`e_15 to 17 years`,
         "moe_10-17"=sqrt((`moe_10 to 14 years`^2)+(`moe_15 to 17 years`^2)),
         
         "e_18-29"=`e_18 and 19 years`+`e_20 to 24 years`+`e_25 to 29 years`,
         "moe_18-29"=sqrt((`moe_18 and 19 years`^2)+(`moe_20 to 24 years`^2)+
                            (`moe_25 to 29 years`^2)),
         
         "e_30-44"=`e_30 to 34 years`+`e_35 to 44 years`,
         "moe_30-44"=sqrt((`moe_30 to 34 years`^2)+(`moe_35 to 44 years`^2)),
         
         "e_45-64"=`e_45 to 54 years`+`e_55 to 64 years`,
         "moe_45-64"=sqrt((`moe_45 to 54 years`^2)+(`moe_55 to 64 years`^2)),
         
         "e_65+"=`e_65 to 74 years`+`e_75 to 84 years`+`e_85 years and over`,
         "moe_65+"=sqrt((`moe_65 to 74 years`^2)+(`moe_75 to 84 years`^2)+
                          (`moe_85 years and over`^2))) %>%
  select("GEOID":"race","e_<10":"moe_65+") %>%
  pivot_longer(cols="e_<10":"moe_65+",names_to="variable",values_to="value") %>%
  separate(col="variable",into=c("type","Age Group"),sep="_") %>%
  pivot_wider(names_from="type",values_from="value") %>%
  mutate("race"=case_when(race=="Black or African American Alone"~"Black",
                          race=="American Indian and Alaska Native Alone"~"AIAN",
                          race=="Asian Alone"~"Asian",
                          race=="Native Hawaiian and Other Pacific Islander Alone"~"HAPI",
                          race=="Some Other Race Alone"~"Other",
                          race=="Two or More Races"~"Twopl",
                          race=="White Alone, Not Hispanic or Latino"~"White",
                          TRUE~"Hispanic/Latino")) %>%
  group_by(race,`Age Group`) %>%
  summarise("e"=sum(e),
            "moe"=sqrt(sum(moe^2))) %>%
  pivot_wider(names_from="race",values_from=c("e","moe"),names_vary="slowest") %>%
  mutate("e_All Other Social Groups"=e_AIAN+e_HAPI+e_Other+e_Twopl,
         "moe_All Other Social Groups"=sqrt((moe_AIAN^2)+(moe_HAPI^2)+
                                              (moe_Other^2)+(moe_Twopl^2))) %>%
  select("Age Group","e_White","moe_White","e_Asian","moe_Asian",
         "e_Hispanic/Latino","moe_Hispanic/Latino","e_Black","moe_Black",
         "e_All Other Social Groups","moe_All Other Social Groups") %>%
  pivot_longer(cols="e_White":"moe_All Other Social Groups",
               names_to="variable",values_to="value") %>%
  separate(col="variable",into=c("type","Race/Ethnicity"),sep="_") %>%
  pivot_wider(names_from="type",values_from="value") 

# Now make the figure for race by age & clean up the working memory.
rcageplot <- 
  ggplot(rcagedat2,aes(x=`Age Group`,y=`e`,
                       fill=factor(`Race/Ethnicity`, 
                                   levels=c("White","Asian","Hispanic/Latino",
                                            "Black","All Other Social Groups")),
                       text=paste("Estimate:",scales::comma(`e`),
                                  "<br>",
                                  "MOE: ±",scales::comma(round(`moe`,0))))) +   
  geom_bar(stat="identity",width=.6)+   
  labs(x="",y="",title="",fill="Race/Ethnicity") +
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000,35000,40000),   
                     labels=c("0","5k","10k","15k","20k","25k","30k","35k","40k")) + 
  theme_minimal() +  
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5), 
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans")) +   
  scale_fill_manual(values = c("#006598","#C16623","#3B6D64","#4B9DA5","#660C53"))
rcageplotly <- ggplotly(rcageplot,tooltip="text",dynamicTicks=TRUE)
rcageplotly
rm(acstables,rcagelabs,rcagedat,rcagedat2,rcageplot)










# Now use the tidycensus package to import the place of birth data from the 2000
# decennial Census. Then process the data.
fb2000 <- get_decennial(geography="place",state="WA", sumfile = "sf3",year=2000,
                        variables = c("Born in WA"="P021003",
                                      "Born Outside Wa_c"="P021004",
                                      "Born Overseas_c"="P021009",
                                      "Naturalized Citizen"="P021014",
                                      "Noncitizen"="P021015")) %>%
  pivot_wider(names_from="variable",values_from="value") %>%
  mutate("Year"="2000",
         "Born US Citizen (Outside WA)"=`Born Outside Wa_c`+`Born Overseas_c`) %>%
  select("GEOID","NAME","Year","Born in WA","Born US Citizen (Outside WA)",
         "Naturalized Citizen","Noncitizen") %>%
  pivot_longer(cols="Born in WA":"Noncitizen",names_to="measure",values_to="value") %>%
  filter(GEOID=="5305210") 

# Now use the tidycensus package to import the ACS 1-Year data. This has to be 
# done in 3 steps because the number of estimates in the DP02 table changes over
# time.
years <- lst(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023)
fblabs <- map_dfr(years, ~
                     load_variables(year=.x,dataset="acs1"),
                   .id="Year") %>%
  filter(str_detect(name,"B05002")) %>%
  separate(col="label",into=c("type","col2","col3","col4","col5"),sep="!!") %>%
  mutate("label"=case_when(col4=="Born in state of residence"~"Born in WA",
                           col4 %in% c("Born in other state in the United States","Born in other state in the United States:",
                                       "Born outside the United States","Born outside the United States:") &
                             is.na(col5)~"Born US Citizen (Outside WA)",
                           col4=="Naturalized U.S. citizen" & is.na(col5)~
                             "Naturalized Citizen",
                           col4=="Not a U.S. citizen" & is.na(col5)~
                             "Noncitizen",
                           col2 %in% c("Total","Total:") & is.na(col3)~"Total",
                           TRUE~NA)) %>%
  filter(!is.na(label)) %>%
  select("Year",
         "variable"="name",
         "label")

fb10toP_r <- map_dfr(years, ~
                       get_acs(state="WA",geography="place",survey="acs1",year=.x,
                               table="B05002"),
                     .id="Year") %>%
  right_join(.,fblabs)

fb10toP <- fb10toP_r %>%
  filter(GEOID=="5305210") %>%
  group_by(Year,label) %>%
  summarise("GEOID"=first(GEOID),
            "NAME"=first(NAME),
            "e"=sum(estimate),
            "moe"=sqrt(sum(moe^2))) %>%
  pivot_wider(names_from="label",values_from=c("e","moe"),names_vary="slowest") %>%
  pivot_longer(cols="e_Born US Citizen (Outside WA)":"moe_Noncitizen",
               names_to="measure",values_to="value") %>%
  separate(col="measure",into=c("type","measure"),sep="_") %>%
  pivot_wider(names_from="type",values_from="value") %>%
  rename("value"=e)

# Finally combine the cleaned Census & ACS data together for a time-series in 
# Bellevue from 1970-2021. Then clean up the working memory a bit.
pob00toP <- bind_rows(fb2000,fb10toP) %>%
  group_by(Year) %>%
  mutate("year"=as.numeric(Year),
         "p"=prop.table(value),
         "pmoe"=(1/e_Total)*sqrt((moe^2)-((p^2)*(moe_Total^2)))) %>%
  ungroup() %>%
  select(-c("e_Total","moe_Total"))

# Now make the figure for place of birth over time & clean up the working memory.
pobplot <- 
  ggplot(pob00toP,aes(x=`year`,y=`value`,
                      fill=factor(`measure`,levels=c("Noncitizen","Naturalized Citizen",
                                                     "Born US Citizen (Outside WA)","Born in WA")),
                      text=paste("Estimate:",scales::comma(`value`),
                                 "<br>",
                                 "MOE: ±",scales::comma(round(`moe`,0)),
                                 "<br>",
                                 "Percent:",scales::percent(round(`p`,3)),
                                 "<br>",
                                 "Percent MOE: ±",scales::percent(round(`pmoe`,3))))) +   
  geom_bar(stat="identity",width=1)+   
  labs(x="",y="",title="",fill="Place of Birth/Citizenship") +
  scale_y_continuous(breaks=c(0,25000,50000,75000,100000,125000,150000),   
                     labels=c("0","25k","50k","75k","100k","125k","150k")) + 
  theme_minimal() +  
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5), 
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans")) +   
  scale_fill_manual(values = c("#4B9DA5","#3B6D64","#006598","#164356"))
pobplotly <- ggplotly(pobplot,tooltip="text")
pobplotly
rm(list=ls(pattern="fb"),pob00toP,pobplot,years)










# Now let's get data on people who speak a language other than English in their
# home & with limited English proficiency. We want this over time in Bellevue &
# across multiple geographies in the most recent year of data available.

# 2000 Census data in two parts - one for total/non-LEP & one for LEP. Then 
# clean things up & merge them.
langlabs <- load_variables(2000,"sf3") %>%
  filter(str_detect(name,"P019")) %>%
  select("variable"="name",
         "label") 

lang00a <- get_decennial(geography="place",state="WA",sumfile="sf3",year=2000,
                         variables = c("Total Lang Pop"="P109001",
                                       "English"="P109002",
                                       "Not English"="P109003"))

lang00b <- get_decennial(geography="place",state="WA",sumfile="sf3",year=2000,
                         table = "P019") %>%
  left_join(.,langlabs) %>%
  separate(col="label",into=c("col1","age","lang","ability"),sep="!!") %>%
  filter(!is.na(ability) & ability!="Speak English \"very well\"") %>%
  group_by(GEOID,NAME) %>%
  summarise("variable"="Limited English Proficiency",
            "value"=sum(value))

lang00 <- bind_rows(lang00a,lang00b) %>%
  pivot_wider(names_from = "variable") %>%
  mutate("Year"="2000",
         "Not English (Percent)"=`Not English`/`Total Lang Pop`,
         "Limited English Proficiency (Percent)"=`Limited English Proficiency`/`Total Lang Pop`) %>%
  select("GEOID","NAME","Year","English","Not English",
         "Limited English Proficiency":"Limited English Proficiency (Percent)") %>%
  pivot_longer(cols="English":"Limited English Proficiency (Percent)",
               values_to="estimate",names_to="variable") %>%
  filter(GEOID=="5305210")

# Now get each year of 1-Year ACS data for Bellevue. 
# First extract and format labels for the desired measures from all of the years
# of ACS data available.
years <- lst(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023)
langlabs <- map_dfr(years, ~ 
                      load_variables(year=.x,dataset="acs1/profile"),
                    .id="Year") %>%
  filter(str_detect(name,"DP02_")) %>%
  separate(col="label",into=c("type","col2","col3","col4","col5"),sep="!!") %>%
  filter(type %in% c("Percent","Percent Estimate"),col2=="LANGUAGE SPOKEN AT HOME") %>%
  mutate("label"=case_when(col3=="Language other than English" & is.na(col4)~"Not English (Percent)",
                           col4=="Language other than English" & is.na(col5)~"Not English (Percent)",
                           
                           col3=="Language other than English" & col4=="Speak English less than \"very well\""~
                             "Limited English Proficiency (Percent)",
                           col4=="Language other than English" & col5=="Speak English less than \"very well\""~
                             "Limited English Proficiency (Percent)",
                           TRUE~NA)) %>%
  filter(!is.na(label)) %>%
  select("Year",
         "variable"="name",
         "label")

# Now pull the data for Bellevue
lang10toP <- map_dfr(years, ~
                       get_acs(state="WA",geography="place",survey="acs1",year=.x,
                               table = "DP02"),
                     .id="Year") %>%
  filter(GEOID=="5305210") %>%
  right_join(.,langlabs) %>%
  relocate("Year",.after="NAME") %>%
  select(-"variable") %>%
  rename("variable"="label")

# Now get data for Seattle, King County, WA state, & the US in the most recent 
# year of data.
langlabsP <- langlabs %>%
  mutate("yrid"=as.numeric(Year)) %>%
  filter(yrid==syear) %>%
  select(-"yrid")

geos <- lst("place","county","state","us")
langgeos <- map_dfr(geos, ~
                      get_acs(geography=.x,year=syear,survey="acs1",
                              table = "DP02"),
                    .id="level") %>%
  right_join(.,langlabsP) %>%
  mutate("id"=case_when(level=="\"place\"" & GEOID=="5363000"~1,
                        level=="\"county\"" & GEOID=="53033"~1,
                        level=="\"state\"" & GEOID=="53"~1,
                        level=="\"us\""~1,
                        TRUE~NA)) %>%
  relocate("Year",.after="NAME") %>%
  filter(!is.na(id)) %>%
  select(-c("level","id","variable")) %>%
  rename("variable"="label")

# Now bind all of the ACS data together
lang10toPgeo <- bind_rows(lang10toP,langgeos) %>%
  mutate("estimate"=estimate/100,
         "moe"=moe/100)

# Now bind the combined Census data with the combined ACS data
spoke00toP <- bind_rows(lang00,lang10toPgeo) %>%
  mutate("variable"=case_when(variable=="Not English (Percent)"~"Language Other Than English (Percent)",
                              TRUE~variable),
         "year"=as.numeric(Year))
rm(list=ls(pattern="lang"),years,geos)

# Subset the data to just Bellevue
bvspoke <- spoke80toP %>%
  filter(GEOID=="5305210")

# Subset the current year's comparison estimates
spokecomp <- spoke80toP %>%
  filter(year==syear) %>%
  mutate("NAME"=case_when(NAME=="Bellevue city, Washington"~"Bellevue",
                          NAME=="Seattle city, Washington"~"Seattle",
                          NAME=="King County, Washington"~"King County",
                          TRUE~NAME))

# Now make the interactive plot of English speaking ability in Bellevue over time.
bvspokeplot <- 
  ggplot(data=bvspoke,aes(x=`year`,y=`estimate`,
                          ymin=`estimate`-`moe`,ymax=`estimate`+`moe`,
                          fill=factor(`variable`, 
                                      levels=c("Language Other Than English (Percent)",
                                               "Limited English Proficiency (Percent)")),
                          text=paste("Estimate:",scales::percent(estimate),
                                     "<br>",
                                     "MOE: ±",scales::percent(round(`moe`,3))))) +
  geom_bar(position=position_dodge(), stat="identity") + 
  geom_errorbar(position=position_dodge(.9), color="#9E1B18",lwd=.9,linetype=2,width=.1) +
  labs(x="",y="",title="",fill="Language at Home") +
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5),   # Breaks
                     labels=c("0","10%","20%","30%","40%","50%")) + 
  theme_minimal() +  
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5), 
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans")) +   
  scale_fill_manual(values = c("#006598","#C16623"))
bvspokeplotly <- ggplotly(bvspokeplot,tooltip="text") %>%
  layout(legend=list(x=.3,y=100))
bvspokeplotly

# Now make the interactive plot of English speaking ability in the comparison
# geographies in the most current year of data.
spokecompplot <- 
  ggplot(data=spokecomp,aes(x=factor(`NAME`, 
                                     levels=c("Bellevue","Seattle","King County",
                                              "Washington","United States")),
                            y=`estimate`,
                            ymin=`estimate`-`moe`,ymax=`estimate`+`moe`,            
                            fill=factor(`variable`, 
                                        levels=c("Language Other Than English (Percent)",
                                                 "Limited English Proficiency (Percent)")),
                            text=paste("Estimate:",scales::percent(`estimate`),
                                       "<br>",
                                       "MOE: ±",scales::percent(round(`moe`,3))))) +
  geom_bar(position=position_dodge(), stat="identity") + 
  geom_errorbar(position=position_dodge(.9), color="#9E1B18",lwd=.9,linetype=2,width=.1) +
  labs(x="",y="",title="",fill="Language at Home") +
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5),   # Breaks
                     labels=c("0","10%","20%","30%","40%","50%")) + 
  theme_minimal() +  
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5), 
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans")) +  
  scale_fill_manual(values = c("#006598","#C16623"))
spokecompplotly <- ggplotly(spokecompplot,tooltip="text") %>%
  layout(legend=list(x=.3,y=100))
spokecompplotly

# Now clean up the working memory a bit
rm(bvspoke,bvspokeplot,geos,spoke00toP,spokecomp,spokecompplot)










# Now let's get data on the age of the head of household broken down by whether 
# the household is a: married couple, other family, single, or two+ non-family. 
# First extract the desired labels from the variable list then use those to filter
# the data.
hohlabs <- acs1yr %>%
  filter(str_detect(name,"B25011_")) %>%
  select("variable"="name",
         "label") %>%
  separate(col="label",into=c("col1","col2","tenure","famtype","hhtype","alntype","hohage"),
           sep="!!") %>%
  mutate("hohage"=case_when(is.na(hohage) & 
                              !alntype %in% c("Male householder, no spouse present:",
                                              "Female householder, no spouse present:")~alntype,
                            TRUE~hohage),
         "hhtype"=case_when(variable=="B25011_001"~"Total",
                            TRUE~hhtype),
         "hohage"=case_when(!is.na(hhtype) & is.na(alntype)~"Total",
                            TRUE~hohage)) %>%
  select(-c("col1","col2")) %>%
  filter(!is.na(hohage)) %>%
  mutate(across(tenure:alntype, ~str_replace(.x,":","")))

hohdat <- get_acs(geography="place",state="WA",year=syear, survey = "acs1",
                  table = "B25011") %>%
  right_join(.,hohlabs) %>%
  filter(GEOID=="5305210") %>%
  select(-"variable")

# Now process the data to the desired categories
hohdat2a <- hohdat %>%
  rename("e"="estimate") %>%
  group_by(hhtype,hohage) %>%
  summarise("e"=sum(e),
            "moe"=sqrt(sum(moe^2))) %>%
  mutate("hhtype"=case_when(hhtype=="Householder living alone"~"Single Person",
                            hhtype=="Householder not living alone"~"Other Non-Family",
                            hhtype=="Other family"~"Other Family",
                            hhtype=="Married-couple family"~"Married-Couple Family",
                            TRUE~hhtype),
         "hohage"=case_when(hohage=="Householder 15 to 34 years"~"HoH 15-34",
                            hohage=="Householder 35 to 64 years"~"HoH 35-64",
                            hohage=="Householder 65 years and over"~"HoH 65+",
                            TRUE~hohage)) %>%
  ungroup()

# Now retain just the total count of HHs 
hohdat2b <- hohdat2a %>%
  filter(hhtype=="Total") %>%
  select("hohage",
         "grp_e"="e",
         "grp_moe"="moe") 

# Now retain the total count of HHs by type & bind the total HH count
hohdat2c <- hohdat2a %>%
  filter(hohage!="Total") %>%
  group_by(hohage) %>%
  summarise("grp_e"=sum(e),
            "grp_moe"=sqrt(sum(moe^2))) %>%
  ungroup() %>%
  bind_rows(.,hohdat2b)

# Now join the processed data with hoh age group totals & compute the percentages
hohdat2 <- hohdat2a %>%
  filter(hhtype!="Total") %>%
  left_join(.,hohdat2c) %>%
  mutate("p"=e/grp_e,
         "pmoe"=(1/grp_e)*sqrt((moe^2)-((p^2)*(grp_moe^2)))) 

# Now plot the household age/type data & clean up the working memory a bit
hheadplot <- 
  ggplot(hohdat2,aes(x=`hohage`,y=`e`,
                     fill=factor(`hhtype`, 
                                 levels=c("Other Non-Family","Single Person",
                                          "Other Family","Married-Couple Family")),
                     text=paste("Estimate:",scales::comma(`e`),
                                "<br>",
                                "MOE: ±",scales::comma(round(`moe`,0)),
                                "<br>",
                                "Percent:",scales::percent(round(`p`,3)),
                                "<br>",
                                "Percent MOE: ±",scales::percent(round(`pmoe`,3))
                                
                                ))) +   
  geom_bar(stat="identity",width=.6)+   
  labs(x="",y="",title="",fill = "Household Type") +
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000,35000,40000,
                              45000,50000,55000,60000),   
                     labels=c("0","5k","10k","15k","20k","25k","30k","35k","40k",
                              "45k","50k","55k","60k")) + 
  theme_minimal() +  
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5), 
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans")) +   
  scale_fill_manual(values = c("#006598","#C16623","#3B6D64","#4B9DA5"))
hheadplotly <- ggplotly(hheadplot,tooltip="text",dynamicTicks=TRUE)
hheadplotly
rm(list=ls(pattern="hoh"),hheadplot)










# Pull PUMS data on language spoken at home and with English speaking ability
# along with the PUMS ID for WA using the most recent vintage of ACS 5-Year data.
# Note that the data include both the 2010 & 2020 PUMA codings as of 2022 data 
# vintage.

# Double check the available variables
pumsvars <- pums_variables %>%
  filter(year=="2022" & survey=="acs5")

# Load the data/desired variables along with the labels and person replication
# weights.
pumsdat5 <- get_pums(state="WA",survey="acs5",year=2022,
                     rep_weights="person",recode=TRUE,  
                     variables=c("PUMA10","PUMA20","AGEP","LANX","LANP","ENG")) 

# Now create two new variables denoting when a respondent speaks English less 
# than "very well" & the primary language spoken in their home.
pumsdat5a <- pumsdat5 %>%
  mutate("Trouble Speaking English"=case_when(ENG %in% c("2","3","4")~"Yes",
                                              ENG=="1"|LANX=="2"~"No",
                                              TRUE~NA),
         "Language at Home"=case_when(LANX=="2"~"English",
                                      LANX=="1"~as.character(LANP_label),
                                      TRUE~NA))

# Translate the data into a survey object in order to properly weight the 
# responses, subset the Greater Bellevue PUMA, & drop observations with no 
# language data (i.e. people under the age of 5).
pumsdatsrvy5 <- pumsdat5a %>%
  to_survey(type = "person",design = "rep_weights") %>%
  filter(PUMA10=="11608" |PUMA20=="23304") %>%
  filter(!is.na(`Language at Home`))

# Now get the count of folks within the Greater Bellevue PUMA with limited 
# English proficiency. Retain only the languages whose MOEs<Count
pumsqac5lep <- pumsdatsrvy5 %>%
  group_by(`Language at Home`,`Trouble Speaking English`) %>%
  summarise("e_LEP"=survey_total()) %>%
  mutate("moe_LEP"=round(e_LEP_se*1.645)) %>%
  filter(e_LEP>moe_LEP,`Trouble Speaking English`=="Yes") %>%
  select("Language at Home","e_LEP","moe_LEP") %>%
  ungroup()

# Now do as above, but instead for primary language spoken at home.
pumsqac5lang <- pumsdatsrvy5 %>%
  group_by(`Language at Home`) %>%
  summarise("e_Speakers"=survey_total()) %>%
  mutate("moe_Speakers"=round(e_Speakers_se*1.645)) %>%
  filter(e_Speakers>moe_Speakers) %>%
  select("Language at Home","e_Speakers","moe_Speakers") %>%
  ungroup()

# Now join the counts for LEP speakers and the primary languages as a whole. Then  
# compute the percentage of all residents represented by limited English speakers 
# of each language.
pumsqacc <- left_join(pumsqac5lang,pumsqac5lep) %>%
  mutate("Rank"=rank(desc(e_Speakers))) %>%
  filter(Rank<12,`Language at Home`!="English") %>%
  mutate("p_LEP"=e_LEP/e_Speakers,
         "pmoe_LEP"=(1/e_Speakers)*sqrt((moe_LEP^2)-((p_LEP^2)*(moe_Speakers^2))),
         
         "Language at Home"=fct_reorder(`Language at Home`,Rank)) %>%
  pivot_longer(cols="e_Speakers":"moe_LEP",names_to="measure",values_to="value") %>%
  separate(col="measure",into=c("type","measure"),sep="_") %>%
  pivot_wider(names_from="type",values_from="value") %>%
  mutate("measure"=case_when(measure=="Speakers"~"Total Speakers",
                             TRUE~"Speakers with Limited English Proficiency"))

# Now plot the data & translate into a plotly.
langplot <- 
  ggplot(data=pumsqacc,aes(x=`Language at Home`,y=`e`,
                           ymin=`e`-`moe`,ymax=`e`+`moe`,
                           fill=factor(`measure`, 
                                       levels=c("Total Speakers",
                                                "Speakers with Limited English Proficiency")),
                           text=paste("Estimate:",scales::comma(`e`),
                                      "<br>",
                                      "MOE: ±",scales::comma(round(`moe`,0)),
                                      "<br>",
                                      "Percent LEP:",scales::percent(round(`p_LEP`,3)),
                                      "<br>",
                                      "MOE: ±",scales::percent(round(`pmoe_LEP`,3))))) +
  geom_bar(position=position_dodge(), stat="identity") + 
  geom_errorbar(position=position_dodge(.9), color="#9E1B18",lwd=.9,linetype=2,width=.1) +
  labs(x="",y="",title="",fill="Group") +
  scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,12000,14000),   
                     labels=c("0","2k","4k","6k","8k","10k","12k","14k")) + 
  theme_minimal() +  # Tufte theme from ggfortify
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5), 
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans")) +   
  scale_fill_manual(values = c("#006598","#C16623"))
langplotly <- ggplotly(langplot,tooltip="text") %>%
  layout(legend=list(x=.3,y=100))
langplotly
rm(list=ls(pattern="pums"),langplot)










# Pull race/ethnicity data for Bellevue over time. First extract all the labels
# from the variable list then use them to filter the DP05 data.
years <- lst(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023)
raclabs <- map_dfr(years, ~
                     load_variables(year=.x,dataset="acs1/profile"),
                   .id="Year") %>%
  filter(str_detect(name,"DP05")) %>%
  separate(col="label",into=c("type","col2","col3","col4","col5","col6"),sep="!!") %>%
  filter(type=="Percent" & col2=="HISPANIC OR LATINO AND RACE") %>%
  mutate(across("type":"col5", ~str_remove(.x,":")),
         "label"=case_when(col3=="Hispanic or Latino (of any race)" & is.na(col4)~"Latino",
                           col4=="Hispanic or Latino (of any race)" & is.na(col5)~"Latino",
                           
                           col3=="Not Hispanic or Latino" & col4=="White alone"~"White",
                           col4=="Not Hispanic or Latino" & col5=="White alone"~"White",
                           
                           col3=="Not Hispanic or Latino" & col4=="Asian alone"~"Asian",
                           col4=="Not Hispanic or Latino" & col5=="Asian alone"~"Asian",
                           
                           col3=="Not Hispanic or Latino" & col4=="Black or African American alone"~"Black",
                           col4=="Not Hispanic or Latino" & col5=="Black or African American alone"~"Black",
                           
                           col3=="Not Hispanic or Latino" & 
                             col4 %in% c("American Indian and Alaska Native alone",
                                         "Native Hawaiian and Other Pacific Islander alone",
                                         "Some other race alone")~"All Other Groups",
                           col4=="Not Hispanic or Latino" & 
                             col5 %in% c("American Indian and Alaska Native alone",
                                         "Native Hawaiian and Other Pacific Islander alone",
                                         "Some other race alone","Some Other Race alone")~
                             "All Other Groups",
                           
                           col3=="Not Hispanic or Latino" & col4 %in% c("Two or more races","Two or More Races") &
                             is.na(col5)~"Multiracial",
                           col4=="Not Hispanic or Latino" & col5 %in% c("Two or more races","Two or More Races") &
                             is.na(col6)~"Multiracial")) %>%
  filter(!is.na(label)) %>%
  select("Year",
         "variable"="name",
         "label")

racdat <- map_dfr(years, ~
                    get_acs(state="WA",geography="place",survey="acs1",year=.x,
                            table="DP05"),
                  .id="Year") %>%
  right_join(.,raclabs) %>%
  filter(GEOID=="5305210") %>%
  group_by(Year,label) %>%
  summarise("GEOID"=first(GEOID),
            "NAME"=first(NAME),
            "e"=sum(estimate)/100,
            "moe"=(sqrt(sum(moe^2)))/100,
            "year2"=as.numeric(Year))

# Now create the figure & convert to a plotly plot. Then clean up the working 
# memory a bit.
racetsplot <- 
  ggplot(racdat,aes(x=`year2`,y=`e`,
                     ymin=`e`-`moe`,ymax=`e`+`moe`,
                     color=factor(`label`,levels=c("White","Asian","Latino",
                                                      "Multiracial","Black",
                                                      "All Other Groups")),
                     group=`label`,
                     text=paste("Estimate:",scales::percent(round(`e`,3)),
                                "<br>",
                                "MOE: ±",scales::percent(round(`moe`,3))))) +
  geom_errorbar(width=0.1,lwd=1.2,linetype=2,color="#9E1B18") + 
  geom_line(lwd=2) + 
  geom_point(size=5) +
  scale_color_manual(values=c("White"="#006598",
                              "Asian"="#C16623",
                              "Latino"="#3B6D64",
                              "Multiracial"="#4B9DA5",
                              "Black"="#660C53",
                              "All Other Groups"="#4E842A")) +                                 
  theme_minimal() + 
  labs(x="",y="",title="",color="Race/Ethnicity") +
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7),
                     labels=scales::percent,
                     limits=c(0,.701)) +
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018,2020,2022)) +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans"))  
racetsplotly <- ggplotly(racetsplot,tooltip="text") %>%
  layout(legend=list(x=100,y=.3),
         yaxis=list(tickformat=".0%")) 
racetsplotly
rm(racetsplot,racdat,raclabs,years)










# Now get data on the relative size of age groups in Bellevue over time.
# First extract and format labels for the desired measures from all of the years
# of ACS data available. Note that from 2010-2016 the table contained ONLY 
# percentages & after that BOTH counts and estimates.
years <- lst(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023)
agelabs <- map_dfr(years, ~ 
                     load_variables(year=.x,dataset="acs1/subject"),
                   .id="Year") %>%
  filter(str_detect(name,"S0101_")) %>%
  separate(col="label",into=c("type","col2","col3","col4","col5"),sep="!!") %>%
  filter(type %in% c("Total","Percent") | col2=="Percent") %>%
  filter(!(Year=="2017" & type=="Total")) %>%
  mutate("label"=case_when(col4=="Under 5 years" | col5=="Under 5 years"~"Under 5",
                           col4 %in% c("5 to 14 years","15 to 17 years") |
                             col5 %in% c("5 to 14 years","15 to 17 years")~"Ages 5-17",
                           col4 %in% c("18 to 24 years","25 to 29 years","30 to 34 years",
                                       "35 to 39 years","40 to 44 years") |
                             col5 %in% c("18 to 24 years","25 to 29 years","30 to 34 years",
                                         "35 to 39 years","40 to 44 years")~"Ages 18-44",
                           col4 %in% c("45 to 49 years","50 to 54 years","55 to 59 years",
                                       "60 to 64 years") |
                             col5 %in% c("45 to 49 years","50 to 54 years","55 to 59 years",
                                         "60 to 64 years")~"Ages 45-64",
                           col4=="65 years and over" | col5=="65 years and over"~"Ages 65+",
                           TRUE~NA)) %>%
  filter(!is.na(label)) %>%
  select("Year",
         "variable"="name",
         "label")

# Now pull the data for Bellevue
ageTS_r <- map_dfr(years, ~
                     get_acs(state="WA",geography="place",survey="acs1",year=.x,
                             table = "S0101"),
                   .id="Year") %>%
  filter(GEOID=="5305210") %>%
  right_join(.,agelabs) %>%
  relocate("Year",.after="NAME") %>%
  select(-"variable") %>%
  rename("variable"="label")

# Now comllapse the data by age group & format some things for plotting
ageTS <- ageTS_r %>%
  group_by(Year,variable) %>%
  summarise("e"=sum(estimate),
            "moe"=sqrt(sum(moe^2))) %>%
  ungroup() %>%
  mutate("factid"=case_when(variable=="Under 5"~1,
                            variable=="Ages 5-17"~2,
                            variable=="Ages 18-44"~3,
                            variable=="Ages 45-64"~4,
                            TRUE~5),
         "fctlev"=fct_reorder(variable,factid),
         "fctlevalt"=fct_reorder(variable,-factid)) %>%
  arrange(Year,fctlev) %>%
  group_by(Year) %>%
  mutate("e"=e/100,
         "moe"=round(moe,1)/100,
         "s"=cumsum(e),
         "year2"=as.numeric(Year))

ageTSplot <- 
  ggplot(ageTS,aes(x=`year2`,y=`e`,
                   fill=`fctlevalt`)) +    
  scale_fill_manual(values = c("#006598","#C16623","#3B6D64","#4B9DA5","#660C53")) +
  geom_area() +
  geom_point(size=5,alpha=0,aes(x=`year2`,y=`s`,
                                fill=`fctlevalt`,
                                text=paste("Estimate:",scales::percent(`e`),
                                           "<br>",
                                           "MOE: ±",scales::percent(`moe`)))) +
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1),
                     labels=scales::percent,
                     limits=c(0,1.01)) + 
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018,2020,2022)) +
  labs(x="",y="",title="",fill="Age Group") +                                               
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(),                                           
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(hjust=.5),
        axis.ticks=element_blank(),
        axis.text.x=element_text(hjust=0.5,size=14,family="sans"),
        axis.text.y=element_text(hjust=0.5,size=14,family="sans"))    
ageTSplotly <- ggplotly(ageTSplot,tooltip = "text") %>%
  layout(legend=list(x=100,y=.6),
         yaxis=list(tickformat=".0%")) 
ageTSplotly
rm(ageTS,ageTS_r,agelabs,ageTSplot,years)










# Now clean up the working memory a bit, check to make sure the folder where we
# want to save the objects we've created for working with later exists, then 
# save the workspace in that folder.
rm(acs1pro,acs1sub,acs1yr,acs5pro,acs5sub,acs5yr,decvars,years,hexcodes,syear)
save.image(file="DPAppData.RData")









