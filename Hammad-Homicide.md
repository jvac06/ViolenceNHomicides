Hammad Homicide
================

## Updating Libraries

``` r
library(rio)
library(formattable)
library(dplyr)
library(tidyverse)
library(readxl)
library(corrplot)
library(stargazer)
library(car)
library(PerformanceAnalytics)
library(tidyr)
library(tm)
library(MASS)
library(AER)
library(ggplot2)
library(lubridate)
library(lattice)
library(lme4)
library(MuMIn)
library("ggridges")
library("hrbrthemes")
library(ggthemes)
library("maps")
library("mapproj")
library(cowplot)
options(scipen = 999)
```

## Importing Data

``` r
df<-read_xlsx("data/HomicideData_Hammad.xlsx",sheet = "HomicideData")
```

    ## New names:
    ## * ViolentCrime -> ViolentCrime...14
    ## * PropertyCrime -> PropertyCrime...15
    ## * ViolentCrime -> ViolentCrime...39
    ## * PropertyCrime -> PropertyCrime...40

## Checking for nulls

``` r
colSums(is.na(df))
```

    ##                     State                 StateCode 
    ##                         0                         0 
    ##                      Year              NumHomicides 
    ##                         0                         0 
    ##              NumDrugUsers           NumAlcoholUsers 
    ##                        51                         0 
    ##    LawEnforcementOfficers                Population 
    ##                         2                         0 
    ##                   RealGDP          RealGDPperCapita 
    ##                         0                         0 
    ##                 GiniIndex      SeriousMentalIllness 
    ##                         0                        51 
    ##        UnenemploymentRate         ViolentCrime...14 
    ##                         0                         0 
    ##        PropertyCrime...15 SubAbuseInpatientCareBeds 
    ##                         0                       153 
    ##           CannabisMedical      CannabisRecreational 
    ##                         0                         0 
    ##                GunLawRank                     Black 
    ##                         8                         0 
    ##                  Hispanic                      Area 
    ##                         0                         0 
    ##              Adults 19-25              Adults 26-34 
    ##                         0                         0 
    ##                Big Cities                 Education 
    ##                         0                        60 
    ##                    Region           AnyOtherWeapon1 
    ##                        10                        51 
    ##        DestructiveDevice2               Machinegun3 
    ##                        51                        51 
    ##                 Silencer4       ShortBarreledRifle5 
    ##                        51                        51 
    ##     ShortBarreledShotgun6              TotalWeapons 
    ##                        51                        51 
    ##        MurderNSlaugtherVC                    RapeVC 
    ##                        10                        10 
    ##                 RobberyVC       AggravatedAssaultVC 
    ##                        10                        10 
    ##         ViolentCrime...39        PropertyCrime...40 
    ##                         0                         0 
    ##          GunLawStrictness 
    ##                       110

``` r
str(df)
```

    ## tibble [510 x 41] (S3: tbl_df/tbl/data.frame)
    ##  $ State                    : chr [1:510] "ALABAMA" "ALASKA" "ARIZONA" "ARKANSAS" ...
    ##  $ StateCode                : chr [1:510] "AL" "AK" "AZ" "AR" ...
    ##  $ Year                     : num [1:510] 2019 2019 2019 2019 2019 ...
    ##  $ NumHomicides             : num [1:510] 128 78 398 253 1818 ...
    ##  $ NumDrugUsers             : num [1:510] 125000 22000 191000 65000 1204000 ...
    ##  $ NumAlcoholUsers          : num [1:510] 219000 41000 289000 122000 2079000 ...
    ##  $ LawEnforcementOfficers   : num [1:510] 10804 1271 13029 7138 79616 ...
    ##  $ Population               : num [1:510] 4767100 701700 7098000 2922500 38642700 ...
    ##  $ RealGDP                  : num [1:510] 201985834440 48150509075 327683930943 115939796370 2773617529880 ...
    ##  $ RealGDPperCapita         : num [1:510] 42371 68620 46166 39671 71776 ...
    ##  $ GiniIndex                : num [1:510] 0.479 0.428 0.466 0.476 0.489 ...
    ##  $ SeriousMentalIllness     : num [1:510] 210000 31000 305000 125000 1364000 ...
    ##  $ UnenemploymentRate       : num [1:510] 2.7 6.1 4.5 3.5 3.9 2.5 3.8 4 5.3 2.9 ...
    ##  $ ViolentCrime...14        : num [1:510] 25046 6343 33141 17643 174331 ...
    ##  $ PropertyCrime...15       : num [1:510] 131133 21294 177638 86250 921114 ...
    ##  $ SubAbuseInpatientCareBeds: num [1:510] 1109 346 1500 546 13593 ...
    ##  $ CannabisMedical          : num [1:510] 0 1 1 1 1 1 1 1 1 1 ...
    ##  $ CannabisRecreational     : num [1:510] 0 1 0 0 1 1 0 0 1 0 ...
    ##  $ GunLawRank               : chr [1:510] "F" "F" "F" "F" ...
    ##  $ Black                    : num [1:510] 26.5 2.2 4.3 15.2 5.3 3.8 10 21.8 45 15 ...
    ##  $ Hispanic                 : num [1:510] 4.4 7 31.8 7.8 39.5 21.8 16.9 9.7 11.2 26.6 ...
    ##  $ Area                     : num [1:510] 51609 589757 113909 53104 158693 ...
    ##  $ Adults 19-25             : num [1:510] 8.7 9.3 9.3 8.7 9.2 8.9 8.6 8.1 9 8 ...
    ##  $ Adults 26-34             : num [1:510] 11.5 13.9 12.1 11.5 13.7 14.1 11.3 11.8 21.6 11.4 ...
    ##  $ Big Cities               : num [1:510] 0 0 1 0 4 1 0 0 1 1 ...
    ##  $ Education                : num [1:510] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Region                   : chr [1:510] "SouthEast" "West" "SouthWest" "SouthEast" ...
    ##  $ AnyOtherWeapon1          : num [1:510] 1228 332 651 1365 4113 ...
    ##  $ DestructiveDevice2       : num [1:510] 80726 5346 70179 108692 299766 ...
    ##  $ Machinegun3              : num [1:510] 29757 1674 5657 17841 30173 ...
    ##  $ Silencer4                : num [1:510] 46902 9250 26804 55099 15131 ...
    ##  $ ShortBarreledRifle5      : num [1:510] 7214 2503 4279 19124 13354 ...
    ##  $ ShortBarreledShotgun6    : num [1:510] 2438 1415 1231 2696 14129 ...
    ##  $ TotalWeapons             : num [1:510] 168265 20520 108801 204817 376666 ...
    ##  $ MurderNSlaugtherVC       : num [1:510] 358 69 365 242 1690 ...
    ##  $ RapeVC                   : num [1:510] 2068 1088 3662 2331 14799 ...
    ##  $ RobberyVC                : num [1:510] 3941 826 6410 1557 52301 ...
    ##  $ AggravatedAssaultVC      : num [1:510] 18679 4360 22704 13513 105541 ...
    ##  $ ViolentCrime...39        : num [1:510] 25046 6343 33141 17643 174331 ...
    ##  $ PropertyCrime...40       : num [1:510] 131133 21294 177638 86250 921114 ...
    ##  $ GunLawStrictness         : num [1:510] 5 5 5 5 1 3 1 2 NA 3 ...

## Dropping Columbia from the Model

``` r
df<-df[ which(df$State!="DISTRICT OF COLUMBIA"), ]
```

## Converting to factor and releveling

``` r
df$GunLawRank<-as.factor(df$GunLawRank)
df$StateCode<-as.factor(df$StateCode)
df$State<-as.factor(df$State)
df$CannabisMedical<-as.factor(df$CannabisMedical)
df$CannabisRecreational<-as.factor(df$CannabisRecreational)
df$Region<-as.factor(df$Region)
```

## Calculating Homicide Rate

``` r
df$HomicideRate<-(df$NumHomicides/df$Population)*100000
glimpse(df)
```

    ## Rows: 500
    ## Columns: 42
    ## $ State                     <fct> ALABAMA, ALASKA, ARIZONA, ARKANSAS, ...
    ## $ StateCode                 <fct> AL, AK, AZ, AR, CA, CO, CT, DE, FL, ...
    ## $ Year                      <dbl> 2019, 2019, 2019, 2019, 2019, 2019, ...
    ## $ NumHomicides              <dbl> 128, 78, 398, 253, 1818, 275, 116, 5...
    ## $ NumDrugUsers              <dbl> 125000, 22000, 191000, 65000, 120400...
    ## $ NumAlcoholUsers           <dbl> 219000, 41000, 289000, 122000, 20790...
    ## $ LawEnforcementOfficers    <dbl> 10804, 1271, 13029, 7138, 79616, 124...
    ## $ Population                <dbl> 4767100, 701700, 7098000, 2922500, 3...
    ## $ RealGDP                   <dbl> 201985834440, 48150509075, 327683930...
    ## $ RealGDPperCapita          <dbl> 42370.80, 68619.79, 46165.67, 39671....
    ## $ GiniIndex                 <dbl> 0.4791, 0.4284, 0.4664, 0.4765, 0.48...
    ## $ SeriousMentalIllness      <dbl> 210000, 31000, 305000, 125000, 13640...
    ## $ UnenemploymentRate        <dbl> 2.7, 6.1, 4.5, 3.5, 3.9, 2.5, 3.8, 4...
    ## $ ViolentCrime...14         <dbl> 25046, 6343, 33141, 17643, 174331, 2...
    ## $ PropertyCrime...15        <dbl> 131133, 21294, 177638, 86250, 921114...
    ## $ SubAbuseInpatientCareBeds <dbl> 1109, 346, 1500, 546, 13593, 1135, 1...
    ## $ CannabisMedical           <fct> 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, ...
    ## $ CannabisRecreational      <fct> 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, ...
    ## $ GunLawRank                <fct> F, F, F, F, A, C, A, B, C, F, A, F, ...
    ## $ Black                     <dbl> 26.5, 2.2, 4.3, 15.2, 5.3, 3.8, 10.0...
    ## $ Hispanic                  <dbl> 4.4, 7.0, 31.8, 7.8, 39.5, 21.8, 16....
    ## $ Area                      <dbl> 51609, 589757, 113909, 53104, 158693...
    ## $ `Adults 19-25`            <dbl> 8.7, 9.3, 9.3, 8.7, 9.2, 8.9, 8.6, 8...
    ## $ `Adults 26-34`            <dbl> 11.5, 13.9, 12.1, 11.5, 13.7, 14.1, ...
    ## $ `Big Cities`              <dbl> 0, 0, 1, 0, 4, 1, 0, 0, 1, 0, 0, 0, ...
    ## $ Education                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ Region                    <fct> SouthEast, West, SouthWest, SouthEas...
    ## $ AnyOtherWeapon1           <dbl> 1228, 332, 651, 1365, 4113, 1045, 97...
    ## $ DestructiveDevice2        <dbl> 80726, 5346, 70179, 108692, 299766, ...
    ## $ Machinegun3               <dbl> 29757, 1674, 5657, 17841, 30173, 737...
    ## $ Silencer4                 <dbl> 46902, 9250, 26804, 55099, 15131, 41...
    ## $ ShortBarreledRifle5       <dbl> 7214, 2503, 4279, 19124, 13354, 1000...
    ## $ ShortBarreledShotgun6     <dbl> 2438, 1415, 1231, 2696, 14129, 1929,...
    ## $ TotalWeapons              <dbl> 168265, 20520, 108801, 204817, 37666...
    ## $ MurderNSlaugtherVC        <dbl> 358, 69, 365, 242, 1690, 218, 104, 4...
    ## $ RapeVC                    <dbl> 2068, 1088, 3662, 2331, 14799, 3872,...
    ## $ RobberyVC                 <dbl> 3941, 826, 6410, 1557, 52301, 3663, ...
    ## $ AggravatedAssaultVC       <dbl> 18679, 4360, 22704, 13513, 105541, 1...
    ## $ ViolentCrime...39         <dbl> 25046, 6343, 33141, 17643, 174331, 2...
    ## $ PropertyCrime...40        <dbl> 131133, 21294, 177638, 86250, 921114...
    ## $ GunLawStrictness          <dbl> 5, 5, 5, 5, 1, 3, 1, 2, 3, 5, 1, 5, ...
    ## $ HomicideRate              <dbl> 2.685071, 11.115861, 5.607213, 8.656...

## Scaling all numerical variables according to population for easier interpretation

``` r
df$DrugUsers<-(df$NumDrugUsers/df$Population)*1000
df$AlcoholUsers<-(df$NumAlcoholUsers/df$Population)*1000
df$LawOfficers<-(df$LawEnforcementOfficers/df$Population)*10000
df$LawCover<-(df$LawEnforcementOfficers/df$Area)*100
df$GDP<-df$RealGDPperCapita/1000
df$MentalIllness<-(df$SeriousMentalIllness/df$Population)*1000
summary(df$MentalIllness)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   23.60   31.96   35.37   36.22   40.56   58.81      50

## Creating data sets for individual years

``` r
Y19<-df %>%
  filter(Year=="2019")

Y18<-df %>%
  filter(Year=="2018")

Y17<-df %>%
  filter(Year=="2017")

Y16<-df %>%
  filter(Year=="2016")

Y15<-df %>%
  filter(Year=="2015")

Y14<-df %>%
  filter(Year=="2014")

Y13<-df %>%
  filter(Year=="2013")

Y12<-df %>%
  filter(Year=="2012")

Y11<-df %>%
  filter(Year=="2011")

Y10<-df %>%
  filter(Year=="2010")
```

## Creating values for Mapping

``` r
US <- map_data("state")
US$region<-toupper(US$region)
US$State<-US$region
head(US)
```

    ##        long      lat group order  region subregion   State
    ## 1 -87.46201 30.38968     1     1 ALABAMA      <NA> ALABAMA
    ## 2 -87.48493 30.37249     1     2 ALABAMA      <NA> ALABAMA
    ## 3 -87.52503 30.37249     1     3 ALABAMA      <NA> ALABAMA
    ## 4 -87.53076 30.33239     1     4 ALABAMA      <NA> ALABAMA
    ## 5 -87.57087 30.32665     1     5 ALABAMA      <NA> ALABAMA
    ## 6 -87.58806 30.32665     1     6 ALABAMA      <NA> ALABAMA

``` r
temp<-left_join(US,df[ which(df$Year>2011), ],by="State")
```

``` r
knitr::opts_chunk$set(echo = TRUE,fig.width = 12,fig.height = 12)
```

## Pattern of Homicide Rate with Gun Laws Rank

``` r
p0 <- ggplot(data = temp,
             mapping = aes(x = long, y = lat,
                           group = group, fill = GunLawRank))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_calc() +
    labs(title = "Gun Laws Rank through the years", fill = NULL)
p3<-p2 + theme_map()
p4<-p3+facet_wrap(temp$Year)
p5<-p4+theme(legend.position=c(.65, .24), legend.direction = "horizontal",legend.title = element_text(size = 18),legend.text = element_text(size = 16),legend.key.size = unit(0.5, "cm"),
  legend.key.width = unit(0.5,"cm"),legend.margin =margin(r=30,l=10,t=1,b=1)  )
p5
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
p0 <- ggplot(data = temp,
             mapping = aes(x = long, y = lat, group = group, fill = HomicideRate))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_gradient(low = "white", high = "Red3") +
        labs(title = "Homicide Rate through the years") 
p3<-p2 + theme_map()+facet_wrap(temp$Year)
p4<-p3+theme(legend.position=c(.65, .22), legend.direction = "horizontal",legend.title = element_text(size = 18),legend.text = element_text(size = 16),legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),legend.margin =margin(r=30,l=30,t=10,b=1)  )
p4
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
p0 <- ggplot(data = temp,
             mapping = aes(x = long, y = lat, group = group, fill = HomicideRate))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_gradient(low = "white", high = "Red3") +
        labs(title = "Average Homicide Rate in the last 8 years") 
p3<-p2 + theme_map()
p4<-p3+theme(legend.direction = "vertical",legend.position = c(.8, .2),legend.title=element_text(size=8),legend.text=element_text(size = 6))

p0 <- ggplot(data = temp,
             mapping = aes(x = long, y = lat, group = group, fill = GunLawStrictness))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_gradient(low = "white", high = "Darkorange") +
        labs(title = "Gun Law average strictness in last 8 years")+labs(fill="Strictness") 
p3<-p2 + theme_map()
p5<-p3+theme(legend.direction = "vertical",legend.position = c(.8, .2),legend.title=element_text(size=8),legend.text=element_text(size = 6))

cowplot::plot_grid(p4, p5)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot(df, aes(x = HomicideRate, y = State, group= State, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  ) +
  labs(title = 'Homicide Distribution by State') 
```

    ## Picking joint bandwidth of 0.423

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
knitr::opts_chunk$set(echo = TRUE,fig.width = 8,fig.height = 5.8)
```

## Exploratory Data Analysis

``` r
temp<-df[ which(df$Year>2011), ]
ggplot(data=df[ which(df$Year>2011), ], aes(x=HomicideRate,fill=GunLawRank)) +
  geom_density(alpha=.6, na.rm=T) +
  labs(title = "Distribution of Homicide Rate by Gun Laws Rank")+
  facet_wrap(~GunLawRank)+
  theme_bw()+
  scale_fill_calc()
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
df %>%
  group_by(GunLawRank) %>%
  ggplot()+
  geom_boxplot(aes(x=GunLawRank,y=HomicideRate,fill="coral"))+
  guides(fill=FALSE)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
df %>%
  group_by(Region) %>%
  ggplot()+
  geom_boxplot(aes(x=Region,y=HomicideRate,fill="coral"))+
  guides(fill=FALSE)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
df %>%
  group_by(StateCode) %>%
  ggplot()+
  geom_boxplot(aes(x=StateCode,y=HomicideRate,fill="coral"))+
  guides(fill=FALSE)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
df %>%
  group_by(CannabisMedical) %>%
  ggplot()+
  geom_boxplot(aes(x=CannabisMedical,y=HomicideRate,fill="coral"))+
  guides(fill=FALSE)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

``` r
df %>%
  group_by(CannabisRecreational) %>%
  ggplot()+
  geom_boxplot(aes(x=CannabisRecreational,y=HomicideRate,fill="coral"))+
  guides(fill=FALSE)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-5.png)<!-- -->

``` r
ggplot(data=df, aes(x=HomicideRate)) +
  geom_histogram(aes(y=..density..), col='black', fill='white') +
  geom_density(alpha=.6, fill="seagreen") +
  labs(title = "Distribution of Homicides: All States & Years")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-6.png)<!-- -->

``` r
ggplot(data=df, aes(x=HomicideRate, group = CannabisMedical, fill = CannabisMedical)) +
  geom_density(alpha=.6, na.rm=T) +
  labs(title = "Distribution of Homicides: All by Medical Cannabis")
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-7.png)<!-- -->

``` r
ggplot(data=df, aes(x=HomicideRate, group = CannabisRecreational, fill = CannabisRecreational)) +
  geom_density(alpha=.6, na.rm=T) +
  labs(title = "Distribution of Homicide Rate by Recreational Cannabis")
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-8.png)<!-- -->

``` r
ggplot(data=df, aes(x=HomicideRate, group = GunLawRank, fill = GunLawRank)) +
  geom_density(alpha=.6, na.rm=T) +
  labs(title = "Distribution of Homicide Rate by Gun Laws Rank")
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-9.png)<!-- -->

``` r
ggplot(df, aes(x=Year, y=HomicideRate)) +
  geom_point() +
  geom_smooth()+
  labs(title="Relationship Log Beds vs. NumHomicides/Population")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-10.png)<!-- -->

``` r
df2<-df %>% 
  group_by(Year) %>%                        
  summarise(MeanHomicide = mean(HomicideRate))  
df2$Year<-as.factor(df2$Year)
df2 %>%
  ggplot( aes(x=Year, y=MeanHomicide)) +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    geom_line( color="grey") +
    theme_ipsum() +
    ggtitle("Trend in Average Homicide Rates")
```

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x,
    ## x$y, : font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-17-11.png)<!-- -->

``` r
knitr::opts_chunk$set(echo = TRUE,fig.width = 10,fig.height = 5)
```

``` r
x1<-ggplot(df, aes(x=HomicideRate)) + geom_histogram(color="gold",fill="seagreen")+ggtitle("Histogram of Homicide Rate")+xlab("Homicide Rate")

x2<-ggplot(df, aes(x=log(HomicideRate))) + geom_histogram(color="gold",fill="seagreen")+ggtitle("Histogram of Log Homicide Rate")+xlab("Log of Homicide Rate")

cowplot::plot_grid(x1, x2)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
knitr::opts_chunk$set(echo = TRUE,fig.width = 8,fig.height = 5.8)
```

``` r
reg1<-lm(formula = log(HomicideRate)~MentalIllness+GDP+LawOfficers+LawCover+AlcoholUsers+DrugUsers+`Big Cities`+Education+GunLawRank+Black+GiniIndex+Year+UnenemploymentRate+TotalWeapons,data = df)

summary(reg1)
```

    ## 
    ## Call:
    ## lm(formula = log(HomicideRate) ~ MentalIllness + GDP + LawOfficers + 
    ##     LawCover + AlcoholUsers + DrugUsers + `Big Cities` + Education + 
    ##     GunLawRank + Black + GiniIndex + Year + UnenemploymentRate + 
    ##     TotalWeapons, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.80357 -0.23156  0.01481  0.24312  1.20193 
    ## 
    ## Coefficients:
    ##                           Estimate      Std. Error t value
    ## (Intercept)        -239.1732781244   31.6241161935  -7.563
    ## MentalIllness        -0.0047219584    0.0049913082  -0.946
    ## GDP                   0.0000434643    0.0026603794   0.016
    ## LawOfficers           0.0110990349    0.0039543661   2.807
    ## LawCover             -0.0008739186    0.0004511267  -1.937
    ## AlcoholUsers          0.0025270908    0.0014624419   1.728
    ## DrugUsers            -0.0010258650    0.0065904336  -0.156
    ## `Big Cities`          0.0458263707    0.0259587971   1.765
    ## Education            -0.0483459534    0.0092404756  -5.232
    ## GunLawRankB          -0.0057621687    0.0924486072  -0.062
    ## GunLawRankC           0.1217242375    0.0985108022   1.236
    ## GunLawRankD           0.1369830616    0.1019335725   1.344
    ## GunLawRankF           0.1644412869    0.0981915203   1.675
    ## GunLawRankX           0.1052919037    0.1475024818   0.714
    ## Black                 0.0260707052    0.0025983850  10.033
    ## GiniIndex             0.2037961314    1.4121541095   0.144
    ## Year                  0.1209180744    0.0157074159   7.698
    ## UnenemploymentRate    0.0941566977    0.0184744123   5.097
    ## TotalWeapons          0.0000001705    0.0000003298   0.517
    ##                                Pr(>|t|)    
    ## (Intercept)           0.000000000000394 ***
    ## MentalIllness                    0.3448    
    ## GDP                              0.9870    
    ## LawOfficers                      0.0053 ** 
    ## LawCover                         0.0536 .  
    ## AlcoholUsers                     0.0849 .  
    ## DrugUsers                        0.8764    
    ## `Big Cities`                     0.0784 .  
    ## Education             0.000000298741429 ***
    ## GunLawRankB                      0.9503    
    ## GunLawRankC                      0.2175    
    ## GunLawRankD                      0.1799    
    ## GunLawRankF                      0.0949 .  
    ## GunLawRankX                      0.4758    
    ## Black              < 0.0000000000000002 ***
    ## GiniIndex                        0.8853    
    ## Year                  0.000000000000162 ***
    ## UnenemploymentRate    0.000000583476946 ***
    ## TotalWeapons                     0.6054    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3354 on 330 degrees of freedom
    ##   (151 observations deleted due to missingness)
    ## Multiple R-squared:  0.6297, Adjusted R-squared:  0.6095 
    ## F-statistic: 31.18 on 18 and 330 DF,  p-value: < 0.00000000000000022

``` r
plot(reg1)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-21-3.png)<!-- -->![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-21-4.png)<!-- -->

``` r
#Checking for linearity
#plot(reg1$fitted.values,dff$HomicideRate,pch=1,main="Predicted Values vs Actual Price",ylab = "Predicted Price Sold",xlab = "Actual Price Sold")
#abline(0,1,col="red3",lwd=3)

#checking for normality
qqnorm(reg1$residuals,pch=20,main="Checking for Normality Plot")
qqline(reg1$residuals,lwd=3,col="red3")
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-21-5.png)<!-- -->

``` r
#Checking for equality of variances
#plot(df$HomicideRate,rstandard(reg1),pch=20,main="Equality of Variances")
#abline(0,0,col="red",lwd=3)

#Checking for leverage points
lev=hat(model.matrix(reg1))
plot(lev,pch=20,ylim=c(0,.3),main="Leverage Points")
abline(3*mean(lev),0,col="red",lwd=3)
```

![](Hammad-Homicide_files/figure-gfm/unnamed-chunk-21-6.png)<!-- -->

``` r
colSums(is.na(df))
```

    ##                     State                 StateCode 
    ##                         0                         0 
    ##                      Year              NumHomicides 
    ##                         0                         0 
    ##              NumDrugUsers           NumAlcoholUsers 
    ##                        50                         0 
    ##    LawEnforcementOfficers                Population 
    ##                         2                         0 
    ##                   RealGDP          RealGDPperCapita 
    ##                         0                         0 
    ##                 GiniIndex      SeriousMentalIllness 
    ##                         0                        50 
    ##        UnenemploymentRate         ViolentCrime...14 
    ##                         0                         0 
    ##        PropertyCrime...15 SubAbuseInpatientCareBeds 
    ##                         0                       150 
    ##           CannabisMedical      CannabisRecreational 
    ##                         0                         0 
    ##                GunLawRank                     Black 
    ##                         0                         0 
    ##                  Hispanic                      Area 
    ##                         0                         0 
    ##              Adults 19-25              Adults 26-34 
    ##                         0                         0 
    ##                Big Cities                 Education 
    ##                         0                        50 
    ##                    Region           AnyOtherWeapon1 
    ##                         0                        50 
    ##        DestructiveDevice2               Machinegun3 
    ##                        50                        50 
    ##                 Silencer4       ShortBarreledRifle5 
    ##                        50                        50 
    ##     ShortBarreledShotgun6              TotalWeapons 
    ##                        50                        50 
    ##        MurderNSlaugtherVC                    RapeVC 
    ##                         0                         0 
    ##                 RobberyVC       AggravatedAssaultVC 
    ##                         0                         0 
    ##         ViolentCrime...39        PropertyCrime...40 
    ##                         0                         0 
    ##          GunLawStrictness              HomicideRate 
    ##                       100                         0 
    ##                 DrugUsers              AlcoholUsers 
    ##                        50                         0 
    ##               LawOfficers                  LawCover 
    ##                         2                         2 
    ##                       GDP             MentalIllness 
    ##                         0                        50

``` r
which(df$HomicideRate<1)
```

    ## [1] 229

``` r
reg2<-lmer(formula = log(HomicideRate)~MentalIllness+GDP+LawOfficers+LawCover+AlcoholUsers+DrugUsers+df$`Big Cities`+Education+GunLawRank+Black+GiniIndex+(1|State)+Year+UnenemploymentRate,data = df)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(reg2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## log(HomicideRate) ~ MentalIllness + GDP + LawOfficers + LawCover +  
    ##     AlcoholUsers + DrugUsers + df$`Big Cities` + Education +  
    ##     GunLawRank + Black + GiniIndex + (1 | State) + Year + UnenemploymentRate
    ##    Data: df
    ## 
    ## REML criterion at convergence: 50
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1334 -0.5419  0.0154  0.5320  4.3268 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  State    (Intercept) 0.11930  0.3454  
    ##  Residual             0.03134  0.1770  
    ## Number of obs: 349, groups:  State, 50
    ## 
    ## Fixed effects:
    ##                       Estimate  Std. Error t value
    ## (Intercept)        -96.2575220  23.0733229  -4.172
    ## MentalIllness       -0.0016637   0.0039143  -0.425
    ## GDP                 -0.0064505   0.0040818  -1.580
    ## LawOfficers          0.0037116   0.0030356   1.223
    ## LawCover            -0.0011727   0.0007373  -1.591
    ## AlcoholUsers         0.0041592   0.0008261   5.035
    ## DrugUsers            0.0031292   0.0048805   0.641
    ## df$`Big Cities`      0.0702076   0.0506263   1.387
    ## Education           -0.0013585   0.0099305  -0.137
    ## GunLawRankB          0.0367695   0.0671626   0.547
    ## GunLawRankC          0.1994800   0.0788558   2.530
    ## GunLawRankD          0.1750678   0.0793570   2.206
    ## GunLawRankF          0.1395672   0.0750588   1.859
    ## GunLawRankX          0.0852228   0.0919870   0.926
    ## Black                0.0295965   0.0061173   4.838
    ## GiniIndex            5.5987381   2.9729483   1.883
    ## Year                 0.0470159   0.0117972   3.985
    ## UnenemploymentRate   0.0240449   0.0147845   1.626

    ## 
    ## Correlation matrix not shown by default, as p = 18 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
AIC(reg1)
```

    ## [1] 248.4539

``` r
AIC(reg2)
```

    ## [1] 90.01046

``` r
reg3<-lm(formula = HomicideRate~MentalIllness+GDP+LawOfficers+LawCover+AlcoholUsers+DrugUsers+`Big Cities`+Education+GunLawRank+Black+GiniIndex+Year+Region,data = Y18,na.action = na.exclude)
summary(reg3)
```

    ## 
    ## Call:
    ## lm(formula = HomicideRate ~ MentalIllness + GDP + LawOfficers + 
    ##     LawCover + AlcoholUsers + DrugUsers + `Big Cities` + Education + 
    ##     GunLawRank + Black + GiniIndex + Year + Region, data = Y18, 
    ##     na.action = na.exclude)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6975 -1.2010 -0.0859  0.7858  4.9249 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                  Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)     -6.814983  25.526081  -0.267  0.79125   
    ## MentalIllness    0.018481   0.098837   0.187  0.85289   
    ## GDP             -0.024881   0.052746  -0.472  0.64044   
    ## LawOfficers      0.194941   0.093074   2.094  0.04449 * 
    ## LawCover        -0.006438   0.008434  -0.763  0.45105   
    ## AlcoholUsers    -0.089637   0.066732  -1.343  0.18894   
    ## DrugUsers        0.218334   0.126399   1.727  0.09406 . 
    ## `Big Cities`     0.100954   0.385295   0.262  0.79504   
    ## Education        0.019863   0.204729   0.097  0.92334   
    ## GunLawRankB      0.358034   1.676314   0.214  0.83227   
    ## GunLawRankC      0.324346   1.939254   0.167  0.86826   
    ## GunLawRankD      0.162084   1.947833   0.083  0.93422   
    ## GunLawRankF      1.193395   2.032236   0.587  0.56130   
    ## Black            0.188825   0.054905   3.439  0.00169 **
    ## GiniIndex        8.605268  27.359466   0.315  0.75523   
    ## Year                   NA         NA      NA       NA   
    ## RegionNorthEast -2.192907   1.333525  -1.644  0.11019   
    ## RegionSouthEast -2.007416   1.308420  -1.534  0.13512   
    ## RegionSouthWest  1.015076   1.765186   0.575  0.56941   
    ## RegionWest       0.132524   1.178232   0.112  0.91117   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.098 on 31 degrees of freedom
    ## Multiple R-squared:  0.6049, Adjusted R-squared:  0.3755 
    ## F-statistic: 2.637 on 18 and 31 DF,  p-value: 0.008592
