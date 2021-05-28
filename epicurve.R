### Tracking the COVID epidemic in Brazil and globally

## load Brazil data - up-to-date file can be downloaded from https://brasil.io/dataset/covid19/files/

br <- read.csv("caso_full.csv", header = TRUE) ## file needs to be updated accordingly
br$date <- as.Date(br$date)


## replace funny characters - state capitals
br[br$city== "BelÃ©m", "city"] <- "Belem" # Belem, PA / ibge_code==1501402
br[br$city=="BrasÃ­lia", "city"] <- "Brasilia" #Brasilia, DF / ibge_code==5300108
br[br$city=="CuiabÃ¡", "city"] <- "Cuiaba" # Cuiaba, MT
br[br$city=="FlorianÃ³polis", "city"] <- "Florianopolis" # Florianopolis, SC / ibge_code==4205407
br[br$city=="GoiÃ¢nia", "city"] <- "Goiania" #Goiania, GO / ibge_code==5208707
br[br$city=="JoÃ£o Pessoa", "city"] <- "Joao Pessoa" #Joao Pessoa, PB / ibge_code==2507507
br[br$city=="MacapÃ¡", "city"] <- "Macapa" #Macapa, AP / ibge_code==1600303
br[br$city=="MaceiÃ³", "city"] <- "Maceio" #Maceio, AL / ibge_code==2704302
br[br$city=="SÃ£o LuÃ­s", "city"] <- "Sao Luis" #Sao Luis, MA / ibge_code==2111300
br[br$city=="SÃ£o Paulo", "city"] <- "Sao Paulo" #Sao Paulo, SP / ibge_code==3550308
br[br$city=="VitÃ³ria", "city"] <- "Vitoria" #Vitoria, ES / ibge_code==3205309

## mind that other municipalities may also contain funny characters
# tip: look them up with View(br), then copy and paste

#add regions
br$region <- ifelse(br$state=="RS" | br$state=="SC" | br$state=="PR", "S",
                    ifelse(br$state=="SP" | br$state=="RJ" | br$state=="MG" | br$state=="ES", "SE", 
                           ifelse(br$state=="PA" | br$state=="RR" | br$state=="AP" | 
                                    br$state=="AM" | br$state=="TO" | br$state=="AC" | br$state=="RO", "N",
                                  ifelse(br$state=="SE" |br$state=="CE" | br$state=="PB" | br$state =="AL" |
                                           br$state=="RN" | br$state=="PE" | br$state=="BA" | br$state=="MA" | br$state=="PI", "NE",
                                         ifelse(br$state=="DF" | br$state=="MS" | br$state=="MT" | br$state=="GO", "W", NA)))))

st <- subset(br, place_type=="state") ## state-only data, no municipalities

## load world data

world <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
world$date <- as.Date(world$date)

ld <- as.Date(Sys.Date()-1) ## ld = last date



library(dplyr)

#### FUNCTIONS ###

covid <- function(place, place_type, initial_date, end_date) {
  if(place_type==1){uf <- subset(br, br$place_type=="city" & (br$city==place | br$city_ibge_code==place) & (br$date>=initial_date & br$date<=end_date))
  pop <- br$estimated_population[(br$city==place | br$city_ibge_code==place) & br$is_last=="True"]
  pop <- unique(pop[!is.na(pop)])
  place.label <- unique(uf$city)}
  else if(place_type==2){uf <- subset(st, st$state==place & st$date>=initial_date & st$date<=end_date)
  pop <- st$estimated_population[st$state==place & st$is_last=="True"]
  place.label <- unique(uf$state)}
  else if(place_type==3){uf <- subset(world, world$iso_code==place & world$date>=initial_date & world$date<=end_date)
  pop <- unique(world$population[world$iso_code==place])
  place.label <- unique(uf$location)}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  

  ### Incidence
  # Total cases
  if(place_type==1 | place_type==2){inc <- uf$last_available_confirmed}
  else if(place_type==3){inc <- uf$total_cases}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  
  cmm <- (lag(inc, 1)-lag(inc,8))/7 
  cmm <- cmm[!is.na(cmm)]
  inc.title <- c("Incidence (7-day rolling average) from", initial_date)
  plot.cases <- plot(cmm, type="l", col="darkblue", xlab= "Days elapsed since initial date + 7 days", ylab="Cases", main=inc.title, sub=place.label)+abline(h=median(cmm), col="grey30")+abline(h=median(tail(cmm)), col="darkred", lty="dashed")+abline(h=median(head(cmm)), col="grey40", lty="dashed")
  
  
    # Incidence rate
  if(place_type==1 | place_type==2){incr <- uf$last_available_confirmed/uf$estimated_population*10^5}
  else if(place_type==3){incr <- uf$total_cases/uf$population*10^5}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
 

  cmmr <- (lag(incr, 1)-lag(incr,8))/7
  cmmr <- cmmr[!is.na(cmmr)]
  incr.title <- c("Incidence rate (7-day rolling average) from", initial_date)
  plot.incr <- plot(cmmr, type="l", col="darkblue",  xlab= "Days elapsed since initial date + 7 days", ylab="Cases per 100K", main=incr.title, sub=place.label)
  
  # Change in incidence
  initial.inc <- round(median(head(cmm)),0)
  median.inc <- round(median(cmm),0)
  current.inc <- round(median(tail(cmm)),0)
  
  initial.incr <- round(median(head(cmmr)),1)
  median.incr <- round(median(cmmr),1)
  current.incr <- round(median(tail(cmmr)),1)
  
  cchange <- cmm/lag(cmm, 14)
  cchange <- cchange[!is.na(cchange)]
  
  
  cchange.title <- c("Change in incidence over the previous 2 weeks", "- period analyzed starts on", initial_date) ## 
  
  plot.cchange.log <- plot(log2(cchange), type="l", col="blue", xlab= "Days elapsed since 14th day from initial date", ylab="Change rate (log2)", main=cchange.title, sub=place.label)+abline(h=0, col="grey30")+abline(h=c(-0.15,.15), lty="dashed", col="grey10")
 
   ### Mortality
  
  # Total deaths - daily average
  if(place_type==1 | place_type==2){mort <- uf$last_available_deaths}
  else if(place_type==3){mort <- uf$total_deaths}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  mmm <- (lag(mort,1)-lag(mort,8))/7
  mmm <- mmm[!is.na(mmm)]
  mort.title <- c("Mortality (7-day rolling average) from", initial_date)
  plot.deaths <- plot(mmm, type="l", col="red", xlab= "Days elapsed since initial date + 7 days", ylab="Deaths", main=mort.title, sub=place.label)
  
  # Mortality rate - daily average
  if(place_type==1 | place_type==2){mortr <- uf$last_available_deaths/uf$estimated_population*10^5}
  else if(place_type==3){mortr <- uf$total_deaths/uf$population*10^5}
  
  mmmr <- (lag(mortr, 1)-lag(mortr,8))/7
  mmmr <- mmmr[!is.na(mmmr)]
  mortr.title <- c("Mortality rate (7-day rolling average) from", initial_date)
  plot.mortr <- plot(mmmr, type="l", col="red", xlab= "Days elapsed since initial date + 7 days", ylab="Deaths per 100K", main=mortr.title, sub=place.label)
  
  # Change in mortality - daily average
  initial.mort <- round(median(head(mmm)),0)
  median.mort <- round(median(mmm),0)
  current.mort <- round(median(tail(mmm)),0)
  
  initial.mortr <- round(median(head(mmmr)),1)
  median.mortr <- round(median(mmmr),1)
  current.mortr <- round(median(tail(mmmr)),1)
  
  dchange <- mmmr/lag(mmmr, 14)
  dchange <- dchange[!is.na(dchange)]
  
  dchange.title <- c("Change in mortality over the previous 2 weeks", " - period analyzed starts on", initial_date)
  
  plot.dchange.log <- plot(log2(dchange), type="l", col="darkred", xlab= "Days elapsed since 14th day from initial date", ylab="Change rate (log2)", main=cchange.title, sub=place.label)+abline(h=0, col="grey30")+abline(h=c(-0.15,.15), lty="dashed", col="grey10")
  
  initial.cfr <- round(median(head(mmmr/cmmr*100)),1)
  median.cfr <- round(median(mmmr/cmmr*100),1)
  current.cfr <- round(median(tail(mmmr/cmmr*100)),1)
  
  
  summary.cov <- list("Place"=place.label, "Period"=c(unique(first(uf$date)), unique(last(uf$date))),  "Population"=pop, "Daily cases (7-day average - initial, median, current)"=c(initial.inc, median.inc, current.inc), "Incidence rate (daily, per 100K - initial, median, current)" = c(initial.incr, median.incr, current.incr), "Daily deaths (7-day average - initial, median, end)"=c(initial.mort, median.mort, current.mort), "Mortality rate (daily, per 100K - initial, median, end)" = c(initial.mortr, median.mortr, current.mortr), "Peak incidence and mortality"=c(round(max(cmm)), round(max(mmm))), "CFR (% - initial, median, end)" = c(initial.cfr, median.cfr, current.cfr))
  
  return(c(plot.cases, plot.incr, plot.cchange.log, plot.deaths, plot.mortr, plot.dchange.log, summary.cov))
}


covid.summary <- function(place, place_type, initial_date, end_date) {
  if(place_type==1){uf <- subset(br, br$place_type=="city" & (br$city==place | br$city_ibge_code==place) & (br$date>=initial_date & br$date<=end_date))
  pop <- br$estimated_population[(br$city==place | br$city_ibge_code==place) & br$is_last=="True"]
  pop <- unique(pop[!is.na(pop)])
  place.label <- unique(uf$city)}
  else if(place_type==2){uf <- subset(st, st$state==place & st$date>=initial_date & st$date<=end_date)
  pop <- st$estimated_population[st$state==place & st$is_last=="True"]
  place.label <- unique(uf$state)}
  else if(place_type==3){uf <- subset(world, world$iso_code==place & world$date>=initial_date & world$date<=end_date)
  pop <- unique(world$population[world$iso_code==place])
  place.label <- unique(uf$location)}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  
  ### Incidence
  # Total cases
  if(place_type==1 | place_type==2){inc <- uf$last_available_confirmed}
  else if(place_type==3){inc <- uf$total_cases}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  
  cmm <- (lag(inc, 1)-lag(inc,8))/7 
  cmm <- cmm[!is.na(cmm)]
  inc.title <- c("Incidence (7-day rolling average) from", initial_date)
  plot.cases <- plot(cmm, type="l", col="darkblue", xlab= "Days elapsed since initial date + 7 days", ylab="Cases", main=inc.title, sub=place.label)+abline(h=median(cmm), col="grey30")+abline(h=median(tail(cmm)), col="darkred", lty="dashed")+abline(h=median(head(cmm)), col="grey40", lty="dashed")
  
  
  # Incidence rate
  if(place_type==1 | place_type==2){incr <- uf$last_available_confirmed/uf$estimated_population*10^5}
  else if(place_type==3){incr <- uf$total_cases/uf$population*10^5}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  
  cmmr <- (lag(incr, 1)-lag(incr,8))/7
  cmmr <- cmmr[!is.na(cmmr)]
 
  # Change in incidence
  initial.inc <- round(median(head(cmm)),0)
  median.inc <- round(median(cmm),0)
  current.inc <- round(median(tail(cmm)),0)
  
  initial.incr <- round(median(head(cmmr)),1)
  median.incr <- round(median(cmmr),1)
  current.incr <- round(median(tail(cmmr)),1)
  
  
  ### Mortality
  
  # Total deaths - daily average
  if(place_type==1 | place_type==2){mort <- uf$last_available_deaths}
  else if(place_type==3){mort <- uf$total_deaths}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  mmm <- (lag(mort,1)-lag(mort,8))/7
  mmm <- mmm[!is.na(mmm)]
  mort.title <- c("Mortality (7-day rolling average) from", initial_date)
  plot.deaths <- plot(mmm, type="l", col="red", xlab= "Days elapsed since initial date + 7 days", ylab="Deaths", main=mort.title, sub=place.label)
  
  # Mortality rate - daily average
  if(place_type==1 | place_type==2){mortr <- uf$last_available_deaths/uf$estimated_population*10^5}
  else if(place_type==3){mortr <- uf$total_deaths/uf$population*10^5}
  
  mmmr <- (lag(mortr, 1)-lag(mortr,8))/7
  mmmr <- mmmr[!is.na(mmmr)]
  
  # Change in mortality - daily average
  initial.mort <- round(median(head(mmm)),0)
  median.mort <- round(median(mmm),0)
  current.mort <- round(median(tail(mmm)),0)
  
  initial.mortr <- round(median(head(mmmr)),1)
  median.mortr <- round(median(mmmr),1)
  current.mortr <- round(median(tail(mmmr)),1)
  
  initial.cfr <- round(median(head(mmmr/cmmr*100)),1)
  median.cfr <- round(median(mmmr/cmmr*100),1)
  current.cfr <- round(median(tail(mmmr/cmmr*100)),1)
  
  
  summary.cov <- list("Place"=place.label, "Period"=c(unique(first(uf$date)), unique(last(uf$date))),  "Population"=pop, "Daily cases (7-day average - initial, median, current)"=c(initial.inc, median.inc, current.inc), "Incidence rate (daily, per 100K - initial, median, current)" = c(initial.incr, median.incr, current.incr), "Daily deaths (7-day average - initial, median, end)"=c(initial.mort, median.mort, current.mort), "Mortality rate (daily, per 100K - initial, median, end)" = c(initial.mortr, median.mortr, current.mortr), "Peak incidence and mortality"=c(round(max(cmm)), round(max(mmm))), "CFR (% - initial, median, end)" = c(initial.cfr, median.cfr, current.cfr))
  
  return(summary.cov)
}


## Find municipalities by population

findbypop <- function(state, population){
  municipalities <- unique(br$city[br$state==state & br$estimated_population>=population])
  municipalities <- municipalities[!is.na(municipalities)]
  return(list(municipalities))
}

## Compare two places

compare <- function (place1, place_type1, place2, place_type2, initial_date){
  list(covid(place1, place_type1, initial_date), covid(place2, place_type2, initial_date))
}

## Epidemic profile by place

covid.profile <- function(place, place_type){
  if(place_type==1){uf <- subset(br, br$place_type=="city"& br$city==place | br$city_ibge_code==place)
  place.label <- unique(uf$city)
  pop <- last(uf$estimated_population) 
  date.first.case <- first(uf$date[uf$last_available_confirmed>=1])
  total.cases <- last(uf$last_available_confirmed)
  total.deaths <- last(uf$last_available_deaths)
  days.inc.100 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=100])-date.first.case
  days.inc.1000 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=1000])-date.first.case
  cases.last.15d <- total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-15]
  cases.last.30d <- total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-30]
  cases.last.60d <- total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-60]
  }
  else if(place_type==2){uf <- subset(st, st$state==place)
  place.label <- unique(uf$state)
  pop <- st$estimated_population[st$state==place & st$is_last=="True"]
  date.first.case <- first(uf$date[uf$last_available_confirmed>=1])
  total.cases <- last(uf$last_available_confirmed)
  total.deaths <- last(uf$last_available_deaths)
  days.inc.100 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=100])-date.first.case
  days.inc.1000 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=1000])-date.first.case
  cases.last.15d <- total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-15]
  cases.last.30d <- total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-30]
  cases.last.60d <- total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-60]
  }
  else if(place_type==3){uf <- subset(world, world$iso_code==place & world$total_cases!=is.na(world$total_cases))
  place.label <- unique(uf$location)
  pop <- unique(world$population[world$iso_code==place])
  date.first.case <- first(uf$date[uf$total_cases>=1])
  total.cases <- last(uf$total_cases)
  total.deaths <- last(uf$total_deaths)
  days.inc.100 <- first(uf$date[uf$total_cases/pop*10^5>=100])-date.first.case
  days.inc.1000 <- first(uf$date[uf$total_cases/pop*10^5>=1000])-date.first.case
  cases.last.15d <- total.cases-uf$total_cases[uf$date==last(uf$date)-15]
  cases.last.30d <- total.cases-uf$total_cases[uf$date==last(uf$date)-30]
  cases.last.60d <- total.cases-uf$total_cases[uf$date==last(uf$date)-60]
  
    }
  
  else(print("place_type should be 1, 2 or 3. Please check your command"))

  cumulative.inc <- round(total.cases/pop*10^5, 1)
  cumulative.mort <- round(total.deaths/pop*10^5, 1)
  CFR <- round(total.deaths/total.cases*100, 1)
  
  perc.15days <- cases.last.15d/total.cases
  perc.30days <- cases.last.30d/total.cases
  perc.60days <- cases.last.60d/total.cases
  change.15.30 <- cases.last.15d/cases.last.30d
  change.30.60 <- cases.last.30d/cases.last.60d
  
  days_elapsed <- as.integer(last(uf$date)-date.first.case) ## cannot be a difftime object
  
 
  expected.60d.90 <- prop.test(60, days_elapsed, conf.level = 0.9)
  
  
  
  
  flag.60d <- ifelse(perc.60days>=(expected.60d.90$conf.int[1]) & perc.60days<(expected.60d.90$conf.int[2]), "CASES WITHIN EXPECTED RANGE",
                        ifelse(perc.60days>=expected.60d.90$conf.int[2], "MORE CASES THAN EXPECTED",
                               ifelse(perc.60days<expected.60d.90$conf.int[1], "FEWER CASES THAN EXPECTED", "ERROR")))
  
  
  flag.15.30 <- ifelse(change.15.30>=0.425 & change.15.30<0.575, "STABLE",
                          ifelse(change.15.30>=0.575 & change.15.30<.7875, "UPWARD",
                                 ifelse(change.15.30>=.7875, "UPWARD+",
                                 ifelse(change.15.30<0.425 & change.15.30>.2125, "DOWNWARD",
                                        ifelse(change.15.30<=.2125, "DOWNWARD+", "ERROR")))))
 
  
  flag.30.60 <- ifelse(change.30.60>=0.45 & change.30.60<0.55, "STABLE",
                          ifelse(change.30.60>=0.55 & change.30.60<.775, "UPWARD",
                                 ifelse(change.30.60>=.775, "UPWARD+",
                                 ifelse(change.30.60<0.45 & change.30.60>.225, "DOWNWARD",
                                        ifelse(change.30.60<=.225, "DOWNWARD+", "ERROR")))))
  
   
   
  
  perc.15days.round <- round(perc.15days*100,1)
  perc.30days.round <- round(perc.30days*100,1)
  perc.60days.round <- round(perc.60days*100,1)
  
  

  return(list("Place"=place.label, "Population"=pop, "Total cases & deaths"=c(total.cases, total.deaths), "Cumulative incidence (per 100K) & mortality (per 100K) & CFR (%)"=c(cumulative.inc, cumulative.mort, CFR), "Date first case reported"=date.first.case, "Days to 100 & 1000 cases per 100K (0.1% & 1% of pop)"=c(days.inc.100, days.inc.1000), "% of cases in the last 15, 30 & 60 days"=c(perc.15days.round, perc.30days.round, perc.60days.round), "Trends"=list("Last 60 days as compared to the entire period of the epidemic (90% confidence)"=c(flag.60d), "Alerts"=list("Last 15 & 30 days"= c(flag.15.30, flag.30.60)))))
}

## covid - all states in N and NE
# change initial_date
for (x in unique(st$state[st$region==c("N", "NE")])){print(c(covid(x,2, "2021-04-10", ld)))}
