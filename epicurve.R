### Tracking the COVID epidemic in Brazil and globally

## load Brazil data - up-to-date file can be downloaded from https://brasil.io/dataset/covid19/files/

br <- read.csv("BR0424.csv", header = TRUE)
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


st <- subset(br, place_type=="state") ## state-only data, no municipalities

## load world data

world <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
world$date <- as.Date(world$date)

#### FUNCTIONS ###

covid <- function(place, place_type, initial_date) {
  if(place_type==1){uf <- subset(br, br$place_type=="city" & (br$city==place | br$city_ibge_code==place) & br$date>=initial_date)
  pop <- br$estimated_population[(br$city==place | br$city_ibge_code==place) & br$is_last=="True"]
  pop <- unique(pop[!is.na(pop)])
  place.label <- unique(uf$city)}
  else if(place_type==2){uf <- subset(st, st$state==place & st$date>=initial_date)
  pop <- st$estimated_population[st$state==place & st$is_last=="True"]
  place.label <- unique(uf$state)}
  else if(place_type==3){uf <- subset(world, world$iso_code==place & world$date>=initial_date)
  pop <- unique(world$population[world$iso_code==place])
  place.label <- unique(uf$location)}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  

  ### Incidence
  # Total cases
  if(place_type==1 | place_type==2){inc <- uf$last_available_confirmed}
  else if(place_type==3){inc <- uf$total_cases}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  cmm <- (dplyr::lag(inc, 1)-dplyr::lag(inc,8))/7
  cmm <- cmm[!is.na(cmm)]
  inc.title <- c("Incidence (7-day rolling average) from", initial_date)
  plot.cases <- plot(cmm, type="l", col="darkblue",  xlab= "Days elapsed since initial date", ylab="Cases", main=inc.title, sub=place.label)
  
  
    # Incidence rate
  if(place_type==1 | place_type==2){incr <- uf$last_available_confirmed/uf$estimated_population*10^5}
  else if(place_type==3){incr <- uf$total_cases/uf$population*10^5}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
 

  cmmr <- (dplyr::lag(incr, 1)-dplyr::lag(incr,8))/7
  cmmr <- cmmr[!is.na(cmmr)]
  incr.title <- c("Incidence rate (7-day rolling average) from", initial_date)
  plot.incr <- plot(cmmr, type="l", col="darkblue",  xlab= "Days elapsed since initial date", ylab="Cases per 100K", main=incr.title, sub=place.label)
  
  # Change in incidence
  initial.incr <- round(median(head(cmmr)),1)
  current.incr <- round(median(tail(cmmr)),1)
  
  cchange <- dplyr::lag(cmm, 2)/dplyr::lag(cmm, 16)
  cchange <- cchange[!is.na(cchange)]
  
  cchange.title <- c("Change in incidence over the previous 2 weeks", "- period analyzed starts on", initial_date) ## 
  plot.cchange <- plot(cchange, type="l", col="blue", xlab= "Days elapsed since 14th day from initial date", ylab="Change rate", main=cchange.title, sub=place.label)+abline(h=1, col="grey30")+abline(h=.9, lty="dashed", col="grey10")+abline(h=1.1, lty="dashed", col="grey10")
  
  ### Mortality
  
  # Total deaths - daily average
  if(place_type==1 | place_type==2){mort <- uf$last_available_deaths}
  else if(place_type==3){mort <- uf$total_deaths}
  else(print("place_type should be 1, 2 or 3. Please check your command"))
  
  mmm <- (dplyr::lag(mort,1)-dplyr::lag(mort,8))/7
  mmm <- mmm[!is.na(mmm)]
  mort.title <- c("Mortality (7-day rolling average) from", initial_date)
  plot.deaths <- plot(mmm, type="l", col="red", xlab= "Days elapsed since initial date", ylab="Deaths", main=mort.title, sub=place.label)
  
  # Mortality rate - daily average
  if(place_type==1 | place_type==2){mortr <- uf$last_available_deaths/uf$estimated_population*10^5}
  else if(place_type==3){mortr <- uf$total_deaths/uf$population*10^5}
  
  mmmr <- (dplyr::lag(mortr, 1)-dplyr::lag(mortr,8))/7
  mmmr <- mmmr[!is.na(mmmr)]
  mortr.title <- c("Mortality rate (7-day rolling average) from", initial_date)
  plot.mortr <- plot(mmmr, type="l", col="red", xlab= "Days elapsed since initial date", ylab="Deaths per 100K", main=mortr.title, sub=place.label)
  
  # Change in mortality - daily average
  initial.mortr <- round(median(head(mmmr)),1)
  current.mortr <- round(median(tail(mmmr)),1)
  
  dchange <- dplyr::lag(mmm, 2)/dplyr::lag(mmm, 16)
  dchange <- dchange[!is.na(dchange)]
  
  dchange.title <- c("Change in mortality over the previous 2 weeks", " - period analyzed starts on", initial_date)
  plot.dchange <- plot(dchange, type="l", col="darkred", xlab= "Days elapsed since 14th day from initial date", ylab="Change rate", main=dchange.title, sub=place.label)+abline(h=1, col="grey30")+abline(h=.9, col="grey10", lty="dashed")+abline(h=1.1, col="grey10", lty="dashed")
  
  
  initial.cfr <- round(median(head(mmm/cmm*100)),1)
  current.cfr <- round(median(tail(mmm/cmm*100)),1)
  
  summary.cov <- list("Population"=pop, "Incidence rate (daily, per 100K)" = c(initial.incr, current.incr), "Mortality rate (daily, per 100K)" = c(initial.mortr, current.mortr), "CFR (%)" = c(initial.cfr, current.cfr))
  
  return(c(plot.cases, plot.incr, plot.deaths, plot.mortr, summary.cov))
}


## Find municipalities by population

findbypop <- function(state, population){
  municipalities <- unique(br$city[br$state==state & br$estimated_population>=population])
  municipalities <- municipalities[!is.na(municipalities)]
  return(municipalities)
}

## Compare two places

compare <- function (place1, place_type1, place2, place_type2, initial_date){
  list(covid(place1, place_type1, initial_date), covid(place2, place_type2, initial_date))
}

## Epidemic profile by place

covid.profile <- function(place, place_type){
  if(place_type==1){uf <- subset(br, br$place_type=="city"& br$city==place | br$city_ibge_code==place)
  pop <- last(uf$estimated_population) 
  date.first.case <- first(uf$date[uf$last_available_confirmed>=1])
  total.cases <- last(uf$last_available_confirmed)
  total.deaths <- last(uf$last_available_deaths)
  days.inc.100 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=100])-date.first.case
  days.inc.1000 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=1000])-date.first.case
  perc.15days <- round((total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-15])/total.cases*100, 1)
  perc.30days <- round((total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-30])/total.cases*100, 1)
  perc.60days <- round((total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-60])/total.cases*100, 1)
  }
  else if(place_type==2){uf <- subset(st, st$state==place)
  pop <- st$estimated_population[st$state==place & st$is_last=="True"]
  date.first.case <- first(uf$date[uf$last_available_confirmed>=1])
  total.cases <- last(uf$last_available_confirmed)
  total.deaths <- last(uf$last_available_deaths)
  days.inc.100 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=100])-date.first.case
  days.inc.1000 <- first(uf$date[uf$last_available_confirmed/pop*10^5>=1000])-date.first.case
  perc.15days <- round((total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-15])/total.cases*100, 1)
  perc.30days <- round((total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-30])/total.cases*100, 1)
  perc.60days <- round((total.cases-uf$last_available_confirmed[uf$date==last(uf$date)-60])/total.cases*100, 1)
  }
  else if(place_type==3){uf <- subset(world, world$iso_code==place & world$total_cases!=is.na(world$total_cases))
  pop <- unique(world$population[world$iso_code==place])
  date.first.case <- first(uf$date[uf$total_cases>0])
  total.cases <- last(uf$total_cases)
  total.deaths <- last(uf$total_deaths)
  days.inc.100 <- first(uf$date[uf$total_cases/pop*10^5>=100])-date.first.case
  days.inc.1000 <- first(uf$date[uf$total_cases/pop*10^5>=1000])-date.first.case
  perc.15days <- round((total.cases-uf$total_cases[uf$date==last(uf$date)-15])/total.cases*100, 1)
  perc.30days <- round((total.cases-uf$total_cases[uf$date==last(uf$date)-30])/total.cases*100, 1)
  perc.60days <- round((total.cases-uf$total_cases[uf$date==last(uf$date)-60])/total.cases*100, 1)
  }
  
  else(print("place_type should be 1, 2 or 3. Please check your command"))

  cumulative.inc <- round(total.cases/pop*10^5, 1)
  cumulative.mort <- round(total.deaths/pop*10^5, 1)
  CFR <- round(total.deaths/total.cases*100, 1)
  
  
  
  return(list("Population"=pop, "Total cases & deaths"=c(total.cases, total.deaths), "Cumulative incidence (per 100K) & mortality (per 100K) & CFR (%)"=c(cumulative.inc, cumulative.mort, CFR), "Date first case reported"=date.first.case, "Days to 100 & 1000 cases per 100K (0.1% & 1% of pop)"=c(days.inc.100, days.inc.1000), "% of cases in the last 15, 30 & 60 days"=c(perc.15days, perc.30days, perc.60days)))
   }

