
br <- read.csv("BR0421.csv", header = TRUE)

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


br$region <- NA
br$region <- ifelse(br$state=="RS" | br$state=="SC" | br$state=="PR", "S",
                    ifelse(br$state=="SP" | br$state=="RJ" | br$state=="MG" | br$state=="ES", "SE", 
                           ifelse(br$state=="PA" | br$state=="RR" | br$state=="AP" | 
                                    br$state=="AM" | br$state=="TO" | br$state=="AC" | br$state=="RO", "N",
                                  ifelse(br$state=="SE" |br$state=="CE" | br$state=="PB" | br$state =="AL" |
                                           br$state=="RN" | br$state=="PE" | br$state=="BA" | br$state=="MA" | br$state=="PI", "NE",
                                         ifelse(br$state=="DF" | br$state=="MS" | br$state=="MT" | br$state=="GO", "W", NA)))))

st <- subset(br, place_type=="state") ## states subset
library(dplyr)


#### FUNCTIONS ###

cov.data <- function(x, place_type, y) {
  if(place_type==1){uf <- subset(st, st$state==x & st$epidemiological_week>=y)}
  else if(place_type==2){uf <- subset(br, br$place_type=="city" & br$city==x & br$epidemiological_week>=y)}
  else(print("ERROR"))
  
  
  ### Incidence
  # Total cases
  inc <- uf$last_available_confirmed
  cmm <- (dplyr::lag(inc, 3)-dplyr::lag(inc,10))/7
  cmm <- cmm[!is.na(cmm)]
  inc.title <- c("Incidence (7-day rolling average) from epi week", y)
  plot.cases <- plot(cmm, type="l", col="darkblue",  ylab="Cases", main=inc.title, sub=x)
  
  # Incidence rate
  incr <- uf$last_available_confirmed/uf$estimated_population*10^5
  cmmr <- (dplyr::lag(incr, 3)-dplyr::lag(incr,10))/7
  cmmr <- cmmr[!is.na(cmmr)]
  incr.title <- c("Incidence rate (7-day rolling average) from epi week", y)
  plot.incr <- plot(cmmr, type="l", col="darkblue",  ylab="Cases per 100K", main=incr.title, sub=x)
  
  # Change in incidence
  initial.incr <- round(median(head(cmmr)),1)
  current.incr <- round(median(tail(cmmr)),1)
  
  cchange <- cmm/dplyr::lag(cmm, 21)
  cchange <- cchange[!is.na(cchange)]
  
  cchange.title <- c("Change in incidence over the previous 3 weeks", "- earliest epi week", y)
  plot.cchange <- plot(cchange, type="l", col="blue", ylab="Change rate", main=cchange.title, sub=x)+abline(h=1, col="grey30")+abline(h=.9, lty="dashed", col="grey10")+abline(h=1.1, lty="dashed", col="grey10")
  
  ### Mortality
  
  # Total deaths - daily average
  mort <- uf$last_available_deaths
  mmm <- (dplyr::lag(mort, 3)-dplyr::lag(mort,10))/7
  mmm <- mmm[!is.na(mmm)]
  mort.title <- c("Mortality (7-day rolling average) from epi week", y)
  plot.deaths <- plot(mmm, type="l", col="red", ylab="Deaths", main=mort.title, sub=x)
  
  # Mortality rate - daily average
  mortr <- uf$last_available_deaths/uf$estimated_population*10^5
  mmmr <- (dplyr::lag(mortr, 3)-dplyr::lag(mortr,10))/7
  mmmr <- mmmr[!is.na(mmmr)]
  mortr.title <- c("Mortality rate (7-day rolling average) from epi week", y)
  plot.mortr <- plot(mmmr, type="l", col="red", ylab="Deaths per 100K", main=mortr.title, sub=x)
  
  # Change in mortality - daily average
  initial.mortr <- round(median(head(mmmr)),1)
  current.mortr <- round(median(tail(mmmr)),1)
  
  dchange <- mmm/dplyr::lag(mmm, 21)
  dchange <- dchange[!is.na(dchange)]
  
  dchange.title <- c("Change in mortality over the previous 3 weeks", " - earliest epi week", y)
  plot.dchange <- plot(dchange, type="l", col="darkred", ylab="Change rate", main=dchange.title, sub=x)+abline(h=1, col="grey30")+abline(h=.9, col="grey10", lty="dashed")+abline(h=1.1, col="grey10", lty="dashed")
  
  
  initial.cfr <- round(median(head(mmm/cmm*100)),1)
  current.cfr <- round(median(tail(mmm/cmm*100)),1)
  
  summary.cov <- list("Incidence rate (daily, per 100K)" = c(initial.incr, current.incr), "Mortality rate (daily, per 100K)" = c(initial.mortr, current.mortr), "CFR (%)" = c(initial.cfr, current.cfr))
  
  return(c(plot.cases, plot.incr, plot.deaths, plot.mortr, summary.cov))
}




# Subset by state - some municipalities have the same name in different states, so the code gives an error

cov.city <- function(s, x, y) { 
  
  uf <- subset(br, br$place_type=="city" & br$state==s & br$city==x & br$epidemiological_week>=y)
  
  ### Incidence
  # Total cases
  inc <- uf$last_available_confirmed
  cmm <- (dplyr::lag(inc, 3)-dplyr::lag(inc,10))/7
  cmm <- cmm[!is.na(cmm)]
  inc.title <- c("Incidence (7-day rolling average) from epi week", y)
  plot.cases <- plot(cmm, type="l", col="darkblue",  ylab="Cases", main=inc.title, sub=x)
  
  # Incidence rate
  incr <- uf$last_available_confirmed/uf$estimated_population*10^5
  cmmr <- (dplyr::lag(incr, 3)-dplyr::lag(incr,10))/7
  cmmr <- cmmr[!is.na(cmmr)]
  incr.title <- c("Incidence rate (7-day rolling average) from epi week", y)
  plot.incr <- plot(cmmr, type="l", col="darkblue",  ylab="Cases per 100K", main=incr.title, sub=x)
  
  # Change in incidence
  initial.incr <- round(median(head(cmmr)),1)
  current.incr <- round(median(tail(cmmr)),1)
  
  cchange <- cmm/dplyr::lag(cmm, 21)
  cchange <- cchange[!is.na(cchange)]
  
  cchange.title <- c("Change in incidence over the previous 3 weeks", "- earliest epi week", y)
  plot.cchange <- plot(cchange, type="l", col="blue", ylab="Change rate", main=cchange.title, sub=x)+abline(h=1, col="grey30")+abline(h=.9, lty="dashed", col="grey10")+abline(h=1.1, lty="dashed", col="grey10")
  
  ### Mortality
  
  # Total deaths - daily average
  mort <- uf$last_available_deaths
  mmm <- (dplyr::lag(mort, 3)-dplyr::lag(mort,10))/7
  mmm <- mmm[!is.na(mmm)]
  mort.title <- c("Mortality (7-day rolling average) from epi week", y)
  plot.deaths <- plot(mmm, type="l", col="red", ylab="Deaths", main=mort.title, sub=x)
  
  # Mortality rate - daily average
  mortr <- uf$last_available_deaths/uf$estimated_population*10^5
  mmmr <- (dplyr::lag(mortr, 3)-dplyr::lag(mortr,10))/7
  mmmr <- mmmr[!is.na(mmmr)]
  mortr.title <- c("Mortality rate (7-day rolling average) from epi week", y)
  plot.mortr <- plot(mmmr, type="l", col="red", ylab="Deaths per 100K", main=mortr.title, sub=x)
  
  # Change in mortality - daily average
  initial.mortr <- round(median(head(mmmr)),1)
  current.mortr <- round(median(tail(mmmr)),1)
  
  dchange <- mmm/dplyr::lag(mmm, 21)
  dchange <- dchange[!is.na(dchange)]
  
  dchange.title <- c("Change in mortality over the previous 3 weeks", " - earliest epi week", y)
  plot.dchange <- plot(dchange, type="l", col="darkred", ylab="Change rate", main=dchange.title, sub=x)+abline(h=1, col="grey30")+abline(h=.9, col="grey10", lty="dashed")+abline(h=1.1, col="grey10", lty="dashed")
  
  
  initial.cfr <- round(median(head(mmm/cmm*100)),1)
  current.cfr <- round(median(tail(mmm/cmm*100)),1)
  
  summary.cov <- list("Incidence rate (daily, per 100K)" = c(initial.incr, current.incr), "Mortality rate (daily, per 100K)" = c(initial.mortr, current.mortr), "CFR (%)" = c(initial.cfr, current.cfr))
  
  return(c(plot.cases, plot.incr, plot.deaths, plot.mortr, summary.cov))
  
  }

## Compare two places

compare <- function (location1, location_type1, location2, location_type2, epiw){
  list(cov.data(location1, location_type1, epiw), cov.data(location2, location_type2, epiw))
}

