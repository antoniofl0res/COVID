# COVID

**epicurve.R** is a script for tracking the COVID epidemic in Brazil (at municipality and state levels) and in the world (at country level)

The script uses two sources of data:
- https://brasil.io/dataset/covid19/files/ - the .csv file has to be downloaded and updated on a daily basis
- https://covid.ourworldindata.org/data/owid-covid-data.csv - data will be updated automaticaly every time the code is run

The script includes 4 functions:

#### covid
With this function, you can create graphs of daily cases (7-day rolling average), daily incidence rate, daily deaths (7-day rolling average) and daily mortality rate by place.  
In addition, two graphs are created to show change in incidence and mortality over the previous 2 weeks.  
Finally, the output also includes population, incidence rates (at the beginning of the specified period, median and at the end of the period), mortality rates (likewise, beginning, median and end) and case-fatality ratio (beginning, median and end)

Parameters:  
*place*: municipality, state or country  
  * municipality: enter the name of the municipality between quotation marks or its IBGE code. Eg, "Porto Alegre" or 4314902. Note that some municipalities in different states may have the same name, while the IBGE code is unique - so, prefer to use it. Also, some municipalitis' names may be accented - call View(br), look up the municipality, then copy and paste the name with special characters. Eg, Ji-Paraná = Ji-ParanÃ¡. Likewise, using the unique IBGE code speeds up execution of the code
  * state: enter the standard two-letter acronym for the state between quotation marks. Eg, Amazonas = "AM", Rondonia = "RO", Rio Grande do Sul = "RS", and so on. Call View(st) and the look up the acronym
  * country: enter the three-letter country ISO code between quotation marks. Eg, Brazil = "BRA", Germany = "DEU, and so on. Call View(world), then look up the ISO code

*place_type*: define type of place
* 1 for municipality
* 2 for state
* 3 for country

*initial_date*: define the start date of the period you wish to plot. Date format should be "YYYY-MM-DD" (between quotation marks)

 
#### covid.profile
Output of this function includes:
* population  
* total cases and deaths
* cumulative incidence, mortality and case-fatality ratio
* date when first case was reported
* days until incidence reached 0.1% and 1% of population
* proportion of total cases in the last 15, 30 and 60 days

Parameters:  
*place*: same as above  
*place_type*: same as above  

#### findbypop
Filter municipalities by population in a determined state.  
For example: what are the cities in Roraima state with a population of 100 thousand or above?

Parameters:
*state*: enter the standard two-letter acronym for the state between quotation marks. Eg, Amazonas = "AM", Rondonia = "RO", Rio Grande do Sul = "RS", and so on. Call View(st) and the look up the acronym  
*population*: enter a number. The function will filter all cities with a population equal or greater than the number you choose. Eg, 100000 or 10^5

#### compare

Compare two places with the covid function

Parameters:  
*place1*  
*place_type1*  
*place2*  
*place_type2*  
*initial_date*  

Same instructions as for the covid funtion
