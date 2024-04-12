library(dplyr)
library(tidyverse)

Google.trends <- read_csv("C:/Users/DAVID/Desktop/Google-trends.csv")
CDC <- read_tsv("C:/Users/DAVID/Desktop/CDC.txt")
nyt <- map_df(list.files("NYT/", full.names = TRUE), read_csv)

cdc_converted <- CDC %>%
  pivot_longer(cols=!contains("cause"), names_to="year", values_to="percent_deaths") %>%
  pivot_wider(names_from=cause, values_from=percent_deaths) %>%
  transmute(
    year=year,
    Alzheimer=alzheimer,
    Cancer=cancer_all+cancer_lung+cancer_breast+cancer_anal,
    Accidents=accident,
    Diabetes=diabetes,
    HeartProblems=heart,
    Homicide=homicide_all+homicide_firearm+homicide_legmil,
    KidneyProblems=kidney,
    Overdose=overdose,
    FluDiseases=influpneu,
    LungIssues=loresp,
    Stroke=stroke,
    Suicide=suicide,
    TerrorismAttacks=terrorism
  ) %>%
  pivot_longer(cols=!year, names_to="cause", values_to="rel_freq") %>%
  mutate(data="CDC") %>%
  select(data, year, cause, rel_freq)



nyt_converted <- nyt %>%
  select(-ID) %>%
  pivot_wider(names_from=Words, values_from=count) %>%
  transmute(
    year=year,
    Alzheimer=`alzheimer's disease`,
    Cancer=cancer+`malignant neoplasms`,
    Accidents=`unintentional injuries`+`car accident`+pileup+`car crash`,
    Diabetes=diabetes,
    HeartProblems=`heart disease`+`heart failure`+`cardiovascular disease`,
    Homicide=homicide+murder+manslaughter+assassination,
    KidneyProblems=`kidney disease`+nephrosis+nephritis+`nephrotic syndrome`,
    Overdose=`drug overdose`,
    FluDiseases=Influenza+pneumonia+flu,
    LungIssues=`respiratory disease`+bronchitis+emphysema+asthma,
    Stroke=stroke+`cerebrovascular diseases`,
    Suicide=suicide+`self-harm`,
    TerrorismAttacks=terrorism+terrorist+`terror attack`+shootings+`gun violence`+`knife attack`+knifing+lynching
  ) %>%
  pivot_longer(cols=!year, names_to="cause", values_to="count") %>%
  group_by(year) %>%
  mutate(total_words=sum(count)) %>%
  ungroup() %>%
  mutate(rel_freq=count/total_words, data="NYT") %>%
  select(data, year, cause, rel_freq)



Google.trends_converted <- Google.trends %>%
  transmute(
    year=Words,
    Alzheimer=`alzheimer's`,
    Cancer=cancer,
    Accidents=`car accidents`,
    Diabetes=diabetes,
    HeartProblems=`heart disease`,
    Homicide=homicide,
    KidneyProblems=`kidney disease`,
    Overdose=overdose,
    FluDiseases=pneumonia,
    LungIssues=`respiratory disease`,
    Stroke=stroke,
    Suicide=suicide,
    TerrorismAttacks=terrorism
  ) %>%
  pivot_longer(cols=!year, names_to="cause", values_to="rel_freq") %>%
  filter(year != "Average") %>%
  mutate(data="Google") %>%
  select(data, year, cause, rel_freq)



FinalDataset <- rbind(cdc_converted, nyt_converted, Google.trends_converted)

ggplot(data=FinalDataset,mapping=aes(x=cause,y=rel_freq))+
labs(x="Death Causes",y="Relative Frequency %",title="The most frequent causes of death in the US")+
geom_col(aes(fill=data))+
theme_linedraw()

