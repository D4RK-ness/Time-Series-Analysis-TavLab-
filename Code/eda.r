library(ggplot2)
library(plotly)



dataset <- read.csv("C://Users//Utkarsh//Documents//TavLab//testing_vaccination.csv")
list_of_states = unique(dataset$State)

list_of_state_graphs <- list()


for (k in 1:length(list_of_states)){
  state_name <- list_of_states[k]
  dataset_curr <- subset(dataset, dataset$District == "DIU")
  
  #Guide:
  #March 2020 - 1
  #April 2020 - 2
  #May 2020 - 3
  #June 2020 - 4
  #July 2020 - 5
  #August 2020 - 6
  #September 2020 - 7
  #October 2020 - 8
  #November 2020 - 9
  #December 2020 - 10
  #January 2021 - 11
  #February 2021 - 12
  #March 2021 - 13
  #April 2021 - 14
  #May 2021 - 15
  #June 2021 - 16
  #July 2021 - 17
  #August 2021 - 18
  #September 2021 - 19
  #October 2021 - 20
  #November 2021 - 21
  #December 2021 - 22
  #January 2022 - 23
  #February 2022 - 24
  #March 2022 - 25
  #April 2022 - 26
  #May 2022 - 27
  #June 2022 - 28
  
  Month_chr <- c("MAR20","APR20","MAY20","JUN20","JUL20","AUG20","SEP20","OCT20","NOV20",
             "DEC20","JAN21","FEB21","MAR21","APR21","MAY21","JUN21","JUL21","AUG21",
             "SEP21","OCT21","NOV21","DEC21","JAN22","FEB22","MAR22","APR22","MAY22")
  
  Month <- factor(Month_chr)
  
  RTPCR_Tests <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  RTPCR_Pos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  CBNAAT_Tests <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  CBNAAT_Pos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  TRUENATTests <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  TRUENATConfTests <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  TRUENATConfPos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  Antigen_Tests <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  Antigen_Pos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  Tests <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  OtherTestPos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  Total_Pos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  OtherTest <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  Total_Vaccination <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  for (i in 19:nrow(dataset_curr)){
    d = dataset_curr[i,4]
    m = substr(d,4,10)
    
    if (m == "03/2020"){
      j=1
    } else if (m == "04/2020"){
      j=2
    } else if (m == "05/2020"){
      j=3
    } else if (m == "06/2020"){
      j=4
    } else if (m == "07/2020"){
      j=5
    } else if (m == "08/2020"){
      j=6
    } else if (m == "09/2020"){
      j=7
    } else if (m == "10/2020"){
      j=8
    } else if (m == "11/2020"){
      j=9
    } else if (m == "12/2020"){
      j=10
    } else if (m == "01/2021"){
      j=11
    } else if (m == "02/2021"){
      j=12
    } else if (m == "03/2021"){
      j=13
    } else if (m == "04/2021"){
      j=14
    } else if (m == "05/2021"){
      j=15
    } else if (m == "06/2021"){
      j=16
    } else if (m == "07/2021"){
      j=17
    } else if (m == "08/2021"){
      j=18
    } else if (m == "09/2021"){
      j=19
    } else if (m == "10/2021"){
      j=20
    } else if (m == "11/2021"){
      j=21
    } else if (m == "12/2021"){
      j=22
    } else if (m == "01/2022"){
      j=23
    } else if (m == "02/2022"){
      j=24
    } else if (m == "03/2022"){
      j=25
    } else if (m == "04/2022"){
      j=26
    } else if (m == "05/2022"){
      j=27
    } else{
      next
    }
    
    x = dataset_curr[i,5]
    if (!is.na(x)){
      RTPCR_Tests[j] = RTPCR_Tests[j] + x
    }
    
    x = dataset_curr[i,6]
    if (!is.na(x)){
      RTPCR_Pos[j] = RTPCR_Pos[j] + x
    }
    
    x = dataset_curr[i,7]
    if (!is.na(x)){
      CBNAAT_Tests[j] = CBNAAT_Tests[j] + x
    }
    
    x = dataset_curr[i,8]
    if (!is.na(x)){
      CBNAAT_Pos[j] = CBNAAT_Pos[j] + x
    }
    
    x = dataset_curr[i,9]
    if (!is.na(x)){
      TRUENATTests[j] = TRUENATTests[j] + x
    }
    
    x = dataset_curr[i,10]
    if (!is.na(x)){
      TRUENATConfTests[j] = TRUENATConfTests[j] + x
    }
    
    x = dataset_curr[i,11]
    if (!is.na(x)){
      TRUENATConfPos[j] = TRUENATConfPos[j] + x
    }
    
    x = dataset_curr[i,12]
    if (!is.na(x)){
      Antigen_Tests[j] = Antigen_Tests[j] + x
    }
    
    x = dataset_curr[i,13]
    if (!is.na(x)){
      Antigen_Pos[j] = Antigen_Pos[j] + x
    }
    
    x = dataset_curr[i,14]
    if (!is.na(x)){
      Tests[j] = Tests[j] + x
    }
    
    x = dataset_curr[i,15]
    if (!is.na(x)){
      OtherTestPos[j] = OtherTestPos[j] + x
    }
    
    x = dataset_curr[i,16]
    if (!is.na(x)){
      Total_Pos[j] = Total_Pos[j] + x
    }
    
    x = dataset_curr[i,17]
    if (!is.na(x)){
      OtherTest[j] = OtherTest[j] + x
    }
    
    x = dataset_curr[i,19]
    
    mm = substr(d,4,5)
    dd = substr(d,1,2)
    
    if (!is.na(x)){
      if (mm == "01" || mm == "03" || mm == "05" || mm == "07" || mm == "08" ||
          mm == "10" || mm == "12"){
        if (dd == "31"){
          Total_Vaccination[j] = Total_Vaccination[j] + x
        }
      } else if (mm == "04" || mm == "06" || mm == "09" || mm == "11"){
        if (dd == "30"){
          Total_Vaccination[j] = Total_Vaccination[j] + x
        }
      } else{
        if (dd == "28"){
          Total_Vaccination[j] = Total_Vaccination[j] + x
        }
      }
    }
  }
  
  for (i in 27:2){
    Total_Vaccination[i] = Total_Vaccination[i] - Total_Vaccination[i-1]
  }
  
  Vac_Diff <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  for (i in 27:2){
    Vac_Diff[i] = Total_Vaccination[i] - Total_Vaccination[i-1]
  }
  
  new_df <- data.frame(Month, RTPCR_Tests, RTPCR_Pos, CBNAAT_Tests, CBNAAT_Pos, TRUENATTests,
              TRUENATConfTests, TRUENATConfPos, Antigen_Tests, Antigen_Pos, Tests,
              OtherTestPos, Total_Pos, OtherTest, Total_Vaccination, Vac_Diff)
  
  
  #plot(new_df$Month, new_df$Total_Pos, type="l")
  
  g1 <- plotly::plot_ly(type="scatter", mode="lines+markers") %>%
    add_trace(data=new_df,
              x=~Month,
              y=~Total_Pos,
              name = "Total Positive cases for the month") %>%
    layout(title = state_name)
  
  g2 <- plotly::plot_ly(type="scatter", mode="lines+markers") %>%
    add_trace(data=new_df,
              x=~Month,
              y=~Total_Pos,
              name = "Total Positive cases for the month") %>%
    add_trace(data=new_df,
              x=~Month,
              y=~Tests,
              name = "Total Tests for the month") %>%
    layout(title = state_name)
    
  g3 <- plotly::plot_ly(type="scatter", mode="lines+markers") %>%
    add_trace(data=new_df,
              x=~Month,
              y=~Total_Pos/Tests,
              name = "Ratio of Positive Cases to the Tests for the month") %>%
    layout(title = state_name)
  
    
  g4 <- plotly::plot_ly(type="scatter", mode="lines+markers") %>%
    add_trace(data=new_df,
              x=~Month,
              y=~Total_Vaccination,
              name = "Total Vaccination for the month") %>%
    layout(title = state_name)
  
  list_of_graphs <- list(g1,g2,g3,g4)
  list_of_state_graphs[[k]] = list_of_graphs
}



#g5 <- plotly::plot_ly(type="scatter", mode="lines+markers") %>%
#  add_trace(data=new_df,
#            x=~Month,
#            y=~Vac_Diff,
#            name = "More Vaccines compared to Previous Month") %>%
#  layout(title = state_name)

#g6 <- plotly::plot_ly(type="scatter", mode="lines+markers") %>%
#  add_trace(data=new_df,
#            x=~Month,
#            y=~Total_Vaccination,
#            name = "Total Vaccination for the month") %>%
#  add_trace(data=new_df,
#            x=~Month,
#            y=~Vac_Diff,
#            name = "More Vaccines compared to Previous Month") %>%
#  layout(title = state_name)
