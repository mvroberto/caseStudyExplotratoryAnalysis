#### NEI <- readRDS("summarySCC_PM25.rds")
#### SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
answer_one <- function(){
  
  
  df <- select(NEI,Emissions,year)
  counts <- table(df$year)
  
  plot <- barplot(counts, main = "PM2.5 Emissions", xlab = "Year")
}

answer_two <- function(){
  
  baltimore <- filter(NEI, fips == "24510")
  df <- select(baltimore,Emissions,year)
  counts <- table(df$year)
  
  plot <- barplot(counts, main = "PM2.5 Emissions Baltimore", xlab = "Year")
}

answer_three <- function(){
  library(ggplot2)
  baltimore <- filter(NEI, fips == "24510")
  df <- select(baltimore,Emissions,type,year)
  df <- group_by(df, type, year) %>% summarise_each(funs(sum), emissions = Emissions)
  a <- ggplot(a,aes(x = year , y = emissions, fill = type))
  plot <- b + geom_bar(stat = "identity", position  ="dodge")
  plot
}

answer_four <- function(){
  
 names <- SCC$Short.Name
 SCC_number <- SCC$SCC
 a <-  grep("(Coal|Combustion)+",names)
 SCC_number <- SCC_number[a]
 df <- filter(NEI, SCC %in% SCC_number)
 df <- select(df,year, Emissions)
 table <- table(df$year)
 plot <- barplot(table, main = "Coal Combustion Emissions")
 a
}


answer_five <- function(){
  
  baltimore <- filter(NEI, fips == "24510")
  df <- select(baltimore,Emissions,year, SCC)
  names <- SCC$Short.Name
  SCC_number <- SCC$SCC
  a <-  grep("(Motor| Vehicle)+",names)
  SCC_names <- names[a]
  SCC_number <- SCC_number[a]
  df <- filter(df, SCC %in% SCC_number)
  df <- select(df,year, Emissions)
  table <- table(df$year)
  plot <- barplot(table, main = "Motor Vehicles Emissions Baltimore")
  plot

}


answer_six <- function(){
  

  
}