#### NEI <- readRDS("summarySCC_PM25.rds")
#### SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
answer_one <- function(){
  par(mfcol = c(1,1))
  
  df <- select(NEI,Emissions,year)
  counts <- tapply(df$Emissions,df$year, sum)
  
  
  plot <- barplot(counts, main = "PM2.5 Emissions", xlab = "Year")
  dev.copy(png,'plot1.png')
  dev.off()
  counts
}

answer_two <- function(){
  par(mfcol = c(1,1))
  baltimore <- filter(NEI, fips == "24510")
  df <- select(baltimore,Emissions,year)
  counts <- tapply(df$Emissions,df$year, sum)
  
  plot <- barplot(counts, main = "PM2.5 Emissions Baltimore", xlab = "Year")
  dev.copy(png,'plot2.png')
  dev.off()
  counts
}

answer_three <- function(){
  library(ggplot2)
  baltimore <- filter(NEI, fips == "24510")
  df <- select(baltimore,Emissions,type,year)
  df$year <- as.factor(df$year)
  df <- group_by(df, type, year) %>% summarise_each(funs(sum), emissions = Emissions)
  b <- ggplot(df,aes(x = year , y = emissions, fill = type))
   plot <- b + geom_bar(stat = "identity", position  ="dodge")
  ggsave("plot3.png", width = 5, height = 5)


  }

answer_four <- function(){
  
  names <- SCC$Short.Name
  SCC_number <- SCC$SCC
  a <-  grep("Coal.+",names)
  SCC_number <- SCC_number[a]
  df <- filter(NEI, SCC %in% SCC_number)
  df <- select(df,year, Emissions)
  table <- tapply(df$Emissions,df$year, sum)
  plot <- barplot(table, main = "Coal Combustion Emissions")
  par(mfcol = c(1,1))
  dev.copy(png,'plot4.png')
  dev.off()
}


answer_five <- function(){
  par(mfcol = c(1,1))
  baltimore <- filter(NEI, fips == "24510")
  df <- select(baltimore,Emissions,year, SCC)
  SCC_number <- SCC$SCC
  names <- SCC$Short.Name
  a <-  grep("(Motor| Vehicle)+",names)
 
  SCC_number <- SCC_number[a]
  df <- filter(df, SCC %in% SCC_number)
  df <- select(df,year, Emissions)
  table <- tapply(df$Emissions,df$year, sum)
  plot <- barplot(table, main = "Motor Vehicles Emissions Baltimore")
  dev.copy(png,'plot5.png')
  dev.off()
  
}


answer_six <- function(){
  names <- SCC$Short.Name
  SCC_number <- SCC$SCC
  
  a <-  grep("(Motor| Vehicle)+",names)
 
  
  SCC_number <- SCC_number[a]
  
  baltimore <- filter(NEI, fips == "24510")
  df_baltimore <- select(baltimore,Emissions,year, SCC)
  df_baltimore <- filter(df_baltimore, SCC %in% SCC_number)
  df_baltimore <- select(df_baltimore,Emissions,year)
  
  los_angeles <- filter(NEI, fips == "06037")
  df_los_angeles <- select(los_angeles,Emissions,year, SCC)
  df_los_angeles <- filter(df_los_angeles, SCC %in% SCC_number)
  df_los_angeles <- select(df_los_angeles,Emissions,year)
  
  par(mfcol = c(1,2))
  
  balt_table <- tapply(df_baltimore$Emissions,df_baltimore$year, sum)
  barplot(balt_table, main = "Motor Vehicles Emissions Baltimore")

  
  angeles_table <- tapply(df_los_angeles$Emissions,df_los_angeles$year, sum)
  barplot(angeles_table, main = "Motor Vehicles Emissions Los Angeles")
 
   dev.copy(png,'plot6.png')
  dev.off()
  
}

a <- function(){
  df <- select(NEI,Emissions,year)
  counts <- table(df$year)
  
  a <- list()
  a <- list(df,counts)
  
}
