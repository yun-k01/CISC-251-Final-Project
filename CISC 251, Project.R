online <- read_csv("Desktop/CISC 251/online_shoppers_intention.csv")

# creating a subset with the selected attributes
online_attr = online %>%
  select(c(Administrative, ExitRates, VisitorType, Revenue))

# changing exit rates to categorical using IQR
summary(online_attr$ExitRates)
online_attr <- within(online_attr, {   
  Exit.cat <- NA # need to initialize variable
  Exit.cat[ExitRates < 0.02516] <- "Min and 1st Q"
  Exit.cat[ExitRates >= 0.02516 & ExitRates < 0.04307] <- "Mean"
  Exit.cat[ExitRates >= 0.04307 & ExitRates < 0.05] <- "Median"
  Exit.cat[ExitRates >= 0.05] <- "3rd Q and Max"
} )

# visualize statistics
ggpairs(data = online_attr, aes(color = Revenue, alpha = 0.8), columns = 1:3)

# cross tables for attributes
CrossTable(online_attr$Administrative, online_attr$Revenue)
CrossTable(online_attr$Exit.cat, online_attr$Revenue)
CrossTable(online_attr$VisitorType, online_attr$Revenue)


