## Midterm Phase I
## Data Description and Visualization
## Group 9
## Group Members
## Basanta Chalise
## Divya Sinha
## Santosh Chapagain
## Wei Gu



##Installation of the libraries, if required
if(!require(ggplot2))install.packages("ggplot2")
if(!require(dplyr))install.packages("dplyr")
if(!require(corrplot))install.packages("corrplot")

#Read the data
data <- read.csv("C://Data//RetentionDataRaw.csv", header = T)
# The data had 97465 observations of 26 variables

##Preprocessing: Checking NAs
cat("There are", nrow(data[!complete.cases(data$DebtDimId),]),"out of",
    nrow(data), "total observations that feature NA value in DebtDimId.","\n",
    "The observations with NA in DebtDimId will be excluded from the new data frame, data2,
    giving us a new total of", nrow(data)-nrow(data[!complete.cases(data),]),
    "observations to work with.")

##Create a working dataset without any NAs in DebtDimId
data2 <- data[complete.cases(data$DebtDimId),]

##Preprocessing: Checking for duplicate entries
cat("The number of non-duplicate observations within the data set is",
    nrow(unique(data2)), "out of", "\n", nrow(data2),
    "indicating that there are", nrow(data2)-nrow(unique(data2)),
    "duplicates within the data set.","\n")

duplicate <- which(duplicated(data2)) #Index of duplicate observations
data3 <- data2[-duplicate,] #create new dataset without the duplicates

#Replace the empty levels of the status column with an indication that it is an open account
levels(data3$External.Status)[2] <- "O"

##Preprocess the datatype appropriately

str(data3) #check the variables' datatype
#DebtDimId - Int - OK
#Open.Date - Factor - Not OK
#Row.Num - Int - OK
#Last.Statement.Date - Factor - Not OK
#Cycle.Date - Factor - Not OK
#Months.on.Book - Factor - Not Ok
#External.Status - Factor - OK
#Days.Deliq - Factor - Not Ok
#Credit.Limit - Factor - Not Ok
#Opening.Balance - Factor - Not Ok
#Ending.Balance - Factor - Not Ok
#Over.limit.Amount - Factor - Not Ok
#Actual.Min.Pay.Due - Factor - Not Ok
#Total.Min.Pay.Due - Factor - Not Ok
#Net.Payments.During.Cycle - Factor - Not Ok
#Net.Purchases.During.Cycle - Factor - Not Ok
#Net.Cash.Advances.during.Cycle - Factor - Not Ok
#Net.Premier.Fees.Billed.During.Cycle - Factor - Not Ok
#Net.Behaviour.Fees.Billed.During.Cycle - Factor - Not Ok
#Net.Concessions.Fees.Billed.During.Cycle - Factor - Not Ok
#ClosureReason - Factor - OK
#Month.End.Date - Factor - Not Ok
#Last.Payment.Date - Factor - Not Ok
#Quarterly.Fico.Score - int - OK
#Behaviour.Score - int - OK
#Good.Customer.Score -  Factor - Not Ok


#date_names <- c("Open.Date", "Last.Statement.Date","Cycle.Date","Month.End.Date","Last.Payment.Date") 
#data3[date_names] <- lapply(data3[date_names], as.Date, format = "%m/%d/%Y")

data3$Open.Date <- as.Date(data3$Open.Date, format = "%m/%d/%Y")
data3$Last.Statement.Date<- as.Date(data3$Last.Statement.Date, format = "%m/%d/%Y")
data3$Cycle.Date<- as.Date(data3$Cycle.Date, format = "%m/%d/%Y")
data3$Month.End.Date<- as.Date(data3$Month.End.Date, format = "%m/%d/%Y")
data3$Last.Payment.Date<- as.Date(data3$Last.Payment.Date, format = "%m/%d/%Y")

data3$Months.On.Book <- as.character(data3$Months.On.Book)
data3$Days.Deliq <- as.character(data3$Days.Deliq)
data3$Good.Customer.Score <- as.character(data3$Good.Customer.Score)

data3$Months.On.Book <- as.numeric(data3$Months.On.Book)
data3$Days.Deliq <- as.numeric(data3$Days.Deliq)
data3$Good.Customer.Score <- as.numeric(data3$Good.Customer.Score)

#Remove $ sign and comma from the amounts and convert the class to numeric
for(i in 9:20){
    data3[,i] <- gsub("\\$","",data3[,i])
    data3[,i] <- gsub(",","",data3[,i])
    data3[,i] <- as.character(data3[,i])
    data3[,i] <- as.numeric(data3[,i])
        }

str(data3) #Recheck the variable datatypes

#Function to check missing values
na<- function(data){
  return(sum(is.na(data)))
}
missing <- apply(data3, 2, na)  
missing

cleandata <- data3

##Write the cleaned data to a .csv file
write.csv(cleandata, file = "C:\\Data\\CleanedData.csv")


##IGNORE
# ##Add a Expenditure to Payment Proportion column in the dataset
# cleandata = data4 %>% mutate(Payment.Purchase.Proportion = Net.Payments.During.Cycle/(Net.Purchases.During.Cycle+
#                                                                   Net.Cash.Advances.During.Cycle+
#                                                                   Net.Premier.Fees.Billed.During.Cycle+
#                                                                   Net.Behavior.Fees.Billed.During.Cycle+
#                                                                   Net.Concessions.Billed.During.Cycle))
# 
# ##Infs are generated in the last column
# ##Change Inf to NAs and ignore them while calculating the mean
# is.na(cleandata) <- sapply(cleandata, is.nan)
# is.na(cleandata) <- sapply(cleandata, is.infinite)

###Exploratory Analysis
###visualizations

### Find the Status At the end of Max Month Used for every User 
res_status <- cleandata %>% group_by(DebtDimId) %>% summarise(minmonth = min(Months.On.Book), 
                                                              maxmonth = max(Months.On.Book),
                                                              status = External.Status[which.max(Months.On.Book)],
                                                              ClosureReason = ClosureReason[which.max(Months.On.Book)],
                                                              maxlimit= min(Credit.Limit),
                                                              minlimit = min(Credit.Limit),
                                                              meanQfico = mean(Quarterly.Fico.Score),
                                                              meanBscore = mean(Behavior.Score),
                                                              meanGCscore = mean(Good.Customer.Score, na.rm = T))

res_status$minmonth<-as.numeric(res_status$minmonth)
res_status$maxmonth<-as.numeric(res_status$maxmonth)

##Function for colors in a ggplot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

### Histogram for Credit Limit of the users
ggplot(res_status, aes(x= maxlimit))+
  geom_histogram(fill=gg_color_hue(1))+ 
  geom_vline(aes(xintercept=mean(maxlimit)),color="darkred", linetype="dashed", size=1)+
  labs(x="Credit Limit", title="Histogram for Credit Limit")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


###Histogram for Months On Book for the users
ggplot(res_status, aes(x= maxmonth))+
  geom_histogram(fill=gg_color_hue(1))+ 
  geom_vline(aes(xintercept=mean(maxmonth)),color="darkred", linetype="dashed", size=1)+
  labs(x="Months on Book", title="Histogram for Months on Book")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


##Columnchart for Account Status
ggplot(res_status, aes(res_status$status, fill=status))+
  geom_bar()+
  labs(x= "Account Status", title=" Column chart for Account Status ", y="Count")+
  scale_fill_manual(name="Account Status", values=gg_color_hue(6), labels=c("Open", "Closed", "Revoked", "Frozen", "Interest Prohibited", "Charged Off"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Table  to view  the total number of each status for the customers
table(res_status$status)

##Boxplot for Quarterly FICO score according to status
ggplot(res_status, aes(x=status, y= meanQfico, fill=status))+
  geom_boxplot()+
  labs(x="Status", y="Quarterly Mean FICO score", title="Quarterly Mean FICO score by status")+
  scale_fill_manual(name="Account Status", values=gg_color_hue(6), labels=c("Open", "Closed", "Revoked", "Frozen", "Interest Prohibited", "Charged Off"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


##Boxplot for Behaviour score according to status
ggplot(res_status, aes(x=status, y= meanBscore, fill=status))+
  geom_boxplot()+
  labs(x="Status", y="Mean Behaviour score", title="Mean Behaviour score by status")+
  scale_fill_manual(name="Account Status", values=gg_color_hue(6), labels=c("Open", "Closed", "Revoked", "Frozen", "Interest Prohibited", "Charged Off"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

##Boxplot for Good Customer Score according to Status
ggplot(res_status, aes(x=status, y= meanGCscore, fill=status))+
  geom_boxplot()+
  labs(x="Status", y="Mean Good Customer score", title="Mean Good Customer score by status")+
  scale_fill_manual(name="Account Status", values=gg_color_hue(6), labels=c("Open", "Closed", "Revoked", "Frozen", "Interest Prohibited", "Charged Off"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Lets find the reason why card is closed 
closurereason <- res_status %>% filter(status == 'C') %>% select(DebtDimId, ClosureReason)

### Bar chart for Closure Reason
ggplot(closurereason, aes(ClosureReason))+
  geom_bar(position = position_stack(reverse = TRUE), fill=gg_color_hue(1))+
  coord_flip()+
  theme_bw()+
  ggtitle("Bar Chart for Closure Reasons")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(closurereason, aes(ClosureReason))+
  geom_bar(position = "identity", fill=gg_color_hue(1))+
  theme(axis.text.y  = element_text(angle=90), axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(angle = 180))


### Plot for Deliquencies Days by account status
ggplot(cleandata, aes(x=External.Status, y=Days.Deliq, fill=External.Status))+
  geom_boxplot()+
  labs(x="Account Status", y="Deliquency Days", title="Delinquency Days by Account Status")+
  scale_fill_manual(name="Account Status", values=gg_color_hue(6),labels=c("Open", "Closed", "Revoked", "Frozen", "Interest Prohibited", "Charged Off"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
### Days Deliquency is more related with the account balance



### Net Payment on Statement Cycle for Every Customer 
res_status_netpayment <- cleandata %>% group_by(DebtDimId, Cycle.Date) %>% select(DebtDimId, Cycle.Date, Net.Payments.During.Cycle,Net.Purchases.During.Cycle, Net.Premier.Fees.Billed.During.Cycle,
                                                                                    Net.Cash.Advances.During.Cycle,Net.Behavior.Fees.Billed.During.Cycle,Net.Concessions.Billed.During.Cycle)
res_status_netpayment$PaymentMonth <-as.Date(res_status_netpayment$Cycle.Date)
res_status_netpayment$Months <- substr(res_status_netpayment$PaymentMonth,0,7)
res_status_netpayment$MonthlyBalance <- rowSums(res_status_netpayment[, c(4,5,6,7,8)])

### Box plot for Payment on Every Months
ggplot(res_status_netpayment, aes(x=Months, y=Net.Payments.During.Cycle, group=Months))+
  geom_boxplot()+
  labs(x="Months", y="Net Payments During Cycle", title="Net Payments During Cycle")+ 
  coord_cartesian(ylim = c(-500, 1600))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Monthly Balance During Cycle 
ggplot(res_status_netpayment, aes(x=Months, y=MonthlyBalance, group=Months))+
  geom_boxplot()+
  labs(x="Status", y="Total Monthly Balance", title="Box Plot for Monthly Balance")+ 
  coord_cartesian(ylim = c(-500, 1400))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

df2 = cor(cleandata[, c(6,8:20,24,25)])
corrplot(df2, method = "circle", type="lower", cl.cex = 1, tl.cex=1)

