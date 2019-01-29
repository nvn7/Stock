
library(caret)

comp=c("SENSEX","TEAM_LEASE","UJJIVAN","LUX_INDUS","MAX_INDIA","THYROCARE")

clear = function()
{
  cat("\014")    
}
clear()
cat("\n\t\tWelcome to Stock Market Prediction","\n\n")
cat("\tBY   Rohith Kumar  Umapathi  Naveen Kumar\n\n")
cat("Choose the dataset you wish to predict the stock for","\n\n")
cat("1. SENSEX","\t","2. TEAM LEASE","\n")
cat("3. UJJIVAN","\t","4. LUX INDUSTRIES","\n")
cat("5. MAX INDIA","\t","6. THYROCARE","\n")


choice = as.numeric(readline("Enter your choice : "))
if(!(choice %in% (1:6)))
{
  stop("Invalid input")
}
COMPANY=comp[choice]

START_DATE = "2017-09-01"
END_DATE = "2018-11-01"

choice = readline("Do you wish to use default date range? (y|n) ")
if(choice=="N" | choice=="n")
{
  cat("Enter the stock data range to be fetched YYYY-MM-DD","\n")
  START_DATA = readline("START_DATE of stock data to fetch")
  END_DATA = readline("END_DATE of stock data to fetch")
}

wantDate = function(dateAsString)
{
  return(as.Date(dateAsString,format="%Y-%m-%d"))
}

cat("Fetching data for COMPANY")

getData = function()
{
   data = read.csv(paste("Dataset/",COMPANY,".csv",sep=""))[,c('Date','Close')]
  
  
  data['Date'] = wantDate(data$Date)
  
  
  data = data[order(data['Date'],decreasing = FALSE),]
  data = data[(data$Date>START_DATE & data$Date<END_DATE),]
  
  rownames(data) = NULL
  
  return(na.omit(data))
}


data = getData()






LAG = 1
DATA_SIZE = nrow(data)

CUR = round(0.6 * DATA_SIZE)

W_SIZE = CUR - 1

TRN_RANGE = (CUR - W_SIZE):CUR 


REM = (DATA_SIZE - CUR) -1
DELAY = as.numeric(readline(paste("Predict stock for how many days in advance? (1 - ",REM,") : ")))



color1 = paste("#","606060",sep="")
color2 = paste("#","03A9F4",sep="")


plotData = function(xval,yval=NULL,color)
{
  points.default(xval, yval,col=color,type="l",lwd="2")
}

plot(ts(data$Close),type="l",xlab = paste(substr(START_DATE,0,4)," to ",substr(END_DATE,0,4)),ylab = "Closing Price",main=paste(COMPANY," Stock Prediction",sep=""),col=color1,lwd="2")

par(font=1) 
legend("topright", c("Actual Stock","Training Data"), fill=c(color1,color2,color3))


slidingWindow = function(data)
{
  
  y = data[-(1:LAG),]
  rownames(y) = NULL
  x= data[1:(DATA_SIZE-LAG),]
  train = data.frame(x[,2],y[,2])
  colnames(train) = c("x","y")
  return(train)
}

View(data)

train =  slidingWindow(data)

View(train)

x_axis = as.numeric(rownames(train))


plotData(x_axis[TRN_RANGE],train$y[TRN_RANGE],color=color2)



