#Lib and packages
library(plotly); library(ggplot2)
#Reporting WD
setwd("C:/Users/pv532f/Desktop/reports/")

#GET SA files and format them
Dallas_ckt1 <- read.csv2(file = "iepudaltx02r1_0101_5weeks_consolidated.csv", header = TRUE, sep=",", colClasses=c(rep("character",5), rep("NULL",6), rep("character",2), rep("NULL",6)))
Dallas_ckt2 <- read.csv2(file = "iepudaltx02r2_0101_5weeks_consolidated.csv", header = TRUE, sep=",", colClasses=c(rep("character",5), rep("NULL",6), rep("character",2), rep("NULL",6)))
cost_model <- read.csv2(file = "cost_sheet_MIS_summary.csv", header = TRUE, sep=",", colClasses=c(rep("numeric",8)))
DallasY_ckt1 <- read.csv2(file = "Yearly reports/Device_Interface_Report_-_Monthly_View_iepudaltx02r1.csv", header = TRUE, sep=",", colClasses=c(rep("character",3), rep("NULL",8), rep("character",2), rep("NULL",6)))
DallasY_ckt2 <- read.csv2(file = "Yearly reports/Device_Interface_Report_-_Monthly_View_iepudaltx02r2.csv", header = TRUE, sep=",", colClasses=c(rep("character",3), rep("NULL",8), rep("character",2), rep("NULL",6)))

#plot Time Series

time_index_W <- 1:nrow(Dallas_ckt1)
time_index_Y <- 1:nrow(DallasY_ckt1)

#graph settings

f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "lightgrey"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 10,
  color = "black"
)
ax <- list(
  title = "Time (Weeks)",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 45,
  tickfont = f2,
  exponentformat = "e",
  showexponent = "All"
)
ay <- list(
  title = "Intf Util (%)",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 45,
  tickfont = f2,
  exponentformat = "e",
  showexponent = "All"
)
#15' polling Plot x 3 weeks // ckt1 + ckt2

pw_ckt1 <- plot_ly(data=Dallas_ckt1, x=Dallas_ckt1$Date.Time, y=Dallas_ckt1$Interface...Out.Utilization...., name = "Intf Out Util", fill = "tozeroy")%>% 
  add_trace(y=fitted(loess(Dallas_ckt1$Interface...Out.Utilization.... ~ time_index_W)), name = "Loess Fit")%>%
  layout(xaxis = ax, yaxis = ay, showlegend = TRUE)
pw_ckt1

pdw_ckt1 <- plot_ly(data=Dallas_ckt1,x=Dallas_ckt1$Date.Time,y=Dallas_ckt1$Interface.Discards...Out...., name = "Packet Discard (%)", mode = "line")%>% 
  add_trace(y=fitted(loess(Dallas_ckt1$Interface.Discards...Out.... ~ time_index_W)), name = "Loess Fit", fill = "tozeroy")
#layout(xaxis = ax, yaxis = ay, showlegend = TRUE)
pdw_ckt1


pw_ckt2 <- plot_ly(data=Dallas_ckt2, x=Dallas_ckt2$Date.Time, y=Dallas_ckt2$Interface...Out.Utilization...., name = "Intf Out Util", fill = "tozeroy")%>%
  add_trace(y=fitted(loess(Dallas_ckt2$Interface...Out.Utilization.... ~ time_index_W)), name = "Loess Fit")%>%
  layout(xaxis = ax, yaxis = ay, showlegend = TRUE)
pw_ckt2

pdw_ckt2 <- plot_ly(data=Dallas_ckt2,x=Dallas_ckt1$Date.Time,y=Dallas_ckt2$Interface.Discards...Out...., name = "Packet Discard (%)", mode = "line")%>% 
  add_trace(y=fitted(loess(Dallas_ckt2$Interface.Discards...Out.... ~ time_index_W)), name = "Loess Fit", fill = "tozeroy")
#layout(xaxis = ax, yaxis = ay, showlegend = TRUE)
pdw_ckt2


#18 month, monthly average plot // ckt1 + ckt2

py_ckt1 <- plot_ly(data=DallasY_ckt1, x=DallasY_ckt1$Date, y=DallasY_ckt1$Interface...Out.Utilization...., name = "Intf Out Util", fill = "tozeroy")
py_ckt1%>% add_trace(y=fitted(glm(as.numeric(DallasY_ckt1$Interface...Out.Utilization....) ~ time_index_Y)), name = "GLM Fit")

py_ckt2 <- plot_ly(data=DallasY_ckt2, x=DallasY_ckt2$Date, y=DallasY_ckt2$Interface...Out.Utilization...., name = "Intf Out Util", fill = "tozeroy")
py_ckt2%>% add_trace(y=fitted(glm(as.numeric(DallasY_ckt2$Interface...Out.Utilization....) ~ time_index_Y)), name = "GLM Fit")

reg_ckt1 <- lm(data = DallasY_ckt1, formula = DallasY_ckt1$Interface...Out.Utilization....~time_index_Y)
reg_ckt2 <- lm(data = DallasY_ckt2, formula = DallasY_ckt2$Interface...Out.Utilization....~time_index_Y)
#optimization problem

fmbcost <- 6.75
vmbcost <- 42.25
lambda <- fmbcost / vmbcost
X <- seq(0.5, 1, 0.01)
phy_speed <- 622
var_temp <- rep(0, length(time_index_W))
toopt <- rep(0, length(X))

for (i in 1:length(X)) {
  for (o in 1:length(time_index_W)) {
    if ((as.numeric(Dallas_ckt1$Interface...Out.Utilization....[o]) - X[i]) < 0) {
      var_temp[o] <- 0
    } else {
      var_temp[o] <- (as.numeric(Dallas_ckt1$Interface...Out.Utilization....[o]) - X[i])
    }
  }
toopt[i] <- (lambda*vmbcost) * X[i] * (phy_speed/100) * (1 - vmbcost/length(time_index_W)) + sum(var_temp) * (phy_speed/100) * (vmbcost  / length(time_index_W))
}

#quantile(as.numeric(Dallas_ckt1$Interface...Out.Utilization....), c(.95))

#Sustained Utilization calculation

ckt95th_1 <- sort(as.numeric(Dallas_ckt1$Interface...Out.Utilization....),decreasing = FALSE, na.last = NA)
#"Percentage you'll pay for - ckt 1"
sustained_ckt1 <- ckt95th_1[0.95*length(Dallas_ckt1$Interface...Out.Utilization....)]

ckt95th_2 <- sort(as.numeric(Dallas_ckt2$Interface...Out.Utilization....),decreasing = FALSE, na.last = NA)
#"Percentage you'll pay for - ckt 2"
sustained_ckt2 <- ckt95th_2[0.95*length(Dallas_ckt1$Interface...Out.Utilization....)]


#Cost analisys

#hicap
for (i in 1:length(cost_model$hicap_minbw)) {
    if (((sustained_ckt1+sustained_ckt2) * phy_speed/100 - (2*cost_model$hicap_minbw[i])) < 0) {
      costo_hicap[i] <- 2 * cost_model$hicap_minbw_cost[i]
      } else {
      costo_hicap[i] <- 2 * cost_model$hicap_minbw_cost[i] + cost_model$hicap_inc_per_mb[i] * (((sustained_ckt1+sustained_ckt2)*phy_speed/100) - 2*cost_model$hicap_minbw[i])
    }
  }

#burstable
b1 <- cost_model$burstable_cost_sust_rate[length(which(cost_model$hicap_minbw < (sustained_ckt1 * phy_speed/100))) + 1]
btot <- b1 + cost_model$burstable_cost_sust_rate[length(which(cost_model$hicap_minbw < (sustained_ckt2 * phy_speed/100))) + 1]
#costo_hicap <- 2 * cost_model$hicap_minbw_cost + cost_model$hicap_inc_per_mb * (((sustained_ckt1+sustained_ckt2)*phy_speed/100) - 2*cost_model$hicap_minbw)

cost_rd_ckt <- plot_ly(data=cost_model, x=2 * cost_model$hicap_minbw, y=2 * cost_model$flat_cost, name = "Flat Model", type = "scatter", mode = "markers+lines")%>% 
  add_trace(y=2 * cost_model$burstable_cost_sust_rate, name = "Burstable model")%>%
  add_trace(y=costo_hicap, name = "HiCap Flex")%>%
  add_trace(y=rep(btot, length(cost_model$hicap_minbw)), name = "Burstable Cost", mode="line")%>%
  add_trace(y=2*rep(cost_model$flat_cost[which(cost_model$hicap_minbw == phy_speed)], length(cost_model$hicap_minbw)), name = "Flat Cost 2x OC12", mode="line")
#layout(xaxis = ax, yaxis = ay, showlegend = TRUE)
cost_rd_ckt

which(cost_model$hicap_minbw == phy_speed)
