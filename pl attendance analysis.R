#loading data and viewing the dataset
data= read.csv("C:\\Users\\abhik\\Desktop\\STATISTICS\\Projects\\Premier League historical data.csv")
View(data)
dim(data)

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(mgcv)
library(nlme)
library(gridExtra)

#variable description
str(data)

#converting to factors
data$CUP.WINS= factor(data$CUP.WINS)
data$TIER= factor(data$TIER)
data$RENOVATION= factor(data$RENOVATION)

#extracting end years from season
data$years <- as.numeric(sub(".*/", "", data$SEASON))

#define attendance to capacity ratio as it gives a better measure of how full the stadiums are each season
data$ATT_CAP_RATIO <- data$ATTENDANCE / data$STADIUM.CAPACITY


attach(data)
library(gridExtra)
library(ggplot2)

#presenting some basic summary statistics
summary(data[ , sapply(data, is.numeric)])

#Team wise
#average attendance of all teams
avg_attendance <- aggregate(ATTENDANCE ~ TEAM, data = data, FUN = mean)
avg_attendance$TEAM <- factor(avg_attendance$TEAM, levels = avg_attendance$TEAM[order(avg_attendance$ATTENDANCE)])

p1=ggplot(avg_attendance, aes(x = TEAM, y = ATTENDANCE, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("blue", "lightblue", "pink", "orange", "brown", "red")) +  # example colors
  labs(title = "Average Attendance per Team", x = "Team", y = "Average Attendance") +
  theme_minimal() +
  theme(legend.position = "none")


#avergae stadium capacity
avg_capacity <- aggregate(STADIUM.CAPACITY ~ TEAM, data = data, FUN = mean)
avg_capacity$TEAM <- factor(avg_capacity$TEAM, levels = avg_capacity$TEAM[order(avg_capacity$STADIUM.CAPACITY)])

p2=ggplot(avg_capacity, aes(x = TEAM, y = `STADIUM.CAPACITY`, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("pink", "blue", "lightblue", "brown", "orange", "red")) +  # example colors
  labs(title = "Average Stadium Capacity per Team", x = "Team", y = "Average Stadium Capacity") +
  theme_minimal() +
  theme(legend.position = "none")

#average ticket prices
avg_tp= aggregate(TICKET.PRICES..in.Pounds. ~ TEAM, data = data, FUN = mean)
avg_tp$TEAM <- factor(avg_tp$TEAM, levels = avg_tp$TEAM[order(avg_tp$TICKET.PRICES..in.Pounds.)])

p3= ggplot(avg_tp, aes(x = TEAM, y = `TICKET.PRICES..in.Pounds.`, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("lightblue", "pink", "orange", "blue", "brown", "red")) +  # example colors
  labs(title = "Average TICKET PRICE (in Pounds) per Team", x = "Team", y = "Average TICKET PRICE (in Pounds)") +
  theme_minimal() +
  theme(legend.position = "none")

#average squad value
df= data
df=na.omit(df)

avg_sqv= aggregate(SQUAD.VALUE..in.millions.of.Pounds. ~ TEAM, data = df, FUN = mean)
avg_sqv$TEAM <- factor(avg_sqv$TEAM, levels = avg_sqv$TEAM[order(avg_sqv$SQUAD.VALUE..in.millions.of.Pounds.)])

p4=ggplot(avg_sqv, aes(x = TEAM, y = `SQUAD.VALUE..in.millions.of.Pounds.`, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("pink", "orange", "blue", "lightblue", "brown", "red")) +  # example colors
  labs(title = "Average SQUAD VALUE (in millions of Pounds) per Team", x = "Team", y = "Average SQUAD VALUE (in millions of Pounds)") +
  theme_minimal() +
  theme(legend.position = "none")

#avergae attendance to capacity ratio
avg_atr <- aggregate(ATT_CAP_RATIO ~ TEAM, data = data, FUN = mean)
avg_atr$TEAM <- factor(avg_atr$TEAM, levels = avg_atr$TEAM[order(avg_atr$ATT_CAP_RATIO)])

p5=ggplot(avg_atr, aes(x = TEAM, y = `ATT_CAP_RATIO`, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("lightblue", "blue", "red", "orange", "brown", "pink")) +  # example colors
  labs(title = "Average Attendance to Stadium Capacity Ratio per Team", x = "Team", y = "Average Attendance to Stadium Capacity Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

#avergae no of matches televised
avg_tv <- aggregate(MATCHES.TELEVISED ~ TEAM, data = data, FUN = mean)
avg_tv$TEAM <- factor(avg_tv$TEAM, levels = avg_tv$TEAM[order(avg_tv$MATCHES.TELEVISED)])

p6=ggplot(avg_tv, aes(x = TEAM, y = `MATCHES.TELEVISED`, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("lightblue", "orange", "blue", "pink", "brown", "red")) +  # example colors
  labs(title = "Average No.of Matches televised per Team", x = "Team", y = "Average No.of Matches televised") +
  theme_minimal() +
  theme(legend.position = "none")



grid.arrange(p1, p2, p3, p4,p5,p6, nrow = 3, ncol = 2)


#Region wise
#average attendance
library(ggplot2)
avg_attendance <- aggregate(ATTENDANCE ~ REGION, data = data, FUN = mean)
avg_attendance$REGION <- factor(avg_attendance$REGION, levels = avg_attendance$REGION[order(avg_attendance$ATTENDANCE)])

p1=ggplot(avg_attendance, aes(x = REGION, y = ATTENDANCE, fill = REGION)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("blue", "brown", "red")) +  # example colors
  labs(title = "Average Attendance per Region", x = "Region", y = "Average Attendance") +
  theme_minimal() +
  theme(legend.position = "none")


#avergae stadium capacity
avg_capacity <- aggregate(STADIUM.CAPACITY ~ REGION, data = data, FUN = mean)
avg_capacity$REGION <- factor(avg_capacity$REGION, levels = avg_capacity$REGION[order(avg_capacity$STADIUM.CAPACITY)])

p2=ggplot(avg_capacity, aes(x = REGION, y = `STADIUM.CAPACITY`, fill = REGION)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("blue", "brown", "red")) +  # example colors
  labs(title = "Average Stadium Capacity per Region", x = "Region", y = "Average Stadium Capacity") +
  theme_minimal() +
  theme(legend.position = "none")

#average ticket prices
avg_tp= aggregate(TICKET.PRICES..in.Pounds. ~ REGION, data = data, FUN = mean)
avg_tp$REGION <- factor(avg_tp$REGION, levels = avg_tp$REGION[order(avg_tp$TICKET.PRICES..in.Pounds.)])

p3=ggplot(avg_tp, aes(x = REGION, y = `TICKET.PRICES..in.Pounds.`, fill = REGION)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue", "brown")) +  # example colors
  labs(title = "Average TICKET PRICE (in Pounds) per Region", x = "Region", y = "Average TICKET PRICE (in Pounds)") +
  theme_minimal() +
  theme(legend.position = "none")

#average Population
avg_sqv= aggregate(POPULATION ~ REGION, data = data, FUN = mean)
avg_sqv$REGION <- factor(avg_sqv$REGION, levels = avg_sqv$REGION[order(avg_sqv$POPULATION)])

p4=ggplot(avg_sqv, aes(x = REGION, y = `POPULATION`, fill = REGION)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("brown", "red", "blue")) +  # example colors
  labs(title = "Average POPULATION per REGION", x = "Region", y = "Average POPULATION") +
  theme_minimal() +
  theme(legend.position = "none")

#avergae attendance to capacity ratio
avg_atr <- aggregate(ATT_CAP_RATIO ~ REGION, data = data, FUN = mean)
avg_atr$REGION <- factor(avg_atr$REGION, levels = avg_atr$REGION[order(avg_atr$ATT_CAP_RATIO)])

p5=ggplot(avg_atr, aes(x = REGION, y = `ATT_CAP_RATIO`, fill = REGION)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue","brown")) +  # example colors
  labs(title = "Average Attendance to Stadium Capacity Ratio per Region", x = "Region", y = "Average Attendance to Stadium Capacity Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

#avergae no of matches televised
avg_tv <- aggregate(MATCHES.TELEVISED ~ REGION, data = data, FUN = mean)
avg_tv$REGION <- factor(avg_tv$REGION, levels = avg_tv$REGION[order(avg_tv$MATCHES.TELEVISED)])

p6=ggplot(avg_tv, aes(x = REGION, y = `MATCHES.TELEVISED`, fill = REGION)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red","brown")) +  # example colors
  labs(title = "Average No.of Matches televised per Region", x = "Region", y = "Average No.of Matches televised") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p1, p2, p3, p4,p5,p6, nrow = 3, ncol = 2)


#impact of Premier League (Founded in 1992)

pre_pl=data[data$years<1990,]
pl=data[data$years>=1990,]

#Before and after PL comaparison

#average match attendance across teams
avg_attendance <- aggregate(ATTENDANCE ~ TEAM, data = pre_pl, FUN = mean)
avg_attendance$TEAM <- factor(avg_attendance$TEAM, levels = avg_attendance$TEAM[order(avg_attendance$ATTENDANCE)])

p1=ggplot(avg_attendance, aes(x = TEAM, y = ATTENDANCE, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("lightblue", "blue", "orange", "pink", "brown", "red")) +  # example colors
  labs(title = "Pre Premier League Era", x = "Team", y = "Average Attendance") +
  theme_minimal() +
  theme(legend.position = "none")

avg_attendance <- aggregate(ATTENDANCE ~ TEAM, data = pl, FUN = mean)
avg_attendance$TEAM <- factor(avg_attendance$TEAM, levels = avg_attendance$TEAM[order(avg_attendance$ATTENDANCE)])

p2=ggplot(avg_attendance, aes(x = TEAM, y = ATTENDANCE, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("pink", "blue", "lightblue", "brown", "orange", "red")) +  # example colors
  labs(title = "Premier League Era", x = "Team", y = "Average Attendance") +
  theme_minimal() +
  theme(legend.position = "none")


#avergae attendance to capacity ratio
avg_atr <- aggregate(ATT_CAP_RATIO ~ TEAM, data = pre_pl, FUN = mean)
avg_atr$TEAM <- factor(avg_atr$TEAM, levels = avg_atr$TEAM[order(avg_atr$ATT_CAP_RATIO)])

p3=ggplot(avg_atr, aes(x = TEAM, y = `ATT_CAP_RATIO`, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "lightblue", "orange", "blue", "brown", "pink")) +  # example colors
  labs(title = "Pre Premier League Era", x = "Team", y = "Average Attendance to Stadium Capacity Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

avg_atr <- aggregate(ATT_CAP_RATIO ~ TEAM, data = pl, FUN = mean)
avg_atr$TEAM <- factor(avg_atr$TEAM, levels = avg_atr$TEAM[order(avg_atr$ATT_CAP_RATIO)])

p4=ggplot(avg_atr, aes(x = TEAM, y = `ATT_CAP_RATIO`, fill = TEAM)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("blue", "lightblue", "pink", "brown", "orange", "red")) +  # example colors
  labs(title = "Premier League Era", x = "Team", y = "Average Attendance to Stadium Capacity Ratio") +
  theme_minimal() +
  theme(legend.position = "none")


grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


#some boxplots
layout(matrix(c(1, 2,
                3, 3), 
              nrow = 2, byrow = TRUE))

boxplot(ATTENDANCE ~ CUP.WINS, 
        data = data,
        main = "Attendance by Cup Wins", 
        xlab = "Cup Wins", 
        ylab = "Attendance",
        names = c("No", "Yes"),           
        col = c("tomato", "skyblue"))     

# Subset the data for Tier 1 and Tier 2 only
df_tier12 <- subset(data, TIER %in% c(1, 2))
df_tier12$TIER <- factor(df_tier12$TIER, 
                         levels = c(1, 2), 
                         labels = c("Tier 1", "Tier 2"))

boxplot(ATTENDANCE ~ TIER, 
        data = df_tier12, 
        main = "Attendance by League Tier", 
        xlab = "League Tier", 
        ylab = "Attendance",
        col = c("gold", "#C0C0C0"))

data$LEAGUE.POSITION <- as.numeric(as.character(data$LEAGUE.POSITION))
data$POSITION.CATEGORY <- with(data, ifelse(LEAGUE.POSITION >= 1 & LEAGUE.POSITION <= 5, "Top 5",
                                     ifelse(LEAGUE.POSITION >= 6 & LEAGUE.POSITION <= 17, "Mid Table",
                                     ifelse(LEAGUE.POSITION >= 18 & LEAGUE.POSITION <= 22, "Relegation", NA))))
data$POSITION.CATEGORY <- factor(data$POSITION.CATEGORY,
                                 levels = c("Top 5", "Mid Table", "Relegation"))

boxplot(ATTENDANCE ~ POSITION.CATEGORY, data = data,
        main = "Match Attendance by League Position Category",
        xlab = "League Position",
        ylab = "Attendance",
        col = c("skyblue", "lightgreen", "tomato"))



# correlation heat map
library(ggcorrplot)

numeric_vars <- data[sapply(data, is.numeric)]
numeric_vars <- numeric_vars[ , !names(numeric_vars) %in% c("years")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")

ggcorrplot(cor_matrix,
           method = "circle",        
           lab = TRUE,               
           lab_size = 5,             # Increase or decrease for fitting
           colors = c("magenta", "gray", "green"),  
           title = "Correlation Heatmap",
           tl.cex = 10) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



#time series analysis
#dividing the dataset wrt to each team
Chelsea<- data[data$TEAM=="Chelsea",]
Arsenal<- data[data$TEAM=="Arsenal",]
United<- data[data$TEAM=="Manchester United",]
City<- data[data$TEAM=="Manchester City",]
Liv<- data[data$TEAM=="Liverpool",]
Spurs<- data[data$TEAM=="Tottenham",]

#plotting attendance vs year of all teams sepreately

teams <- list(Chelsea, Arsenal, Liv, United, City, Spurs)
names <- c("Chelsea", "Arsenal", "Liverpool", "Manchester United", "Manchester City", "Tottenham")
colors <- c("blue", "orange", "brown", "red", "dodgerblue", "purple")

par(mfrow = c(2, 3))
for (i in 1:6) {
  plot(1950:2019, teams[[i]]$ATTENDANCE, type = "l", col = colors[i], main = names[i], xlab = "Year", ylab = "Attendance")
  abline(v=1990,lty=2)
arrows(x0 = 1982, y0 = max(teams[[i]]$ATTENDANCE)*0.95,
       x1 = 1990, y1 = max(teams[[i]]$ATTENDANCE)*0.95,
       length = 0.1,lwd = 2)
text(x = 1981, y = max(teams[[i]]$ATTENDANCE)*0.95, labels = "PL Formed", pos = 2)
}

#plotting attendance to capacity ratio vs year of all teams sepreately
par(mfrow = c(2, 3))
for (i in 1:6) {
  plot(1950:2019, teams[[i]]$ATT_CAP_RATIO, type = "l", col = colors[i], main = names[i], xlab = "Year", ylab = "Attendance")
  abline(v=1990,lty=2)
arrows(x0 = 1982, y0 = max(teams[[i]]$ATT_CAP_RATIO)*0.95,
       x1 = 1990, y1 = max(teams[[i]]$ATT_CAP_RATIO)*0.95,
       length = 0.1,lwd = 2)
text(x = 1981, y = max(teams[[i]]$ATT_CAP_RATIO)*0.95, labels = "PL Formed", pos = 2)

}


#forecasting attendance using ARIMA model
library(forecast)

#attendance
par(mfrow = c(2, 3))
for(i in 1:6){
ts <- ts(teams[[i]]$ATTENDANCE, start=c(1950, 1), frequency=1)
arima_fit <- auto.arima(ts)

forecast_attendance <- forecast(arima_fit, h = 30)
plot(forecast_attendance, main = paste("ARIMA Forecast of",names[i],"Attendance"),
     xlab = "Year", ylab = "Attendance")
lines(ts, col = colors[i], lwd = 2) 
y_last <- tail(ts, 1)
y_first <- forecast_attendance$mean[1]
lines(c(2019, 2020), c(y_last, y_first), col = colors[i], lwd = 2)
abline(v=1990,lty=2)
arrows(x0 = 1982, y0 = max(teams[[i]]$ATTENDANCE)*0.95,
       x1 = 1990, y1 = max(teams[[i]]$ATTENDANCE)*0.95,
       length = 0.1,lwd = 2)
text(x = 1981, y = max(teams[[i]]$ATTENDANCE)*0.95, labels = "PL Formed", pos = 2)
}

#attendance to capacity ratio
par(mfrow = c(2, 3))
for(i in 1:6){
ts <- ts(teams[[i]]$ATT_CAP_RATIO, start=c(1950, 1), frequency=1)
arima_fit <- auto.arima(ts)

forecast_attendance <- forecast(arima_fit, h = 30)

plot(forecast_attendance, main = paste("ARIMA Forecast of",names[i],"Attendance to Capacity ratio"),
     xlab = "Year", ylab = "Attendance to Capacity ratio")
lines(ts, col = colors[i], lwd = 2)
y_last <- tail(ts, 1)
y_first <- forecast_attendance$mean[1]
lines(c(2019, 2020), c(y_last, y_first), col = colors[i], lwd = 2)
abline(v=1990,lty=2)
arrows(x0 = 1982, y0 = max(teams[[i]]$ATT_CAP_RATIO)*0.95,
       x1 = 1990, y1 = max(teams[[i]]$ATT_CAP_RATIO)*0.95,
       length = 0.1,lwd = 2)
text(x = 1981, y = max(teams[[i]]$ATT_CAP_RATIO)*0.95, labels = "PL Formed", pos = 2)
}



#mutiple linear regression
model2 <- lm(ATTENDANCE ~ TICKET.PRICES..in.Pounds. +
              POSITION.CATEGORY + MATCHES.TELEVISED + UK.GDP.PC..in.Pounds. +
              STADIUM.CAPACITY+ CUP.WINS +TIER+ NO..OF.GOALS.SCORED, data = data)

summary(model2)

#Goldfeld Quandt Test
library(car)
gq_test <- gqtest(model2, fraction = 1/3) 
print(gq_test)


# Extract the data actually used in the model
model_data <- model.frame(model2)

# List of predictor variable names
predictors <- c("TICKET.PRICES..in.Pounds.",
                "MATCHES.TELEVISED", "UK.GDP.PC..in.Pounds.",
                "STADIUM.CAPACITY", "NO..OF.GOALS.SCORED")

residual <- resid(model2)
par(mfrow=c(3,2))
# Loop through each predictor and plot residuals
for (i in predictors) {
  
  plot(model_data[[i]], residual,
       xlab = i,
       ylab = "Residuals",
       main = paste("Residuals vs", i))
  
  abline(h = 0, col = "red", lty = 2)
}



#GLS
library(nlme)
data_clean <- na.omit(data[, c("ATTENDANCE", "TICKET.PRICES..in.Pounds.",
                               "POSITION.CATEGORY", "MATCHES.TELEVISED",
                               "UK.GDP.PC..in.Pounds.", "STADIUM.CAPACITY",
                               "CUP.WINS", "TIER", "NO..OF.GOALS.SCORED")])

model_gls <- gls(ATTENDANCE ~ TICKET.PRICES..in.Pounds. +
                   POSITION.CATEGORY + MATCHES.TELEVISED +
                   UK.GDP.PC..in.Pounds. + STADIUM.CAPACITY +
                   CUP.WINS + TIER + NO..OF.GOALS.SCORED,
                 data = data_clean,
                 weights = varPower(form = ~ fitted(.)))  # handles heteroscedasticity

summary(model_gls)

#model comparison
AIC(model2, model_gls)
BIC(model2, model_gls)



#Principle Component Analysis
# Select variables for PCA
pca_vars <- data[, c("TICKET.PRICES..in.Pounds.", "MATCHES.TELEVISED", "UK.GDP.PC..in.Pounds.")]

pca_vars_complete <- na.omit(pca_vars)

# Scale variables (important for PCA)
pca_scaled <- scale(pca_vars_complete)

# Run PCA
pca_result <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)

# Summary to see variance explained
summary(pca_result)

# Scree plot to visualize variance explained by components
plot(pca_result, type = "lines")


# Get PC scores for first component
pc_scores <- pca_result$x[, 1]

data_clean <- na.omit(data[, c("ATTENDANCE", "TICKET.PRICES..in.Pounds.",
                               "POSITION.CATEGORY", "MATCHES.TELEVISED",
                               "UK.GDP.PC..in.Pounds.", "STADIUM.CAPACITY",
                               "CUP.WINS", "TIER", "NO..OF.GOALS.SCORED")])
# Add PC1 as a new variable in your data
data_clean$PC1 <- pc_scores

# Example linear model using PC1 instead of original 3 variables
model_pca <- lm(ATTENDANCE ~ PC1 + POSITION.CATEGORY + STADIUM.CAPACITY + CUP.WINS + TIER + NO..OF.GOALS.SCORED, data = data_clean)

# Check VIF for this model
library(car)
vif(model_pca)

summary(model_pca)


#model performance evaluation

set.seed(123)  # for reproducibility

# 1. Remove rows with NA in UK.GDP.PC..in.Pounds.
data_clean <- data[!is.na(data$UK.GDP.PC..in.Pounds.), ]

# 2. Split into train (70%) and test (30%)
sample_size <- floor(0.7 * nrow(data_clean))
train_indices <- sample(seq_len(nrow(data_clean)), size = sample_size)

train_data <- data_clean[train_indices, ]
test_data <- data_clean[-train_indices, ]

# 3. Run PCA on train_data for the 3 collinear variables
pca_vars_train <- train_data[, c("TICKET.PRICES..in.Pounds.", "MATCHES.TELEVISED", "UK.GDP.PC..in.Pounds.")]
pca_vars_train_scaled <- scale(pca_vars_train)

pca_train <- prcomp(pca_vars_train_scaled, center = TRUE, scale. = TRUE)

# Extract PC1 for train and test sets
train_data$PC1 <- pca_train$x[, 1]

# For test, scale test vars using train mean & sd, then get PC1 scores using train rotation
pca_vars_test <- test_data[, c("TICKET.PRICES..in.Pounds.", "MATCHES.TELEVISED", "UK.GDP.PC..in.Pounds.")]

# Scale test set using train mean and sd
pca_vars_test_scaled <- scale(pca_vars_test, center = attr(pca_vars_train_scaled, "scaled:center"), scale = attr(pca_vars_train_scaled, "scaled:scale"))

# Calculate PC1 for test data
test_data$PC1 <- as.numeric(as.matrix(pca_vars_test_scaled) %*% pca_train$rotation[, 1])

# 4. Fit model on train data
model_pca <- lm(ATTENDANCE ~ PC1 + POSITION.CATEGORY + STADIUM.CAPACITY + CUP.WINS + TIER + NO..OF.GOALS.SCORED, data = train_data)

# 5. Predict on test data
test_data$predicted_attendance <- predict(model_pca, newdata = test_data)

# 6. Evaluate model performance

# Calculate Residuals
residuals <- test_data$ATTENDANCE - test_data$predicted_attendance

# RMSE
rmse <- sqrt(mean(residuals^2))

# MAE
mae <- mean(abs(residuals))

# R-squared on test set
sst <- sum((test_data$ATTENDANCE - mean(test_data$ATTENDANCE))^2)
ssr <- sum(residuals^2)
r_squared_test <- 1 - (ssr / sst)

cat("Test set performance:\n")
cat("RMSE =", rmse, "\n")
cat("MAE =", mae, "\n")
cat("R-squared =", r_squared_test, "\n")

#visual plots
library(ggplot2)

ggplot(test_data, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +   # Adds actual residual density estimate
  stat_function(fun = dnorm,
                args = list(mean = mean(test_data$residuals), sd = sd(test_data$residuals)),
                color = "red", size = 1, linetype = "dashed") +
  labs(title = "Histogram of Residuals with Normal Density Curve",
       x = "Residuals",
       y = "Density") +
  theme_minimal()

p1=ggplot(test_data, aes(x = ATTENDANCE, y = predicted_attendance)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Attendance",
       x = "Actual Attendance",
       y = "Predicted Attendance") +
  theme_minimal()


test_data$residuals <- test_data$ATTENDANCE - test_data$predicted_attendance

p2=ggplot(test_data, aes(x = predicted_attendance, y = residuals)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Predicted Attendance",
       x = "Predicted Attendance",
       y = "Residuals") +
  theme_minimal()


grid.arrange(p1, p2, nrow = 1, ncol = 2)

