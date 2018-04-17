require(ggplot2)
require(gridExtra)
df <- read.csv("Icecream.csv")
p1 <- ggplot(df, aes(x=X, y=cons)) +
            ylab("Consumption") +
            xlab("") +
            geom_line() +
            expand_limits(x=0, y=0)

p2 <- ggplot(df, aes(x=X, y=temp)) +
            ylab("temperature") +
            xlab("") +
            geom_line() +
            expand_limits(x=0, y=0)

p3 <- ggplot(df, aes(x=X, y=income)) +
            ylab("Income") +
            xlab("Period") +
            geom_line() +
            expand_limits(x=0, y=0)

grid.arrange(p1, p2, p3, ncol=1, nrow=3)

require(forecast)
fit_cons <- auto.arima(df$cons)
#fcast_con <- forecast(fit_cons, h=6)
#autoplot.forecast(fcast_con)
autoplot(forecast::forecast(fit_cons, h=6))
accuracy(fit_cons)

fit_cons_temp <- auto.arima(df$cons, xreg = df$temp)
fcast_temp <- c(70.5, 66, 60.5, 45.5, 36, 28)
autoplot(forecast::forecast(fit_cons_temp, xreg=fcast_temp, h=6))

summary(forecast(fit_cons_temp, xreg=fcast_temp, h=6))
require(lmtest)
coeftest(forecast(fit_cons_temp, xreg=fcast_temp, h=6))

temp_column <- matrix(df$temp, ncol=1)
income <- c(NA, NA, df$income)
income_matrix <- embed(income, 3)
vars_matrix <- cbind(temp_column, income_matrix)
print(vars_matrix)

fit_vars_0 <- auto.arima(df$cons, xreg = vars_matrix[, 1:2])
fit_vars_1 <- auto.arima(df$cons, xreg = vars_matrix[, 1:3])
fit_vars_2 <- auto.arima(df$cons, xreg = vars_matrix[, 1:4])

print(fit_vars_0$aic)
print(fit_vars_1$aic)
print(fit_vars_2$aic)

expected_temp_income <- matrix(c(fcast_temp, 91, 91, 93, 96, 96, 96),
                              ncol = 2, nrow = 6)
#expected_temp_income <- matrix(c(fcast_temp, 91, 91, 93, 96, 96, 96), ncol=2, nrow = 6)
autoplot(forecast::forecast(fit_vars_0, xreg= expected_temp_income, h=6))
        
