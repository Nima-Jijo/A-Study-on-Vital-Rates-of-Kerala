
# Load necessary libraries
library(readr)
options(max.print=1000000)

# Read dataset
df = read.csv("C:\\Users\\DELL\\Downloads\\dataset (2).csv")

# Filter birth and death rate data
Birth_rate = df[df$Indicator.Name == "Birth rate", c(1:5)]
Idukki = Birth_rate[Birth_rate$District == "Idukki", c(2,4)]
Death_Rate = df[df$Indicator.Name == "Death Rate", c(1:5)]
Idukki = Death_Rate[Death_Rate$District == "Idukki", c(2,4)]

# Summary statistics
summary(Idukki)
summary(Ernakulam)
summary(Thiruvananthapuram)
summary(Kollam)
summary(Pathanamthitta)
summary(Alappuzha)
summary(Kottayam)
summary(Malappuram)
summary(Kozhikode)
summary(Kannur)
summary(Wayanad)
summary(Kasaragod)
summary(Thrissur)
summary(Palakkad)

# Multiple bar diagram for birth rate
district_order <- c("Kasaragod", "Kannur", "Wayanad", "Kozhikode", "Malappuram",
                    "Palakkad", "Thrissur", "Ernakulam", "Idukki", "Kottayam",
                    "Alappuzha", "Pathanamthitta", "Kollam", "Thiruvananthapuram")
District <- factor(Birth_rate$District, levels = district_order)

Birth_rate_2005 = df[df$Indicator.Name == "Birth rate" & df$Calendar.Year == "2005", c(1:5)]
Value_2005 = Birth_rate_2005$Indicator.Value
data1 <- data.frame("District" = District, "Value" = Value_2005)
data1 <- data1[1:14,]

Birth_rate_2020 = df[df$Indicator.Name == "Birth rate" & df$Calendar.Year == "2020", c(1:5)]
Value_2020 = Birth_rate_2020$Indicator.Value
data2 <- data.frame("District" = District, "Value" = Value_2020)
data2 <- data2[211:224,]

# Combine data for plotting
data_combined <- cbind(data1, data2$Value)
colnames(data_combined) <- c("District", "2005", "2020")
data_combined_long <- tidyr::gather(data_combined, Year, Value, -District)

# Plot birth rate comparison
ggplot(data = data_combined_long, aes(x = District, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  labs(title = "Birth rate of districts", y = "per 1000 population", x = "District") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))

# Time series for Idukki
Idukki_Birth_Rate = Birth_rate[Birth_rate$District == "Idukki", c(1:5)]
Idukki_Death_Rate = Death_Rate[Death_Rate$District == "Idukki", c(1:5)]

ggplot(Idukki_Birth_Rate, aes(x = `Calendar.Year`, y = `Indicator.Value`)) +  
  geom_line(color = "blue") + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  labs(x = "Year", y = "Rate per 1000 population", title = "Birth rate of Idukki") + 
  annotate("text", x = 2011, y = 12, label = paste0("Equation: y = ",
    round(coef(lm(Indicator.Value ~ Calendar.Year, data = Idukki_Birth_Rate))[2], 2), "x + ",
    round(coef(lm(Indicator.Value ~ Calendar.Year, data = Idukki_Birth_Rate))[1], 2)))

ggplot(Idukki_Death_Rate, aes(x = `Calendar.Year`, y = `Indicator.Value`)) +  
  geom_line(color = "blue") + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  labs(x = "Year", y = "Rate per 1000 population", title = "Death rate of Idukki") + 
  annotate("text", x = 2011, y = 7, label = paste0("Equation: y = ",
    round(coef(lm(Indicator.Value ~ Calendar.Year, data = Idukki_Death_Rate))[2], 2), "x + ",
    round(coef(lm(Indicator.Value ~ Calendar.Year, data = Idukki_Death_Rate))[1], 2)))

# Regression models
model1 <- lm(Indicator.Value ~ Calendar.Year, data = Idukki_birth_rate)
summary(model1)
new_data1 <- data.frame(Calendar.Year = 2021:2025)
predicted_values <- predict(model1, newdata = new_data1)

model2 <- lm(Indicator.Value ~ Calendar.Year, data = Idukki_death_rate)
summary(model2)
new_data2 <- data.frame(Calendar.Year = 2021:2025)
predicted_values <- predict(model2, newdata = new_data2)

model3 <- lm(Indicator.Value ~ Calendar.Year, data = Idukki_infant_mortality_rate)
summary(model3)
new_data3 <- data.frame(Calendar.Year = 2021:2025)
predicted_values <- predict(model3, newdata = new_data3)

model4 <- lm(Indicator.Value ~ Calendar.Year, data = Idukki_still_birth_rate)
summary(model4)
new_data4 <- data.frame(Calendar.Year = 2021:2025)
predicted_values <- predict(model4, newdata = new_data4)

model5 <- lm(Indicator.Value ~ Calendar.Year, data = Idukki_maternal_mortality_ratio)
summary(model5)
new_data5 <- data.frame(Calendar.Year = 2021:2025)
predicted_values <- predict(model5, newdata = new_data5)

# Clustering
year2005new1 = year2005new[c(2:6)]
vital = as.matrix(year2005new1)
names = c("Thiruvananthapuram", "Kollam", "Pathanamthitta", "Alappuzha", "Kottayam", "Idukki",
          "Ernakulam", "Thrissur", "Palakkad", "Malappuram", "Kozhikode", "Wayanad", "Kannur", "Kasaragod")
rownames(vital) = names

# Scaling function
scale.data.frame = function(dfr) {
  if (!is.data.frame(dfr)) stop(paste(deparse(substitute(dfr)), "must be a data frame"))
  x = dfr
  cols = sapply(dfr, is.numeric)
  scaledvars = scale.default(dfr[, cols])
  x[, cols] = scaledvars
  attr(x, "scaled:center") = attr(scaledvars, "scaled:center")
  attr(x, "scaled:scale") = attr(scaledvars, "scaled:scale")
  return(x)
}

dat <- scale.data.frame(as.data.frame(vital))
distance <- get_dist(dat)  # requires 'factoextra'
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# K-means clustering
k2 <- kmeans(dat, centers = 3, nstart = 25)
fviz_cluster(k2, data = dat, repel = TRUE, labelsize = 10, plotsize = 1)
fviz_nbclust(dat, kmeans, method = "wss")

# Correlation matrix
cor <- cor(dat)  # requires 'ggcorrplot'
p.mat <- cor_pmat(dat)
ggcorrplot(cor, p.mat = p.mat, insig = "blank", tl.cex = 5, tl.srt = 90)

# Line graph for cluster means
library(ggplot2)
year2005new1$cluster <- k2$cluster
cluster_means <- aggregate(year2005new1[, 1:5], by = list(year2005new1$cluster), FUN = mean)
cluster_means_long <- reshape2::melt(cluster_means, id.vars = "Group.1")

ggplot(data = cluster_means_long, aes(x = variable, y = value, group = Group.1, color = factor(Group.1))) +
  geom_line() +
  labs(x = "Variable", y = "Mean value", color = "Cluster") +
  theme_minimal()
