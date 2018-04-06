#########plotting individual vs Quality############# 
library(ggplot2)
wine_data=read.csv(file = "C://Users/gandixit/Desktop/wine/wine_dataset.csv",header = T)
str(wine_data)




#fixed_acidity
ggplot(data=wine_data, aes(x=as.numeric(quality), y=fixed_acidity)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$fixed_acidity, linetype='longdash', alpha=.5, color='blue') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("fixed_acidity ")


#volatile_acidity
ggplot(data=wine_data, aes(x=as.numeric(quality), y=volatile_acidity)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$volatile_acidity, linetype='longdash', alpha=.5, color='blue') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("Volatile Acidity")




#citric_acid
ggplot(data=wine_data, aes(x=as.numeric(quality), y=citric_acid)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$citric_acid, linetype='longdash', alpha=.5, color='blue') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("citric_acid")


#residual_sugar
ggplot(data=wine_data, aes(x=as.numeric(quality), y=residual_sugar)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$residual_sugar, linetype='longdash', alpha=.5, color='blue') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("residual_sugar")




#chlorides
ggplot(data=wine_data, aes(x=as.numeric(quality), y=chlorides)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$chlorides, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("chlorides")


#free_sulfur_dioxide
ggplot(data=wine_data, aes(x=as.numeric(quality), y=free_sulfur_dioxide)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$free_sulfur_dioxide, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("free_sulfur_dioxide")



#total_sulfur_dioxide
ggplot(data=wine_data, aes(x=as.numeric(quality), y=total_sulfur_dioxide)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$total_sulfur_dioxide, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("total_sulfur_dioxide")




#density
ggplot(data=wine_data, aes(x=as.numeric(quality), y=density)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$density, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("density")




#pH
ggplot(data=wine_data, aes(x=as.numeric(quality), y=pH)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$pH, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("pH")



#sulphates
ggplot(data=wine_data, aes(x=as.numeric(quality), y=sulphates)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$sulphates, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("sulphates")





#pH
ggplot(data=wine_data, aes(x=as.numeric(quality), y=alcohol)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$alcohol, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$quality, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("alcohol")



#pH
ggplot(data=wine_data, aes(x=as.numeric(quality), y=alcohol)) +
  geom_jitter(alpha=1/3) +
  geom_smooth(method='lm', aes(group = 1))+
  geom_hline(yintercept=wine_data$alcohol, linetype='longdash', alpha=.5, color='grey') +
  geom_vline(xintercept = wine_data$style, linetype='longdash', color='blue', alpha=.5) +
  xlab("Wine Quality") +
  ylab("alcohol")










