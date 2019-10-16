#Plotting results of mediation analysis
library(ggplot2)
library(gridExtra)
#Amyloid
data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'Age', 'ADI', 'Systolic BP', 'BMI', 'WMH'),
  mean = c(-0.19, -0.042, -0.148, -0.059, -0.044, 0.007, -0.051, -0.002),
  se = c(0.053, 0.037, 0.039, 0.025, 0.027, 0.009, 0.019, 0.005))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'Age', 'ADI', 'BMI', 'WMH', 'Systolic BP'))
data.estimates$CI_low<-data.estimates$mean - data.estimates$se
data.estimates$CI_high<-data.estimates$mean + data.estimates$se
p1<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + ggtitle("A. Race - Centiloid Mediation Analysis") +
  theme(legend.position = "none") + ylim(c(-0.33, 0.033))


#Tau
data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect',  'ADI', 'Systolic BP', 'BMI', 'WMH'),
  mean = c(-0.007, -0.008, 0.001, 0.008, -0.002, -0.007, 0),
  se = c(0.018, 0.012, 0.012, 0.008, 0.003, 0.006, 0.004))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'BMI', 'WMH', 'Systolic BP'))
data.estimates$CI_low<-data.estimates$mean - data.estimates$se
data.estimates$CI_high<-data.estimates$mean + data.estimates$se
p2<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + ggtitle("B. Race - Tau Mediation Analysis") +
  theme(legend.position = "none") + ylim(c(-0.1, 0.025))


#rsfc
data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect',  'ADI', 'Systolic BP', 'BMI', 'WMH'),
  mean = c(-0.012, 0.004, -0.016, -0.014, -0.001, -0.002, 0.001),
  se = c(0.011, 0.007, 0.008, 0.006, 0.002, 0.003, 0.003))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'BMI', 'WMH', 'Systolic BP'))
data.estimates$CI_low<-data.estimates$mean - data.estimates$se
data.estimates$CI_high<-data.estimates$mean + data.estimates$se
p3<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + ggtitle("C. Race - rsFC Mediation Analysis") +
  theme(legend.position = "none") + ylim(c(-0.1, 0.025))


#Volume
data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect',  'ADI', 'Systolic BP', 'BMI', 'WMH', 'Sex'),
  mean = c(-0.260, -0.224, -0.036, -0.033, -0.004, 0.012, -0.011, -0.003),
  se = c(0.058, 0.056, 0.023, 0.019, 0.004, 0.008, 0.010, 0.003))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'BMI', 'WMH', 'Systolic BP', 'Sex'))
data.estimates$CI_low<-data.estimates$mean - data.estimates$se
data.estimates$CI_high<-data.estimates$mean + data.estimates$se
p4<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + ggtitle("D. Race - AD Signature Volume Mediation Analysis") +
  theme(legend.position = "none") + ylim(c(-0.33, 0.033))

grid.arrange(p1, p2, p3, p4, nrow = 2)
