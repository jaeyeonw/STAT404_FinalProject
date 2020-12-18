#STAT 404 Project
#Group 3 - Jaeyeon Won, Coskun Erden, Wanying Fu

library(psych)
library(car)
library(interactions)
library(olsrr)
library(GGally)
library(ggplot2)

project<-read.csv(file.choose(), header=T)

#################################################################################################
#Analyses for Full model: Total ~ Age + Age2 + Acres + SqFt + BathsNew + StoryNew + StoryNew*SqFt
#################################################################################################

##Descriptive Statistic
hist(project$Total) #Skewed
hist(project$Age) #Skewed
hist(project$Acres) #Skewed
hist(project$SqFt) #Skewed
describe(project, IQR = T)


##Scatterplot and Correlation Matrix
ggpairs(project[,c( 2, 4, 10, 7)], diag = list(continuous = wrap("barDiag")))+theme_bw()+ 
  labs(title="Scatterplot and Correlation Matrix")+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))


##Force categorical variables to be factors.
project$StoryNew <- as.factor(project$StoryNew)
project$BathsNew <- as.factor(project$BathsNew)

       
##Full Model
modelproject <- lm(Total ~ Age + Age2 + Acres + SqFt + BathsNew + StoryNew + StoryNew*SqFt , data=project)
summary(modelproject)


##Type I Sums of Squares 
anova(modelproject)


##Type II Sums of Squares  
Anova(modelproject)


##CI for slopes
confint(modelproject, level = 0.95)


##Checking Assumptions for Full Model
outProject = fortify(modelproject)

#Residual Plot
ggplot(outProject, aes(x=.fitted, y=.stdresid)) + 
  geom_point(shape=16, size=3) + 
  labs(x = "Predicted Total",y="Standardized Residuals",title="Residual Plot") +
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+    
  theme(axis.title.x = element_text(size = rel(1.4)))+    
  theme(axis.text.x = element_text(size = rel(1.6)))+    
  theme(axis.text.y = element_text(size = rel(1.6)))    

##Levene's Test
outProject$yHatCategory <- ifelse(outProject$.fitted < median(outProject$.fitted), c("group1"), c("group2")) 
outProject$yHatCategory <- factor(outProject$yHatCategory, levels = c("group1", "group2"))
leveneTest(.resid ~ yHatCategory, data=outProject)

##Normal QQ Plot
qqnorm(outProject$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(outProject$.resid)  ##Adds line to plot

#Histogram
hist(outProject$.resid, main="Histogram of Residuals", xlab="Residuals")

##Shapiro-Wilk Test
shapiro.test(outProject$.resid)


##Compute VIF's.
vif(modelproject)


##Create plot with leverage values on x-axis and STANDARDIZED residuals on 
##y-axis, size by cooks d
###Leverage cut-offs:
lev.cutoff.low<-2*(8+1)/99
lev.cutoff.high<-3*(8+1)/99

ggplot(outProject, aes(x=.hat, y=.stdresid, size=.cooksd)) + geom_point() + 
  labs(x = "Leverage Values",y="Standardized Residuals",title="Influence Plot")+
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_hline(yintercept=2, colour="blue")+
  geom_hline(yintercept=-2, colour="blue")+
  geom_hline(yintercept=3, colour="red")+
  geom_hline(yintercept=-3, colour="red")+
  guides(size=guide_legend(title="Cook's D"))+
  geom_vline(xintercept=lev.cutoff.low, colour="blue")+
  geom_vline(xintercept=lev.cutoff.high, colour="red")


##Find all possible models
comparison<-ols_step_all_possible(modelproject)
View(comparison)


##Forward Selection 
ols_step_forward_p(modelproject, penter = 0.05, details=TRUE)  


##Backward Elimination
ols_step_backward_p(modelproject, prem = 0.05, details=TRUE)  




########################################################
#Analyses for Reduced model: Total ~ Age + Age^2 + SqFt
########################################################


##Reduced Model
modelproject2 <- lm(Total~Age+ Age2+ SqFt, data=project)
summary(modelproject2)


#Partial F test 
anova(modelproject2, modelproject)


##Type I Sums of Squares 
anova(modelproject2)


##Type II Sums of Squares 
Anova(modelproject2)


##CI for slopes
confint(modelproject2, level = 0.95)


##Checking Assumptions
outProject2 = fortify(modelproject2)

##Residual Plot
ggplot(outProject2, aes(x=.fitted, y=.stdresid)) + 
  geom_point(shape=16, size=3) + 
  labs(x = "Predicted Total",y="Standardized Residuals",title="Residual Plot") +
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+    
  theme(axis.title.x = element_text(size = rel(1.4)))+    
  theme(axis.text.x = element_text(size = rel(1.6)))+    
  theme(axis.text.y = element_text(size = rel(1.6)))    

##Levene's Test
outProject2$yHatCategory <- ifelse(outProject2$.fitted < median(outProject2$.fitted), c("group1"), c("group2")) 
outProject2$yHatCategory <- factor(outProject2$yHatCategory, levels = c("group1", "group2"))
leveneTest(.resid ~ yHatCategory, data=outProject2)

##Normal QQ Plot
qqnorm(outProject2$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(outProject2$.resid)  

#Histogram
hist(outProject2$.resid, main="Histogram of Residuals", xlab="Residuals")

##Shapiro-Wilk Test
shapiro.test(outProject2$.resid)


##Compute VIF's.
vif(modelproject2)


##Create plot with leverage values on x-axis and STANDARDIZED residuals on 
##y-axis, size by cooks d
###Leverage cut-offs:
lev.cutoff.low<-2*(3+1)/99
lev.cutoff.high<-3*(3+1)/99

ggplot(outProject2, aes(x=.hat, y=.stdresid, size=.cooksd)) + geom_point() + 
  labs(x = "Leverage Values",y="Standardized Residuals",title="Influence Plot")+
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_hline(yintercept=2, colour="blue")+
  geom_hline(yintercept=-2, colour="blue")+
  geom_hline(yintercept=3, colour="red")+
  geom_hline(yintercept=-3, colour="red")+
  guides(size=guide_legend(title="Cook's D"))+
  geom_vline(xintercept=lev.cutoff.low, colour="blue")+
  geom_vline(xintercept=lev.cutoff.high, colour="red")




##############################################################
#Analyses for the Final model: ln(Total) ~ Age + Age^2 + SqFt
##############################################################


##Final Model
modelprojectfinal <- lm(Totalln ~ Age + Age2 + SqFt, data=project)
summary(modelprojectfinal)


##Type I Sums of Squares 
anova(modelprojectfinal)


##Type II Sums of Squares 
Anova(modelprojectfinal)


##CI for slopes
confint(modelprojectfinal, level = 0.95)


##Checking Assumptions
outProjectfinal = fortify(modelprojectfinal)

ggplot(outProjectfinal, aes(x=.fitted, y=.stdresid)) + 
  geom_point(shape=16, size=3) + 
  labs(x = "Predicted Total",y="Standardized Residuals",title="Residual Plot") +
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+    
  theme(axis.title.x = element_text(size = rel(1.4)))+    
  theme(axis.text.x = element_text(size = rel(1.6)))+    
  theme(axis.text.y = element_text(size = rel(1.6)))    

##Normal QQ Plot
qqnorm(outProjectfinal$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(outProjectfinal$.resid)  

#Histogram
hist(outProjectfinal$.resid, main="Histogram of Residuals", xlab="Residuals")

##Levene test for Homoskedasticity
outProjectfinal$yHatCategory <- ifelse(outProjectfinal$.fitted < median(outProjectfinal$.fitted), c("group1"), c("group2")) 
outProjectfinal$yHatCategory <- factor(outProjectfinal$yHatCategory, levels = c("group1", "group2"))
leveneTest(.resid ~ yHatCategory, data=outProjectfinal)

##Shapiro-Wilk test for Normality of the residuals
shapiro.test(outProjectfinal$.resid)

##Compute VIF's.
vif(modelprojectfinal)


##Create plot with leverage values on x-axis and STANDARDIZED residuals on 
##y-axis, size by cooks d
###Leverage cut-offs:
lev.cutoff.low<-2*(3+1)/99
lev.cutoff.high<-3*(3+1)/99

ggplot(outProjectfinal, aes(x=.hat, y=.stdresid, size=.cooksd)) + geom_point() + 
  labs(x = "Leverage Values",y="Standardized Residuals",title="Influence Plot")+
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_hline(yintercept=2, colour="blue")+
  geom_hline(yintercept=-2, colour="blue")+
  geom_hline(yintercept=3, colour="red")+
  geom_hline(yintercept=-3, colour="red")+
  guides(size=guide_legend(title="Cook's D"))+
  geom_vline(xintercept=lev.cutoff.low, colour="blue")+
  geom_vline(xintercept=lev.cutoff.high, colour="red")


#############################################################################################################################
#Analyses after removing 2 very unusual points (with projectdata2 dataset)for the final model: ln(Total) ~ Age + Age^2 + SqFt
#############################################################################################################################

projectdata2<-read.csv(file.choose(), header=T)


##Force categorical variables to be factors
projectdata2$StoryNew <- as.factor(projectdata2$StoryNew)
projectdata2$BathsNew <- as.factor(projectdata2$BathsNew)

##Final model after removing 2 points
modelprojectfinal2 <- lm(Totalln~Age+ Age2+ SqFt, data=projectdata2)
summary(modelprojectfinal2)


##Type I Sums of Squares 
anova(modelprojectfinal2)


##Type II Sums of Squares 
Anova(modelprojectfinal2)


##CI for slopes
confint(modelprojectfinal2, level = 0.95)


##Checking Assumptions
outProjectfinal2 = fortify(modelprojectfinal2)

ggplot(outProjectfinal2, aes(x=.fitted, y=.stdresid)) + 
  geom_point(shape=16, size=3) + 
  labs(x = "Predicted Total",y="Standardized Residuals",title="Residual Plot") +
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+    
  theme(axis.title.x = element_text(size = rel(1.4)))+    
  theme(axis.text.x = element_text(size = rel(1.6)))+    
  theme(axis.text.y = element_text(size = rel(1.6)))    

##Normal QQ Plot
qqnorm(outProjectfinal2$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(outProjectfinal2$.resid)
#Histogram
hist(outProjectfinal2$.resid, main="Histogram of Residuals", xlab="Residuals")

##Levene test for Homoskedasticity
outProjectfinal2$yHatCategory <- ifelse(outProjectfinal2$.fitted < median(outProjectfinal2$.fitted), c("group1"), c("group2")) 
outProjectfinal2$yHatCategory <- factor(outProjectfinal2$yHatCategory, levels = c("group1", "group2"))
leveneTest(.resid ~ yHatCategory, data=outProjectfinal2)

##Shapiro-Wilk test for Normality of the residuals
shapiro.test(outProjectfinal2$.resid)

##Compute VIF's.
vif(modelprojectfinal2)


##Create plot with leverage values on x-axis and STANDARDIZED residuals on 
##y-axis, size by cooks d
###Leverage cut-offs:
lev.cutoff.low<-2*(3+1)/97
lev.cutoff.high<-3*(3+1)/97

ggplot(outProjectfinal2, aes(x=.hat, y=.stdresid, size=.cooksd)) + geom_point() + 
  labs(x = "Leverage Values",y="Standardized Residuals",title="Influence Plot")+
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_hline(yintercept=2, colour="blue")+
  geom_hline(yintercept=-2, colour="blue")+
  geom_hline(yintercept=3, colour="red")+
  geom_hline(yintercept=-3, colour="red")+
  guides(size=guide_legend(title="Cook's D"))+
  geom_vline(xintercept=lev.cutoff.low, colour="blue")+
  geom_vline(xintercept=lev.cutoff.high, colour="red")



