setwd("C:/Users/SAJAL/Desktop/PGDDA/Predictive Analytics 1")
carmileage <- read.csv("carMPG.csv")
carmileage$Horsepower <- as.numeric(carmileage$Horsepower)

carmileage$Horsepower[is.na(carmileage$Horsepower)] <- round(mean(carmileage$Horsepower[!is.na(carmileage$Horsepower)]),0)



carmileage$Cylinders <- as.factor(carmileage$Cylinders)
carmileage$Model_year <- as.factor(carmileage$Model_year)
carmileage$Origin <- as.factor(carmileage$Origin)
###carmileage$Car_Name <- as.factor(carmileage$Car_Name)
str(carmileage)

sum(is.na(carmileage$Horsepower))

carmileage$Car_Name<- gsub("([A-Za-z]+).*", "\\1", carmileage$Car_Name)

str(carmileage)
##carmileage$Car_Name <- as.character(carmileage$Car_Name)

##str(carmileage)

dummy_1 <- data.frame(model.matrix( ~Car_Name, data = carmileage))
dummy_1<-dummy_1[,-1]

#-------------------------------------------------------------------------------------------

# For Origin
dummy_2 <- data.frame(model.matrix( ~Origin, data = carmileage))
dummy_2<-dummy_2[,-1]

#-------------------------------------------------------------------------------------------

# Model_year 

dummy_3 <- data.frame(model.matrix( ~Model_year, data = carmileage))
dummy_3<-dummy_3[,-1]

#-------------------------------------------------------------------------------------------

#Cylinders
dummy_4 <- data.frame(model.matrix( ~Cylinders, data = carmileage))
dummy_4<-dummy_4[,-1]

#-------------------------------------------------------------------------------------------


carmileage_1<- cbind(carmileage[ , -c(2,7,8,9)], dummy_1,dummy_2,dummy_3,dummy_4)


set.seed(101)
indices= sample(1:nrow(carmileage_1), 0.7*nrow(carmileage_1))

train=carmileage_1[indices,]
test = carmileage_1[-indices,]


model_1 <- lm(MPG~.,data=train[-1,])

summary(model_1)

#-------------------------------------------------------------------------------------------

#stepwise

step <- stepAIC(model_1, direction="both")

step

#-------------------------------------------------------------------------------------------
model_2 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namecadillac + 
                Car_Namedatsun + Car_Namefiat + Car_Namehonda + Car_Namemazda + 
                Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                Car_Namerenault + Car_Namevokswagen + Car_Namevolkswagen + 
                Car_Namevw + Model_year2005 + Model_year2006 + Model_year2007 + 
                Model_year2008 + Model_year2009 + Model_year2010 + Model_year2011 + 
                Model_year2012 + Model_year2013 + Model_year2014 + Model_year2015 + 
                Cylinders4 + Cylinders5 + Cylinders6 + Cylinders8, data = train[-1, ])
#--------------------------------------------------------------------------
summary(model_2)
#---------------------------------------------------------------------------
library(car)

vif(model_2)

#---------------------------------------------------------------------------
summary(model_2)
#---------------------------------------------------------------------------
#Removing Cylinder4 variable ; highly correlation between Cylinder6 & Cylindr8

model_3 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namecadillac + 
                Car_Namedatsun + Car_Namefiat + Car_Namehonda + Car_Namemazda + 
                Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                Car_Namerenault + Car_Namevokswagen + Car_Namevolkswagen + 
                Car_Namevw + Model_year2005 + Model_year2006 + Model_year2007 + 
                Model_year2008 + Model_year2009 + Model_year2010 + Model_year2011 + 
                Model_year2012 + Model_year2013 + Model_year2014 + Model_year2015 + 
                Cylinders5 + Cylinders6 + Cylinders8, data = train[-1, ])

vif(model_3)
summary(model_3)
#---------------------------------------------------------------------------
#Removing cylinder8

model_4 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namecadillac + 
                Car_Namedatsun + Car_Namefiat + Car_Namehonda + Car_Namemazda + 
                Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                Car_Namerenault + Car_Namevokswagen + Car_Namevolkswagen + 
                Car_Namevw + Model_year2005 + Model_year2006 + Model_year2007 + 
                Model_year2008 + Model_year2009 + Model_year2010 + Model_year2011 + 
                Model_year2012 + Model_year2013 + Model_year2014 + Model_year2015 + 
                Cylinders5 + Cylinders6, data = train[-1, ])

summary(model_4)

vif(model_4)
#-----------------------------------------------------------------------------
#Removing Cylinder5

model_5 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namecadillac + 
                Car_Namedatsun + Car_Namefiat + Car_Namehonda + Car_Namemazda + 
                Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                Car_Namerenault + Car_Namevokswagen + Car_Namevolkswagen + 
                Car_Namevw + Model_year2005 + Model_year2006 + Model_year2007 + 
                Model_year2008 + Model_year2009 + Model_year2010 + Model_year2011 + 
                Model_year2012 + Model_year2013 + Model_year2014 + Model_year2015 + 
                Cylinders6, data = train[-1, ])

vif(model_5)
summary(model_5)
#------------------------------------------------------------------------------
#Removing carmazda

model_6 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namecadillac + 
     Car_Namedatsun + Car_Namefiat + Car_Namehonda + 
     Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
     Car_Namerenault + Car_Namevokswagen + Car_Namevolkswagen + 
     Car_Namevw + Model_year2005 + Model_year2006 + Model_year2007 + 
     Model_year2008 + Model_year2009 + Model_year2010 + Model_year2011 + 
     Model_year2012 + Model_year2013 + Model_year2014 + Model_year2015 + 
     Cylinders6, data = train[-1, ])

vif(model_6)
summary(model_6)
#-------------------------------------------------------------------------------
#Removing carvolkswagen

model_7 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namecadillac + 
                Car_Namedatsun + Car_Namefiat + Car_Namehonda + Car_Nameoldsmobile + 
                Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Namevokswagen 
                + Car_Namevw + Model_year2005 + Model_year2006 + 
                Model_year2007 + Model_year2008 + Model_year2009 + Model_year2010 + 
                Model_year2011 + Model_year2012 + Model_year2013 + Model_year2014 + 
                Model_year2015 + Cylinders6, data = train[-1, ])

vif(model_7)
summary(model_7)
#--------------------------------------------------------------------------------
#Remoing carvw

model_8 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namecadillac + 
                Car_Namedatsun + Car_Namefiat + Car_Namehonda + Car_Nameoldsmobile + 
                Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Namevokswagen 
                + Model_year2005 + Model_year2006 + Model_year2007 + 
                Model_year2008 + Model_year2009 + Model_year2010 + Model_year2011 + 
                Model_year2012 + Model_year2013 + Model_year2014 + Model_year2015 + 
                Cylinders6, data = train[-1, ])

vif(model_8)
summary(model_8)

#----------------------------------------------------------------------------------
#Removing car_name caddillac

model_9 <- lm(formula = MPG ~ Weight + Acceleration + 
                Car_Namedatsun + Car_Namefiat + Car_Namehonda + Car_Nameoldsmobile + 
                Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Namevokswagen + 
                Model_year2005 + Model_year2006 + Model_year2007 + Model_year2008 + 
                Model_year2009 + Model_year2010 + Model_year2011 + Model_year2012 + 
                Model_year2013 + Model_year2014 + Model_year2015 + Cylinders6, 
              data = train[-1, ])

vif(model_9)
summary(model_9)
#-----------------------------------------------------------------------------------
#Remoing fiat

model_10 <- lm(formula = MPG ~ Weight + Acceleration + Car_Namedatsun + 
                 Car_Namehonda + Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                 Car_Namerenault + Car_Namevokswagen + Model_year2005 + Model_year2006 + 
                 Model_year2007 + Model_year2008 + Model_year2009 + Model_year2010 + 
                 Model_year2011 + Model_year2012 + Model_year2013 + Model_year2014 + 
                 Model_year2015 + Cylinders6, data = train[-1, ])

vif(model_10)
summary(model_10)
#-------------------------------------------------------------------------------------
#Removing

model_11 <- lm(formula = MPG ~ Weight  + Car_Namedatsun + Car_Namehonda + 
                 + Car_Namepontiac + Car_Namevokswagen + Model_year2005 + Model_year2006 + 
                 Model_year2007 + Model_year2008 + Model_year2009 + Model_year2010 + 
                 Model_year2011 + Model_year2012 + Model_year2013 + Model_year2014 + 
                 Model_year2015 + Cylinders6, data = train[-1, ])
vif(model_11)
summary(model_11)
#----------------------------------------------------------------------------------------
#Removing
 
model_12 <- lm(formula = MPG ~ Weight  + Model_year2005 +
                  Model_year2012 + Model_year2013 + 
                 Model_year2015 + Cylinders6, data = train[-1, ])

summary(model_12)
#-----------------------------------------------------------------------------------------
#Predict

Predict_1 <- predict(model_12,test[,-c(1)])
test$test_mpg <- Predict_1


cor(test$MPG,test$test_mpg)
cor(test$MPG,test$test_mpg)^2
#-----------------------------------------------------------------------------------------


