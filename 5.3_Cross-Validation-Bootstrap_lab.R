library(ISLR)
set.seed(1)

train = sample(392, 196)

#linear regression fit
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)

#MSE for the linear regression
mean((mpg-predict(lm.fit, Auto)) [-train]^2)

#added quadratic term into the model
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)

#MSE for the quadratic fit
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

#cubic polynom model fit
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

#MSE cubic model
mean((mpg - predict(lm.fit3, Auto))[-train]^2)


#LOOCV
library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

cv.error = rep(0, 5)
for (i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
