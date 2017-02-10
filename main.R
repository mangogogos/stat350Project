rm(list=ls())
setwd("~/workspace/school/stat/350-AppliedLinearRegression/project")

library(RSQLite) # for access to data
library(ggplot2) # better plotting
library(reshape) # for melting into long format

# Provide control over when the console is printed to
suppressConsole <- function() {
  sink("temp.txt")
}
resumeConsole <- function () {
  sink(NULL)
  unlink("temp.txt")
}

db <- dbConnect(dbDriver("SQLite"), "database.sqlite") # Connect to the dataset
# This stops SQLite from writing temp files to disk, which use all the available space
dbGetQuery(db, "PRAGMA temp_store=2;")

vars <- dbGetQuery(db, "
  SELECT
    INSTNM Name,
    CONTROL Type,
    TUITIONFEE_OUT Tuition,
    UGDS NumStudents,
    (DISTANCEONLY == \"Distance-education only\") IsDistance,
    PPTUG_EF PartTimeProportion,
    PCTFLOAN FederalLoanProportion,
    DEBT_MDN MedianDebt,
    GRAD_DEBT_MDN MedianDebt_Graduated,
    WDRAW_DEBT_MDN MedianDebt_Withdrew,
    LO_INC_DEBT_MDN MedianDebt_LowIncome,
    MD_INC_DEBT_MDN MedianDebt_MedIncome,
    HI_INC_DEBT_MDN MedianDebt_HighIncome,
    DEP_DEBT_MDN MedianDebt_Dependent,
    IND_DEBT_MDN MedianDebt_Independent,
    (C200_4_POOLED_SUPP * 100) CompletionRate,
    ACTCM25 CumulativeACT_25,
    ACTCM75 CumulativeACT_75
  FROM Scorecard
  WHERE Tuition > 0
    AND Year = 2013
    AND MedianDebt > 0
    AND NumStudents > 0
    AND NumStudents < 100000
    AND PartTimeProportion IS NOT NULL
    AND FederalLoanProportion IS NOT NULL
    AND CAST(CompletionRate AS decimal) > 0.01
    AND CURROPER=\"Currently certified as operating\"")

# Show distance-only colleges
print(vars[which(vars$IsDistance == 1), c("Name", "Type", "NumStudents", "CompletionRate")])

# This has the handy side-effect of removing the above distance-only colleges
vars <- vars[which(complete.cases(vars)),]

suppressConsole()

plots <- list()

NO_Y_AXIS_THEME <- theme(axis.title.y = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())

# Exploration plots

vars2 <- melt(vars[, c("MedianDebt_LowIncome", "MedianDebt_MedIncome", "MedianDebt_HighIncome")])
vars2 <- vars2[which(vars2$value > 0),]
levels(vars2$variable) <- c("MedianDebt_LowIncome" = "Low Income",
                            "MedianDebt_MedIncome" = "Medium Income",
                            "MedianDebt_HighIncome" = "High Income")
vars2$`Income Status` <- vars2$variable
vars2$variable <- NULL
vars2$`Median Debt ($)` <- vars2$value
vars2$value <- NULL
plots$MedianDebtByIncome <- ggplot(vars2, aes(x=`Median Debt ($)`,
                                              color=`Income Status`,
                                              fill=`Income Status`)) +
  geom_density(alpha=0.3) +
  NO_Y_AXIS_THEME +
  ggtitle("Density of Median Debt by Income Status")

vars2 <- melt(vars[, c("MedianDebt_Dependent", "MedianDebt_Independent")])
vars2 <- vars2[which(vars2$value > 0),]
levels(vars2$variable) <- c("MedianDebt_Dependent" = "Dependent",
                            "MedianDebt_Independent" = "Independent")
vars2$`Dependency Status` <- vars2$variable
vars2$variable <- NULL
vars2$`Median Debt ($)` <- vars2$value
vars2$value <- NULL
plots$MedianDebtByDependency <- ggplot(vars2, aes(x=`Median Debt ($)`,
                                                  color=`Dependency Status`,
                                                  fill=`Dependency Status`)) +
  geom_density(alpha=0.3) +
  NO_Y_AXIS_THEME +
  ggtitle("Density of Median Debt by Dependency Status")

vars2 <- melt(vars[, c("MedianDebt_Graduated", "MedianDebt_Withdrew")])
vars2 <- vars2[which(vars2$value > 0),]
levels(vars2$variable) <- c("MedianDebt_Graduated" = "Graduated",
                            "MedianDebt_Withdrew" = "Widthrew")
vars2$`Graduation Status` <- vars2$variable
vars2$variable <- NULL
vars2$`Median Debt ($)` <- vars2$value
vars2$value <- NULL
plots$MedianDebyByGraduated <- ggplot(vars2, aes(x=`Median Debt ($)`,
                                                 color=`Graduation Status`,
                                                 fill=`Graduation Status`)) +
  geom_density(alpha=0.3) +
  NO_Y_AXIS_THEME +
  ggtitle("Density of Median Debt by Graduation Status")

plots$NumStudents <- ggplot(vars, aes(NumStudents)) +
  geom_density() +
  NO_Y_AXIS_THEME +
  ggtitle("Density of Number of Students") +
  xlab("")

# Run simple linear regression for exploration

response <- "CompletionRate"
covariates <- c("Tuition",
                "MedianDebt",
                "FederalLoanProportion",
                "PartTimeProportion",
                "NumStudents",
                "Type",
                "CumulativeACT_25",
                "CumulativeACT_75")

simpleModels <- list()

alpha <- 0.01

INTERCEPT <- c("(Intercept)")
ESTIMATE <- "Estimate"
TVAL <- c("Pr(>|t|)")

maxResponse <- max(vars[[response]])
labelYPos1 <- 1.05 * maxResponse
labelYPos2 <- 1.10 * maxResponse

# Run some automated analysis on all numeric covariates
for (covariate in covariates) {
  simpleModel <- lm(as.formula(paste(response, "~", covariate)), data = vars)
  simpleModels[[covariate]] <- simpleModel
  simpleCoefficients <- summary(simpleModel)$coefficients
  plotName <- paste(response, "By", covariate, sep = "")

  if(is.numeric(vars[[covariate]])) {
    simpleIntercept <- simpleCoefficients[[INTERCEPT, ESTIMATE]]
    simpleSlope <- simpleCoefficients[[covariate, ESTIMATE]]
    isSimplySignificant <- simpleCoefficients[covariate, TVAL] < alpha
    
    labelXPos <- mean(range(vars[[covariate]]))

    plots[[plotName]] <- ggplot(vars, aes_string(y = response, x = covariate)) +
      geom_point() +
      geom_abline(intercept = simpleIntercept, slope = simpleSlope, colour = "red") +
      ggtitle(paste(response, "VS", covariate)) +
      annotate("text",
               x = labelXPos,
               y = labelYPos1,
               label = paste("Simple intercept:", simpleIntercept)) +
      annotate("text",
               x = labelXPos,
               y = labelYPos2,
               label = paste("Simple slope:", simpleSlope, "Significant:", isSimplySignificant)) +
      theme(legend.position="none")
  } else {
    values <- unique(vars[[covariate]])
    numUnique <- length(values)

    intercepts <- simpleCoefficients[INTERCEPT, ESTIMATE]
    otherIntercepts <- simpleCoefficients[2:numUnique, ESTIMATE]
    otherIntercepts <- simpleCoefficients[INTERCEPT, ESTIMATE] + otherIntercepts
    intercepts <- append(intercepts, otherIntercepts)

    labelXPos <- median(1:numUnique)

    isSimplySignificant <- TRUE
    for (intercept in 1:numUnique) {
      if (simpleCoefficients[intercept, TVAL] > alpha) {
        isSimplySignificant <- FALSE
        break
      }
    }
    
    plots[[plotName]] <- ggplot(vars, aes_string(y = response, x = covariate)) +
      geom_point(aes_string(colour = covariate)) +
      ggtitle(paste(response, "VS", covariate)) +
      annotate("text",
               x = labelXPos,
               y = labelYPos1,
               label = paste("Significant:", isSimplySignificant)) +
      theme(legend.position="none")
    
    # extract the colours of each value to match the hlines to
    # there's definitely a more idiomatic way to do this
    colours <- unique(ggplot_build(plots[[plotName]])$data[[1]][c("x", "colour")])

    for (intercept in 1:numUnique) {
      plots[[plotName]] <- plots[[plotName]] +
        geom_hline(yintercept = intercepts[intercept], colour = colours[colours$x == intercept,][1, "colour"])
    }
  }

  if (!isSimplySignificant) {
    # omit insignificant covariates
    covariates <- covariates[covariates != covariate]
  }
}

# Collect all relevant data
data <- vars[, append(covariates, response)]

upperModelFormula <- "~ .^2" # Include all variables and up to second-order interaction terms
upperModel <- lm(as.formula(paste(response, upperModelFormula)), data = data)
upperModelFormula <- as.formula(upperModelFormula)

lowerModelFormula <- "~ 1"
lowerModel <- lm(as.formula(paste(response, lowerModelFormula)), data = data)
lowerModelFormula <- as.formula(lowerModelFormula)

scope <- list(lower = lowerModelFormula, 
              upper = upperModelFormula)
selectedModels <- list()

# NOTE: Ran into some issues with forward AIC/BIC. They are omitted.

# selectedModels$`Forward AIC` <- step(lowerModel,
#                                     scope = scope,
#                                     direction = "forward",
#                                     data = data)

selectedModels$`Reverse AIC` <- step(upperModel,
                                     scope = scope,
                                     direction = "backward",
                                     data = data)

selectedModels$`Both AIC` <- step(upperModel,
                                  scope = scope,
                                  direction = "both",
                                  data = data)

kBIC <- log(NROW(vars))

# selectedModels$`Forward BIC` <- step(lowerModel,
#                                      scope = scope,
#                                      direction = "forward",
#                                      k = kBIC,
#                                      data = data)

selectedModels$`Reverse BIC` <- step(upperModel,
                                     scope = scope,
                                     direction = "backward",
                                     k = kBIC,
                                     data = data)

selectedModels$`Both BIC` <- step(upperModel,
                                   scope = scope,
                                   direction = "both",
                                   k = kBIC,
                                   data = data)
resumeConsole()

bestAIC <- NULL
bestBIC <- NULL

for (modelName in names(selectedModels)) {
  model <- selectedModels[[modelName]]

  formula <- toString(model$call)
  # formula looks like "lm, [formula], vars" because the equivalent call for the model would be lm([formula], vars)
  # thus we need to substring the result to drop the "lm, " and the ", vars" parts of the string
  # note also that 
  formula <- substr(formula, 5, nchar(formula) - 6)
  # formula now looks like "[Response] ~ Covariate1 + Covariate2 + ... + CovariateN"

  AIC <- extractAIC(model)[2]
  BIC <- extractAIC(model, k = kBIC)[2]

  if (is.null(bestAIC) || AIC < bestAIC) {
    bestAICModelName <- modelName
    bestAIC <- AIC
    bestAICModelBIC <- BIC
  }

  if (is.null(bestBIC) || BIC < bestBIC) {
    bestBICModelName <- modelName
    bestBIC <- BIC
    bestBICModelAIC <- AIC
  }

  # Keep track of covariates which were not selected by the model
  omittedCovariates <- list()
  for (covariate in covariates) {
    if(!grepl(covariate, formula)) {
      omittedCovariates[[NROW(omittedCovariates) + 1]] <- covariate
    } else {
    }
  }
  if (NROW(omittedCovariates) == 0) {
    omittedCovariates[1] <- "None"
  }

  cat(modelName, "selected:", formula,
      "\nOmitted covariates:", paste(omittedCovariates, collapse = ", "),
      "\nAIC:", AIC,
      "\nBIC:", BIC)
  cat("\n\n\n")
}

cat("The model with the best AIC was", bestAICModelName,
    "with AIC =", bestAIC,
    "(its BIC was", bestAICModelBIC, ")\n")
cat("The model with the best BIC was", bestBICModelName,
    "with BIC =", bestBIC,
    "(its AIC was", bestBICModelAIC, ")\n")
cat("\n")

# Select the best BIC model because BIC will generate a simpler model
cat("Selected model is:", bestBICModelName)
model <- selectedModels[[bestBICModelName]]
cat("\n\n")

print(summary(model)$coefficients)
cat("\n\n")

vars$PredictedOrActual <- "Actual"

vars$UpperPrediction <- rep(0, NROW(vars))
vars$LowerPrediction <- rep(0, NROW(vars))
vars$UpperConfidence <- rep(0, NROW(vars))
vars$LowerConfidence <- rep(0, NROW(vars))

predictions <- predict(model, vars, interval = "prediction")
confPredictions <- predict(model, vars, interval = "confidence")
fit <- predictions[,c("fit")]
predictions <- data.frame(UpperPrediction = predictions[, c("upr")],
                          LowerPrediction = predictions[, c("lwr")],
                          UpperConfidence = confPredictions[, c("upr")],
                          LowerConfidence = confPredictions[, c("lwr")],
                          PredictedOrActual = rep("Predicted", NROW(predictions)))
predictions[[response]] <- fit

predictions <- predictions[order(predictions[[response]]),]
predictions$Rank <- 1:NROW(predictions)

vars2 <- vars[order(predictions[[response]]),c(response,
                                               "PredictedOrActual",
                                               "UpperPrediction",
                                               "LowerPrediction",
                                               "UpperConfidence",
                                               "LowerConfidence")]
vars2$Rank <- 1:NROW(vars2)

vars2 <- rbind(predictions, vars2)

plots[[paste(response,"VsPredictedDensitty", sep = "")]] <- ggplot(vars2, aes_string(response, fill = "PredictedOrActual")) +
  geom_density(alpha = 0.3)

nMispredicted <- 0
for (i in 1:NROW(vars2)) {
  val <- vars2[i,]
  if (val$PredictedOrActual == "Actual") {
    equivalentPrediction <- predictions[val$Rank,]
    if (val[[response]] < equivalentPrediction$UpperPrediction && val[[response]] > equivalentPrediction$LowerPrediction) {
      vars2[i, ][[response]] <- 0
    } else {
      nMispredicted <- nMispredicted + 1
    }
  }
}

plots[[paste(response, "VsPredictedMasked", sep = "")]] <- ggplot(vars2, aes_string(y = response, x = "Rank")) +
  geom_point(size = 0.1) +
  geom_errorbar(aes(ymax = UpperPrediction, ymin = LowerPrediction),
                colour = "green",
                alpha = 0.3) +
  geom_errorbar(aes(ymax = UpperConfidence, ymin = LowerConfidence),
                colour = "blue",
                alpha = 0.3) +
  annotate("text", x = 200, y = 1.1 * max(vars[[response]]), label = paste("Mispredicted:", nMispredicted))

plots$Residuals <- ggplot(NULL, aes(1:NROW(model$residuals), model$residuals)) +
  geom_point() +
  xlab("") +
  ylab("Residuals")

meanResiduals <- mean(model$residuals)
sdResiduals <- sd(model$residuals)
plots$ResidualsDensity <- ggplot(NULL, aes(model$residuals)) +
  geom_density(alpha = 0.3) +
  stat_function(colour="red",
                fun = dnorm,
                args = list(mean = meanResiduals,
                            sd = sdResiduals)) +
  xlab("Residuals") +
  annotate("text", x = 18, y = 0.05, label = paste("Mean:", meanResiduals)) +
  annotate("text", x = 18, y = 0.055, label = paste("SD:", sdResiduals)) +
  NO_Y_AXIS_THEME +
  xlim(-25, 25)

for (plotName in names(plots)) {
  ggsave(paste(plotName, ".png", sep = ""), plot = plots[[plotName]])
}
