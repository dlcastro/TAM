## File Name: anova.tam.R
## File Version: 9.12


#-- Likelihood ratio test for tam objects
#-- Function is copied from the CDM package
anova.tam <- function( object, ... )
{
    cl2 <- paste(match.call())[-1]
    if (length(list(object, ...)) !=2){
        stop("anova method can only be applied for comparison of two models.\n")
    }
    objects <- list(object, ...)
    model1 <- objects[[1]]
    model2 <- objects[[2]]

    # define some necessary parameters
    model1$AIC <- model1$ic$AIC
    model1$BIC <- model1$ic$BIC
    model1$loglike <- model1$deviance / (-2)
    model1$Npars <- model1$ic$Npars
    model2$AIC <- model2$ic$AIC
    model2$BIC <- model2$ic$BIC
    model2$loglike <- model2$deviance / (-2)
    model2$Npars <- model2$ic$Npars
    # test
    dfr1 <- data.frame( "Model"=cl2[1],
        "loglike"=model1$loglike,
        "Deviance"=-2*model1$loglike )
    dfr1$Npars <- sum(model1$Npars)
    dfr1$AIC <- model1$AIC
    dfr1$BIC <- model1$BIC
    dfr2 <- data.frame( "Model"=cl2[2],
        "loglike"=model2$loglike,
        "Deviance"=-2*model2$loglike )
    dfr2$Npars <- sum(model2$Npars)
    dfr2$AIC <- model2$AIC
    dfr2$BIC <- model2$BIC
    dfr <- rbind( dfr1, dfr2 )
    dfr <- dfr[ order( dfr$Npars ), ]
    dfr$Chisq <- NA
    dfr$df <- NA
    dfr$p <- NA
    digits <- 5
    dfr[1,"Chisq"] <- dfr[1,"Deviance"] - dfr[2,"Deviance"]
    dfr[1,"df"] <- abs( dfr[1,"Npars"] - dfr[2,"Npars"] )
    dfr[1, "p"] <- round( 1 - stats::pchisq( dfr[1,"Chisq"], df=dfr[1,"df"] ), digits)
    tam_round_data_frame_print(obji=dfr, from=2, digits=digits, rownames_null=TRUE)
    invisible(dfr)
}

anova.tam.mml <- anova.tam
anova.tam.mml.3pl <- anova.tam.mml
anova.tamaan <- anova.tam.mml
anova.tam.latreg <- anova.tam
anova.tam.np <- anova.tam
