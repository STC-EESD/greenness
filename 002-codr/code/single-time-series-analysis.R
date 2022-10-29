
single.time.series.analysis <- function(x) {

    thisFunctionName <- "single.time.series.analysis";

    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if( any(is.na(x)) ) {

        output.vector <- rep(x = NA, times = 1 + 4 + 4 + 7);

    } else {

        require(deming);
        require(litteR);
        require(trend);
        # require(mblm);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        sum.sqs.0 <- sum((x - mean(x))^2);

        temp.ts <- stats::ts(
            data      = x,
            start     = c(2000,1),
            frequency = 1
            );

        years <- seq(2000,2022);
        years.centred <- years - mean(years);
        DF.temp <- data.frame(
            year  = years.centred,
            value = x
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.trend.sens.slope <- trend::sens.slope(x = temp.ts);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.litteR.theil_sen <- litteR::theil_sen(x = DF.temp$year, y = DF.temp$value);

        litteR.slope     <- slope(    results.litteR.theil_sen);
        litteR.intercept <- intercept(results.litteR.theil_sen);
        litteR.fitted    <- litteR.intercept + litteR.slope * DF.temp[,'year'];
        litteR.residuals <- (litteR.fitted - x);
        litteR.sum.sqs   <- sum(litteR.residuals^2);
        litteR.R.squared <- 1 - litteR.sum.sqs / sum.sqs.0;

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.deming.theilsen <- deming::theilsen(formula = value ~ year, data = DF.temp);

        deming.slope     <- results.deming.theilsen[['coefficients']]['year'];
        deming.intercept <- results.deming.theilsen[['coefficients']]['(Intercept)'];
        deming.fitted    <- deming.intercept + deming.slope * DF.temp[,'year'];
        deming.residuals <- (deming.fitted - x);
        deming.R.squared <- 1 - sum(deming.residuals^2) / sum.sqs.0;

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        # results.mblm         <- mblm::mblm(formula = value ~ year, data = DF.temp);
        # confint.results.mblm <- confint(results.mblm);
        # summary.results.mblm <- summary(results.mblm);
        # mblm.R.squared       <- 1 - sum(results.mblm$residuals^2) / sum((x - mean(x))^2)

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        output.vector <- c(

            sum.sqs.0 = sum.sqs.0,

            litteR.slope     = litteR.slope,
            litteR.intercept = litteR.intercept,
            litteR.sum.sqs   = litteR.sum.sqs,
            litteR.R.squared = litteR.R.squared,

            trend.slope    = results.trend.sens.slope[['estimates']],
            trend.slope.pv = results.trend.sens.slope[['p.value'  ]],
            trend.slope.lb = results.trend.sens.slope[['conf.int']][1],
            trend.slope.ub = results.trend.sens.slope[['conf.int']][2],
            # trend.zstat  = results.trend.sens.slope[['statistic']],
            # conf.level   = attr(x = results.trend.sens.slope[['conf.int']], which = "conf.level"),

            deming.slope        = deming.slope,
            deming.slope.lb     = results.deming.theilsen[['ci']][2,1],
            deming.slope.ub     = results.deming.theilsen[['ci']][2,2],
            deming.intercept    = deming.intercept,
            deming.intercept.lb = results.deming.theilsen[['ci']][1,1],
            deming.intercept.ub = results.deming.theilsen[['ci']][1,2],
            deming.R.squared    = deming.R.squared

            # mblm.slope        = results.mblm[['coefficients']]['year'],
            # mblm.slope.pv     = summary.results.mblm$coefficients["year","Pr(>|V|)"],
            # mblm.slope.lb     = confint.results.mblm["year","0.025"],
            # mblm.slope.ub     = confint.results.mblm["year","0.975"],
            #
            # mblm.intercept    = results.mblm[['coefficients']]['(Intercept)'],
            # mblm.intercept.pv = summary.results.mblm$coefficients["(Intercept)","Pr(>|V|)"],
            # mblm.intercept.lb = confint.results.mblm["(Intercept)","0.025"],
            # mblm.intercept.ub = confint.results.mblm["(Intercept)","0.975"],
            #
            # mblm.R.squared    = mblm.R.squared

            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    attr(x = output.vector, which = "names") <- c(

        "sum.sqs.0",

        "litteR.slope",
        "litteR.intercept",
        "litteR.sum.sqs",
        "litteR.R.squared",

        "trend.slope",
        "trend.slope.pv",
        "trend.slope.lb",
        "trend.slope.ub",

        "deming.slope",
        "deming.slope.lb",
        "deming.slope.ub",
        "deming.intercept",
        "deming.intercept.lb",
        "deming.intercept.ub",
        "deming.R.squared"

        # "mblm.slope",
        # "mblm.slope.pv",
        # "mblm.slope.lb",
        # "mblm.slope.ub",
        # "mblm.intercept",
        # "mblm.intercept.pv",
        # "mblm.intercept.lb",
        # "mblm.intercept.ub",
        # "mblm.R.squared"

        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # cat(paste0("\n# ",thisFunctionName,"() exits."));
    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.vector );

    }

##################################################
test_single.time.series.analysis <- function(
    DF.input  = NULL,
    row.index = 1
    ) {
    thisFunctionName <- "test_single.time.series.analysis";
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));
    years <- seq(2000,2022);
    DF.temp <- data.frame(year = years, value = as.numeric(DF.input[row.index,as.character(years)]));
    DF.temp[,'years.centred'] <- DF.temp[,'year'] - mean(DF.temp[,'year']);
    DF.temp[,'avg.value'    ] <- mean(DF.temp[,'value']);
    Sens.slope <- single.time.series.analysis(x = DF.temp[,'value']);
    DF.temp[,'fitted' ]  <- Sens.slope[["litteR.intercept"]] + Sens.slope[["litteR.slope"]] * DF.temp[,'years.centred'];
    DF.temp[,'residual'] <- DF.temp[,'fitted'] - DF.temp[,'value'];
    R.squared <- 1 - sum(DF.temp[,'residual']^2) / sum((DF.temp[,'value'] - DF.temp[,'avg.value'])^2);
    cat("\nDF.temp\n");
    print( DF.temp   );
    cat("\nSens.slope[['litteR.slope']]\n");
    print( Sens.slope[['litteR.slope']]   );
    cat("\nSens.slope[['litteR.intercept']]\n");
    print( Sens.slope[['litteR.intercept']]   );
    cat("\nR.squared\n");
    print( R.squared   );
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );
    }
