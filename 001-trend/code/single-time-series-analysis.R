
single.time.series.analysis <- function(x) {

    thisFunctionName <- "single.time.series.analysis";

    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if( any(is.na(x)) ) {

        output.vector <- rep(x = NA, times = 12);

    } else {

        require(deming);
        require(litteR);
        require(mblm);
        require(trend);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.ts <- stats::ts(
            data      = x,
            start     = c(2000,1),
            frequency = 1
            );

        years <- seq(2000,2022);
        DF.temp <- data.frame(
            year  = years - mean(years),
            value = x
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.trend.sens.slope <- trend::sens.slope(x = temp.ts);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.litteR.theil_sen <- litteR::theil_sen(x = DF.temp$year, y = DF.temp$value);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.deming.theilsen <- deming::theilsen(formula = value ~ year, data = DF.temp);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.mblm         <- mblm::mblm(formula = value ~ year, data = DF.temp);
        confint.results.mblm <- confint(results.mblm);
        summary.results.mblm <- summary(results.mblm);
        mblm.R.squared       <- 1 - sum(results.mblm$residuals^2) / sum((x - mean(x))^2)

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        output.vector <- c(

            trend.slope    = results.trend.sens.slope[['estimates']],
            trend.slope.pv = results.trend.sens.slope[['p.value'  ]],
            trend.slope.lb = results.trend.sens.slope[['conf.int']][1],
            trend.slope.ub = results.trend.sens.slope[['conf.int']][2],
            # trend.zstat  = results.trend.sens.slope[['statistic']],
            # conf.level   = attr(x = results.trend.sens.slope[['conf.int']], which = "conf.level"),

            litteR.slope     = slope(    results.litteR.theil_sen),
            litteR.intercept = intercept(results.litteR.theil_sen),

            deming.slope        = results.deming.theilsen[['coefficients']]['year'],
            deming.slope.lb     = results.deming.theilsen[['ci']][2,1],
            deming.slope.ub     = results.deming.theilsen[['ci']][2,2],
            deming.intercept    = results.deming.theilsen[['coefficients']]['(Intercept)'],
            deming.intercept.lb = results.deming.theilsen[['ci']][1,1],
            deming.intercept.ub = results.deming.theilsen[['ci']][1,2]

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

        "trend.slope",
        "trend.slope.pv",
        "trend.slope.lb",
        "trend.slope.ub",

        "litteR.slope",
        "litteR.intercept",

        "deming.slope",
        "deming.slope.lb",
        "deming.slope.ub",
        "deming.intercept",
        "deming.intercept.lb",
        "deming.intercept.ub"

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
