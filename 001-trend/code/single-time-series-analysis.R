
single.time.series.analysis <- function(x) {

    thisFunctionName <- "single.time.series.analysis";

    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if( any(is.na(x)) ) {

        output.vector <- rep(x = NA, times = 11);

    } else {

        require(trend);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        temp.ts <- stats::ts(
            data      = x,
            start     = c(2000,1),
            frequency = 1
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.SenSlope <- trend::sens.slope(x = temp.ts);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        output.vector <- c(
            Sen.slope   = results.SenSlope[['estimates']],
            z.stat      = results.SenSlope[['statistic']],
            p.value     = results.SenSlope[['p.value'  ]],
            conf.level  = attr(x = results.SenSlope[['conf.int']], which = "conf.level"),
            conf.int.lb = results.SenSlope[['conf.int']][1],
            conf.int.ub = results.SenSlope[['conf.int']][2]
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    attr(x = output.vector, which = "names") <- c(
        "Sen.slope",
        "z.stat",
        "p.value",
        "conf.level",
        "conf.int.lb",
        "conf.int.ub"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # cat(paste0("\n# ",thisFunctionName,"() exits."));
    # cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.vector );

    }
