
permutation.test.Sens.slope <- function(
    DF.input       = NULL,
    DGUID.to.test  = NULL,
    n.permutations = 1000L
    ) {

    thisFunctionName <- "permutation.test.Sens.slope";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(gtools);
    require(trend);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames.years <- grep(x = colnames(DF.input), pattern = "^[0-9]+$", value = TRUE);
    min.year <- min(as.integer(colnames.years));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    input.vector <- DF.input[DF.input[,'DGUID'] == DGUID.to.test,colnames.years];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.ts <- stats::ts(
        data      = input.vector,
        start     = c(min.year,1),
        frequency = 1
        );

    results.trend.sens.slope <- trend::sens.slope(x = temp.ts);
    observed.Sens.slope      <- results.trend.sens.slope[['estimates']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    permuted.Sens.slopes <- numeric(n.permutations);
    for ( index in seq(1,n.permutations) ) {
        temp.ts <- stats::ts(
            data      = gtools::permute(input.vector),
            start     = c(min.year,1),
            frequency = 1
            );
        results.trend.sens.slope    <- trend::sens.slope(x = temp.ts);
        permuted.Sens.slopes[index] <- results.trend.sens.slope[['estimates']];
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.output <- list(
        observed.Sens.slope  = observed.Sens.slope,
        permuted.Sens.slopes = permuted.Sens.slopes
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nsum( permuted.Sens.slopes < observed.Sens.slope )\n");
    print( sum( permuted.Sens.slopes < observed.Sens.slope )   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    permutation.test.Sens.slope_histogram(
        DGUID = DGUID.to.test,
        observed.Sens.slope = observed.Sens.slope,
        DF.permuted = data.frame(
            index = seq(1,length(permuted.Sens.slopes)),
            slope = permuted.Sens.slopes
            )
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

##################################################
permutation.test.Sens.slope_histogram <- function(
    DGUID               = NULL,
    observed.Sens.slope = NULL,
    DF.permuted         = NULL,
    bin.width           = 1e-5,
    limits              = 3e-3 * c(-1,1),
    breaks              = seq(-3e-3,3e-3,1e-3),
    PNG.output          = "plot-histogram-permutation-test-Sens-slope.png"
    ) {

    my.ggplot <- initializePlot(title = NULL, subtitle = DGUID);
    my.ggplot <- my.ggplot + geom_vline(
        xintercept = observed.Sens.slope,
        colour     = "orange",
        size       = 1.00
        );
    my.ggplot <- my.ggplot + geom_histogram(
        data     = DF.permuted,
        mapping  = aes(x = slope),
        binwidth = bin.width,
        alpha    = 0.5
        # fill     = "black",
        # colour   = NULL
        );
    my.ggplot <- my.ggplot + scale_x_continuous(
        limits = limits,
        breaks = breaks
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    permuted.Sens.slopes <- as.numeric(DF.permuted[,'slope']);

    n.more.extrems <- sum( permuted.Sens.slopes < observed.Sens.slope );
    n.permutations <- nrow(DF.permuted);

    p.value <- n.more.extrems / n.permutations;
    p.value <- format(p.value, digits = 4, scientific = TRUE);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.xmax <- max(layer_scales(my.ggplot,i=1L,j=1L)[['x']]$get_limits());
    temp.ymax <- max(layer_scales(my.ggplot,i=1L,j=1L)[['y']]$get_limits());

    my.ggplot <- my.ggplot + annotate(
        geom  = "text",
        label = c(
            paste0("n.permutations = ",n.permutations),
            paste0("p-value (one-sided) = ",p.value)
            ),
        x     = temp.xmax * 0.5 * c(1,1),
        y     = temp.ymax * c(0.98,0.91),
        size  = 13,
        color = "black"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ggplot2::ggsave(
        file   = PNG.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =  12,
        width  =  20,
        units  = 'in'
        );

    }
