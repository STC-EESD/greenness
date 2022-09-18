
visualize.Sens.slopes <- function(
    list.input = NULL
    ) {

    thisFunctionName <- "visualize.Sens.slopes";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.variable in names(list.input) ) {
        visualize.Sens.slopes_variable(
            variable = temp.variable,
            DF.input = list.input[[temp.variable]]
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
visualize.Sens.slopes_variable <- function(
    variable = NULL,
    DF.input = NULL
    ) {

    visualize.Sens.slopes_scatter.plot(
        variable  = variable,
        DF.input  = DF.input,
        diag.line = TRUE,
        x.var     = "sum.sqs.0",
        y.var     = "litteR.sum.sqs"
        );

    visualize.Sens.slopes_scatter.plot(
        variable = variable,
        DF.input = DF.input,
        x.var    = "sum.sqs.0",
        y.var    = "litteR.R.squared"
        );

    visualize.Sens.slopes_scatter.plot(
        variable = variable,
        DF.input = DF.input,
        x.var    = "litteR.slope",
        y.var    = "litteR.R.squared",
        x.max    = ifelse( variable == "ndvi", 0.01, 3 )
        );

    visualize.Sens.slopes_scatter.plot(
        variable = variable,
        DF.input = DF.input,
        x.var    = "litteR.slope",
        x.max    = ifelse( variable == "ndvi", 0.01, 3 ),
        y.var    = "trend.slope.pv",
        y.log    = TRUE,
        y.label  = "log10(trend.slope.pv)"
        );

    }

visualize.Sens.slopes_scatter.plot <- function(
    variable      = NULL,
    DF.input      = NULL,

    diag.line     = FALSE,

    x.var         = NULL,
    x.label       = x.var,
    x.log         = FALSE,
    x.max         = NULL,
    x.min         = -x.max,
    x.breaks      = NULL,

    y.var         = NULL,
    y.label       = y.var,
    y.log         = FALSE,
    y.max         = NULL,
    y.min         = -y.max,
    y.breaks      = NULL,

    dots.per.inch = 300
    ) {

    # output.directory <- variable;
    # if (!dir.exists(output.directory)) {
    #     dir.create(path = output.directory, recursive = TRUE);
    #     }

    DF.temp <- DF.input[,c(x.var,y.var)];
    colnames(DF.temp) <- c("x.var","y.var");

    if ( x.log ) { DF.temp[,'x.var'] <- log10(DF.temp[,'x.var']); }
    if ( y.log ) { DF.temp[,'y.var'] <- log10(DF.temp[,'y.var']); }

    my.ggplot <- initializePlot(
        subtitle = variable,
        x.label  = x.label,
        y.label  = y.label
        );

    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.temp,
        mapping = ggplot2::aes(x = x.var, y = y.var)
        );
    #my.ggplot <- my.ggplot + ggplot2::labs(x = x.label, y = y.label);

    if ( diag.line ) {
        my.ggplot <- my.ggplot + geom_abline(
            slope     = 1,
            intercept = 0,
            colour    = "gray"
            );
        }

    if ( !is.null(x.max) ) {
        my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(
            limits = c(x.min,x.max)
            # breaks = x.breaks
            );
        }

    if ( !is.null(y.max) ) {
        my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(
            limits = c(y.min,y.max)
            # breaks = x.breaks
            );
        }

    # my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(
    #     limits = y.limits,
    #     breaks = y.breaks
    #     );

    # my.ggplot <- my.ggplot + ggplot2::theme(
    #     legend.position = "none",
    #     axis.title.x    = ggplot2::element_blank(),
    #     axis.title.y    = ggplot2::element_blank(),
    #     axis.text.x     = element_text(face = "bold", angle = 90, vjust = 0.5)
    #     );
    #
    # my.years <- unique(lubridate::year(DF.time.series[,'date']));
    # is.selected <- rep(c(TRUE,FALSE), times = ceiling((1+length(my.years))/2));
    # my.years <- my.years[is.selected[1:length(my.years)]];
    # my.breaks = as.Date(paste0(my.years,"-01-01"));
    #
    # my.ggplot <- my.ggplot + ggplot2::geom_hline(
    #     yintercept = 0,
    #     size       = 1.3,
    #     color      = "grey85"
    #     );
    #
    # my.ggplot <- my.ggplot + ggplot2::geom_line(
    #     data    = DF.time.series,
    #     mapping = ggplot2::aes(x = date, y = value)
    #     );
    #
    # my.ggplot <- my.ggplot + ggplot2::geom_line(
    #     data    = DF.time.series,
    #     mapping = ggplot2::aes(x = date, y = moving.average, colour = "red")
    #     );
    #
    # my.ggplot <- my.ggplot + tidyquant::geom_ma(ma_fun = SMA, n = 365, color = "red");

    PNG.output <- file.path(
        # output.directory,
        paste0(
            "plot-",variable,"-",
            gsub(x = x.var, pattern = "\\.", replacement = "-"),
            "-",
            gsub(x = y.var, pattern = "\\.", replacement = "-"),
            ".png"
            )
        );

    ggplot2::ggsave(
        filename = PNG.output,
        plot     = my.ggplot,
        # scale  = 1,
        width    = 16,
        height   = 12,
        units    = "in",
        dpi      = dots.per.inch
        );

    return( NULL );

    }
