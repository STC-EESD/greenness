
visualize.Sens.slopes <- function(
    list.input      = NULL,
    pcpuids.to.plot = NULL
    ) {

    thisFunctionName <- "visualize.Sens.slopes";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.variable in names(list.input) ) {
        visualize.Sens.slopes_variable(
            variable        = temp.variable,
            DF.input        = list.input[[temp.variable]],
            pcpuids.to.plot = pcpuids.to.plot
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
visualize.Sens.slopes_variable <- function(
    variable        = NULL,
    DF.input        = NULL,
    pcpuids.to.plot = NULL
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

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames.years <- grep(
        x       = colnames(DF.input),
        pattern = "^20",
        value   = TRUE
        );

    if ( variable == "greenness" ) {
        y.limits <- c(0,100);
    } else {
        y.limits <- c(0,1);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( temp.pcpuid in pcpuids.to.plot ) {
        visualize.Sens.slopes_time.plot(
            variable       = variable,
            DF.input       = DF.input,
            pcpuid         = temp.pcpuid,
            colnames.years = colnames.years,
            y.limits       = y.limits,
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( NULL );

    }

visualize.Sens.slopes_time.plot <- function(
    variable       = NULL,
    DF.input       = NULL,
    pcpuid         = NULL,
    colnames.years = NULL,
    y.limits       = NULL,
    dots.per.inch  = 300
    ) {

    output.directory <- variable;
    if (!dir.exists(output.directory)) {
        dir.create(path = output.directory, recursive = TRUE);
        }

    DF.temp <- data.frame(
        year  = as.numeric(colnames.years),
        value = as.numeric(DF.input[DF.input[,'pcpuid'] == pcpuid,colnames.years])
        );

    temp.title <- paste0(
        pcpuid,', ',
        DF.input[DF.input[,'pcpuid'] == pcpuid,'pcname'],', ',
        "pruid = ",DF.input[DF.input[,'pcpuid'] == pcpuid,'pruid']
        );

    temp.slope     <- format(DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.slope'    ], digits = 3);
    temp.slope.lb  <- format(DF.input[DF.input[,'pcpuid'] == pcpuid,'trend.slope.lb'  ], digits = 3);
    temp.slope.ub  <- format(DF.input[DF.input[,'pcpuid'] == pcpuid,'trend.slope.ub'  ], digits = 3);
    temp.slope.pv  <- format(DF.input[DF.input[,'pcpuid'] == pcpuid,'trend.slope.pv'  ], digits = 3);
    temp.R2        <- format(DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.R.squared'], digits = 3);
    temp.sum.sqs   <- format(DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.sum.sqs'  ], digits = 3);
    temp.sum.sqs.0 <- format(DF.input[DF.input[,'pcpuid'] == pcpuid,'sum.sqs.0'       ], digits = 3);

    temp.subtitle <- paste0(
        "Sen's slope = ",  temp.slope,", ",
        "CI(slope) = (",   temp.slope.lb,", ",temp.slope.ub,"), ",
        "pvalue(slope) = ",temp.slope.pv,", ",
        "R2 = ",           temp.R2,", ",
        "sum.sqs = ",      temp.sum.sqs,", ",
        "sum.sqs.0 = ",    temp.sum.sqs.0
        );

    my.ggplot <- initializePlot(
        title    = temp.title,
        subtitle = temp.subtitle,
        y.label  = variable
        );

    my.ggplot <- my.ggplot + ggplot2::theme(
        title         = ggplot2::element_text(size = 20, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 15, face = "bold")
        );

    my.ggplot <- my.ggplot + ggplot2::geom_line(
        data    = DF.temp,
        mapping = ggplot2::aes(x = year, y = value)
        );

    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.temp,
        mapping = ggplot2::aes(x = year, y = value)
        );

    x.min <- min(DF.temp[,'year']);
    x.max <- max(DF.temp[,'year']);
    my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(
        limits =   c(x.min,x.max),
        breaks = seq(x.min,x.max,2)
        );

    # my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(
    #     limits = y.limits
    #     # breaks = x.breaks
    #     );

    temp.slope.lb     <- DF.input[DF.input[,'pcpuid'] == pcpuid,'trend.slope.lb'];
    temp.slope.ub     <- DF.input[DF.input[,'pcpuid'] == pcpuid,'trend.slope.ub'];
    temp.intercept.lb <- DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.intercept'] - temp.slope.lb * mean(DF.temp[,'year']);
    temp.intercept.ub <- DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.intercept'] - temp.slope.ub * mean(DF.temp[,'year']);

    DF.temp[,'ymin'] <- temp.intercept.lb + temp.slope.lb * DF.temp[,'year'];
    DF.temp[,'ymax'] <- temp.intercept.ub + temp.slope.ub * DF.temp[,'year'];

    my.ggplot <- my.ggplot + geom_ribbon(
        data    = DF.temp,
        mapping = aes(x = year, ymin = ymin, ymax = ymax),
        fill    = "red",
        alpha   = 0.1
        );

    my.ggplot <- my.ggplot + geom_hline(
        yintercept = mean(DF.temp[,'value']),
        colour     = "blue"
        );

    temp.slope     <- DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.slope'];
    temp.intercept <- DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.intercept'] - temp.slope * mean(DF.temp[,'year']);
    my.ggplot <- my.ggplot + geom_abline(
        slope     = temp.slope,
        intercept = temp.intercept,
        colour    = "red"
        );

    # temp.slope     <- DF.input[DF.input[,'pcpuid'] == pcpuid,'trend.slope.lb'];
    # temp.intercept <- DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.intercept'] - temp.slope * mean(DF.temp[,'year']);
    #
    # my.ggplot <- my.ggplot + geom_abline(
    #     slope     = temp.slope,
    #     intercept = temp.intercept,
    #     colour    = "red",
    #     linetype  = 2
    #     );
    #
    # temp.slope     <- DF.input[DF.input[,'pcpuid'] == pcpuid,'trend.slope.ub'];
    # temp.intercept <- DF.input[DF.input[,'pcpuid'] == pcpuid,'litteR.intercept'] - temp.slope * mean(DF.temp[,'year']);
    #
    # my.ggplot <- my.ggplot + geom_abline(
    #     slope     = temp.slope,
    #     intercept = temp.intercept,
    #     colour    = "red",
    #     linetype  = 2
    #     );

    PNG.output <- file.path(
        output.directory,
        paste0("plot-",variable,"-",pcpuid,".png")
        );

    ggplot2::ggsave(
        filename = PNG.output,
        plot     = my.ggplot,
        # scale  = 1,
        width    = 16,
        height   =  4,
        units    = "in",
        dpi      = dots.per.inch
        );

    return( NULL );

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
        title    = NULL,
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
