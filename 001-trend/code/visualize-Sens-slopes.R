
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
        variable = variable,
        DF.input = DF.input,
        x.var    = "litteR.slope",
        y.var    = "litteR.R.squared"
        );

    }

visualize.Sens.slopes_scatter.plot <- function(
    variable      = NULL,
    DF.input      = NULL,
    x.var         = NULL,
    y.var         = NULL,
    x.label       = x.var,
    y.label       = y.var,
    dots.per.inch = 300
    ) {

    output.director <- variable;
    if (!dir.exists(output.directory)) {
        dir.create(path = output.directory, recursive = TRUE);
        }

    # my.ggplot <- initializePlot(
    #     # subtitle = paste0("pointID = ",pointID,", (x,y) = (",x.coord,",",y.coord,"), TestZ = ",TestZ)
    #     );

    DF.temp <- DF.input[,c(x.var,y.var)];
    colnames(DF.temp) <- c("x.var","y.var");

    my.ggplot <- initializePlot();
    my.ggolot <- my.ggplot + ggplot2::xlab(label = x.label);
    my.ggolot <- my.ggplot + ggplot2::ylab(label = y.label);
    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.temp,
        mapping = ggplot2::aes(x = x.var, y = y.var)
        );

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
    # my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(
    #     breaks = my.breaks,
    #     labels = my.breaks
    #     );
    #
    # my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(
    #     limits = 175 * c(  -1,1),
    #     breaks =  50 * seq(-4,4)
    #     );
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
        output.directory,
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
        width    = 12,
        height   = 12,
        units    = "in",
        dpi      = dots.per.inch
        );

    return( NULL );

    }
