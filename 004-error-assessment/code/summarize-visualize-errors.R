
summarize.visualize.errors <- function(
    DF.errors = NULL
    ) {

    thisFunctionName <- "summarize.visualize.errors";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # summarize.visualize.errors_five.stats(
    #     DF.errors = DF.errors
    #     );
    #
    # summarize.visualize.errors_scatter.errors(
    #     DF.errors = DF.errors
    #     );

    summarize.visualize.errors_density(
        DF.errors = DF.errors
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
summarize.visualize.errors_density <- function(
    DF.errors     = NULL,
    textsize.axis = 20
    ) {

    temp.directory <- "plots-density";
    if ( !dir.exists(temp.directory) ) {
        dir.create(temp.directory);
        }

    years <- unique(DF.errors[,'year']);
    for ( temp.year in years ) {

        DF.temp <- DF.errors[DF.errors[,'year'] == temp.year,];

        temp.title <- paste(temp.year,"Greenness","error",sep = " ");
        plot.greenness.err <- initializePlot(
            title    = NULL,
            subtitle = temp.title
            );
        plot.greenness.err <- plot.greenness.err + geom_density(
            data    = DF.temp,
            mapping = aes(x = greenness.err.codr.230m),
            alpha   = 0.5,
            size    = 0.5
            );

        temp.title <- paste(temp.year,"Greenness","relative error",sep = " ");
        plot.greenness.rel.err <- initializePlot(
            title    = NULL,
            subtitle = temp.title
            );
        plot.greenness.rel.err <- plot.greenness.rel.err + geom_density(
            data    = DF.temp,
            mapping = aes(x = greenness.rel.err.codr.230m),
            alpha   = 0.5,
            size    = 0.5
            );

        temp.title <- paste(temp.year,"NDVI","error",sep = " ");
        plot.NDVI.err <- initializePlot(
            title    = NULL,
            subtitle = temp.title
            );
        plot.NDVI.err <- plot.NDVI.err + geom_density(
            data    = DF.temp,
            mapping = aes(x = NDVI.err.codr.230m),
            alpha   = 0.5,
            size    = 0.5
            );

        temp.title <- paste(temp.year,"NDVI","relative error",sep = " ");
        plot.NDVI.rel.err <- initializePlot(
            title    = NULL,
            subtitle = temp.title
            );
        plot.NDVI.rel.err <- plot.NDVI.rel.err + geom_density(
            data    = DF.temp,
            mapping = aes(x = NDVI.rel.err.codr.230m),
            alpha   = 0.5,
            size    = 0.5
            );

        my.cowplot <- cowplot::plot_grid(
            plot.greenness.err,
            plot.greenness.rel.err,
            plot.NDVI.err,
            plot.NDVI.rel.err,
            byrow       = TRUE,
            nrow        = 2,
            ncol        = 2,
            # align     = "v",
            rel_heights = c(1,1,1,1),
            rel_widths  = c(1,1,1,1)
            );

        PNG.output  <- paste0("plot-density-codr-230m-",temp.year,".png");
        cowplot::ggsave2(
            file   = file.path(temp.directory,PNG.output),
            plot   = my.cowplot,
            dpi    = 300,
            height =  12,
            width  =  24,
            units  = 'in'
            );

        } # for ( temp.year in years )

    return( NULL );

    }

summarize.visualize.errors_scatter.errors <- function(
    DF.errors     = NULL,
    textsize.axis = 20
    ) {

    temp.directory <- "plots-scatter-errors";
    if ( !dir.exists(temp.directory) ) {
        dir.create(temp.directory);
        }

    years <- unique(DF.errors[,'year']);
    for ( temp.year in years ) {

        DF.temp <- DF.errors[DF.errors[,'year'] == temp.year,];

        temp.title <- paste(temp.year,"Greenness",sep = " ");
        plot.greenness <- initializePlot(
            title    = NULL,
            subtitle = temp.title
            );

        plot.greenness <- plot.greenness + geom_abline(
            slope     = 1,
            intercept = 0,
            colour    = "gray"
            );

        plot.greenness <- plot.greenness + geom_point(
            data    = DF.temp[DF.temp[,'is.aggregate'],],
            mapping = aes(x = greenness.value.codr, y = greenness.value.230m),
            colour  = "red",
            size    = 3.00,
            alpha   = 0.25
            );

        plot.greenness <- plot.greenness + geom_point(
            data    = DF.temp,
            mapping = aes(x = greenness.value.codr, y = greenness.value.230m),
            colour  = "black",
            size    = 0.50,
            alpha   = 0.50
            );

        plot.greenness <- plot.greenness + xlab("CODR");
        plot.greenness <- plot.greenness + ylab("Albers, 230m");

        temp.title <- paste(temp.year,"NDVI",sep = " ");
        plot.NDVI <- initializePlot(
            title    = NULL,
            subtitle = temp.title
            );

        plot.NDVI <- plot.NDVI + geom_abline(
            slope     = 1,
            intercept = 0,
            colour    = "gray"
            );

        plot.NDVI <- plot.NDVI + geom_point(
            data    = DF.temp[DF.temp[,'is.aggregate'],],
            mapping = aes(x = NDVI.value.codr, y = NDVI.value.230m),
            colour  = "red",
            size    = 3.00,
            alpha   = 0.25
            );

        plot.NDVI <- plot.NDVI + geom_point(
            data    = DF.temp,
            mapping = aes(x = NDVI.value.codr, y = NDVI.value.230m),
            colour  = "black",
            size    = 0.50,
            alpha   = 0.50
            );

        plot.NDVI <- plot.NDVI + xlab("CODR");
        plot.NDVI <- plot.NDVI + ylab("Albers, 230m");

        my.cowplot <- cowplot::plot_grid(
            plot.greenness,
            plot.NDVI,
            nrow        = 1,
            # align     = "v",
            rel_heights = c(1,1)
            );

        PNG.output  <- paste0("plot-scatter-codr-230m-",temp.year,".png");
        cowplot::ggsave2(
            file   = file.path(temp.directory,PNG.output),
            plot   = my.cowplot,
            dpi    = 300,
            height =  12,
            width  =  24,
            units  = 'in'
            );

        } # for ( temp.year in years )

    return( NULL );

    }

summarize.visualize.errors_five.stats <- function(
    DF.errors     = NULL,
    textsize.axis = 20
    ) {

    temp.directory <- "plots-year-error";
    if ( !dir.exists(temp.directory) ) {
        dir.create(temp.directory);
        }

    err.colnames <- grep(
        x      = colnames(DF.errors),
        patter = "\\.err\\.",
        value  = TRUE
        );

    # DF.output <- DF.errors[,c('year','DGUID','is.aggregate',err.colnames)];
    for ( temp.err in err.colnames ) {

        DF.five.stats <- as.data.frame(aggregate(
            data = DF.errors[,c('year',temp.err)],
            x    = as.formula(paste0(temp.err," ~ year")),
            FUN  = function(x) {quantile(x = x, prob = c(0,0.025,0.05,0.50,0.95,0.975,1))}
            ));
        colnames(DF.five.stats) <- gsub(
            x           = colnames(DF.five.stats),
            pattern     = temp.err, #paste0(temp.err,"\\."),
            replacement = ""
            );
        DF.five.stats <- cbind(DF.five.stats[1],DF.five.stats[,2]);
        colnames(DF.five.stats) <- gsub(
            x           = colnames(DF.five.stats),
            pattern     = "%$",
            replacement = ""
            );
        colnames(DF.five.stats) <- paste0("percentile.",colnames(DF.five.stats));
        colnames(DF.five.stats) <- gsub(
            x           = colnames(DF.five.stats),
            pattern     = "percentile\\.year",
            replacement = "year"
            );

        cat("\nerror variable:",temp.err);
        cat("\nstr(DF.five.stats)\n");
        print( str(DF.five.stats)   );
        cat("\nDF.five.stats\n");
        print( DF.five.stats   );
        cat("\nDF.five.stats\n");

        temp.title <- gsub(x = temp.err, pattern = "\\.", replacement = " ");
        my.ggplot <- initializePlot(
            title    = NULL,
            subtitle = temp.title
            );

        my.ggplot <- my.ggplot + geom_ribbon(
            data    = DF.five.stats,
            mapping = aes(x = year, ymin = percentile.0, ymax = percentile.100),
            alpha   = 0.30,
            fill    = "gray",
            colour  = NA
            );

        my.ggplot <- my.ggplot + geom_ribbon(
            data    = DF.five.stats,
            mapping = aes(x = year, ymin = percentile.2.5, ymax = percentile.97.5),
            alpha   = 0.80,
            fill    = "yellow",
            colour  = NA
            );

        my.ggplot <- my.ggplot + geom_ribbon(
            data    = DF.five.stats,
            mapping = aes(x = year, ymin = percentile.5, ymax = percentile.95),
            alpha   = 0.30,
            fill    = "red",
            colour  = NA
            );

        my.ggplot <- my.ggplot + geom_line(
            data    = DF.five.stats,
            mapping = aes(x = year, y = percentile.50),
            colour  = "black"
            );

        my.ggplot <- my.ggplot + scale_x_continuous(breaks = seq(2000,2022,2));
        # my.ggplot <- my.ggplot + scale_y_continuous(limits = seq(-0.12,0.12,0.04));

        my.ggplot <- my.ggplot + theme(
            axis.text.x = element_text(size = textsize.axis, face = "bold", angle = 90, vjust = 0.5)
            );

        temp.stem  <- gsub(x = temp.err, pattern = "\\.", replacement = "-");
        PNG.output <- paste0("plot-year-err-",temp.stem,".png");
        ggsave(
            file   = file.path(temp.directory,PNG.output),
            plot   = my.ggplot,
            dpi    = 300,
            height =   5,
            width  =  24,
            units  = 'in'
            );

        } # for ( temp.err in err.colnames )

    return( NULL );

    }
