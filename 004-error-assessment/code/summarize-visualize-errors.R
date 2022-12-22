
summarize.visualize.errors <- function(
    DF.errors = NULL
    ) {

    thisFunctionName <- "summarize.visualize.errors";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    summarize.visualize.errors_five.stats(
        DF.errors = DF.errors
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
summarize.visualize.errors_five.stats <- function(
    DF.errors     = NULL,
    textsize.axis = 20
    ) {

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
            FUN  = function(x) {quantile(x = x, prob = c(0,0.05,0.5,0.95,1))}
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
            alpha   = 0.50,
            fill    = "gray",
            colour  = NA
            );

        my.ggplot <- my.ggplot + geom_ribbon(
            data    = DF.five.stats,
            mapping = aes(x = year, ymin = percentile.5, ymax = percentile.95),
            alpha   = 0.75,
            fill    = "gray",
            colour  = NA
            );

        my.ggplot <- my.ggplot + geom_line(
            data    = DF.five.stats,
            mapping = aes(x = year, y = percentile.50),
            colour  = "black"
            );

        my.ggplot <- my.ggplot + scale_x_continuous(breaks = seq(2000,2022,4));
        my.ggplot <- my.ggplot + scale_y_continuous(limits = seq(-0.12,0.12,0.04));

        my.ggplot <- my.ggplot + theme(
            axis.text.x = element_text(size = textsize.axis, face = "bold", angle = 90, vjust = 0.5)
            );

        temp.stem  <- gsub(x = temp.err, pattern = "\\.", replacement = "-");
        PNG.output <- paste0("plot-year-err-",temp.stem,".png");
        ggsave(
            file   = PNG.output,
            plot   = my.ggplot,
            dpi    = 300,
            height =   5,
            width  =  24,
            units  = 'in'
            );

        } # for ( temp.err in err.colnames )

    return( NULL );

    }