
attach.Sens.slopes <- function(
    list.input = NULL
    ) {

    thisFunctionName <- "attach.Sens.slopes";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.output <- list();
    for ( temp.variable in names(list.input) ) {
        DF.input <- list.input[[temp.variable]];
        rownames(DF.input) <- DF.input[,'pcpuid'];
        DF.output <- attach.Sens.slopes_get.DF.output(
            DF.input   = DF.input,
            CSV.output = paste0("DF-",temp.variable,"-Sens-slopes.csv")
            );
        LIST.output[[ temp.variable ]] <- DF.output;
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( LIST.output );

    }

##################################################
attach.Sens.slopes_get.DF.output <- function(
    DF.input   = NULL,
    CSV.output = NULL
    ) {

    if ( file.exists(CSV.output) ) {

        DF.output <- read.csv(file = CSV.output);
        colnames(DF.output) <- gsub(x = colnames(DF.output), pattern = "^X", replacement = "");

    } else {

        colnames.pcp <- c('pcpuid','pcname','pruid','pcclass');
        DF.output <- as.data.frame(t(apply(
            X      = DF.input[,setdiff(colnames(DF.input),colnames.pcp)],
            MARGIN = 1,
            FUN    = single.time.series.analysis
            )));
        colnames.Sens.slopes <- colnames(DF.output);
        DF.output[,'pcpuid'] <- as.numeric(rownames(DF.output));

        DF.output <- dplyr::left_join(
            x  = DF.output,
            y  = DF.input,
            by = "pcpuid"
            );

        colnames.reordered <- c(colnames.pcp,colnames.Sens.slopes);
        colnames.reordered <- c(colnames.reordered,setdiff(colnames(DF.output),colnames.reordered));
        DF.output <- DF.output[,colnames.reordered];

        write.csv(file = CSV.output, x = DF.output, row.names = FALSE);

        }

    return( DF.output );

    }
