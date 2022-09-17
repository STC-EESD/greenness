
attach.Sens.slopes <- function(
    list.input = NULL
    ) {

    thisFunctionName <- "attach.Sens.slopes";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames.pcp <- c('pcpuid','pcname','pruid','pcclass');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.output <- list();
    for ( temp.variable in names(list.input) ) {

        DF.input <- LIST.input[[temp.variable]];
        rownames(DF.input) <- DF.input[,'pcpuid'];
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

        LIST.output[[ temp.variable ]] <- DF.output;

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( LIST.output );

    }

##################################################
