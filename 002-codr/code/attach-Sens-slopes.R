
attach.Sens.slopes <- function(
    FILE.input = NULL,
    CSV.output = paste0("DF-ndvi-Sens-slopes.csv")
    ) {

    thisFunctionName <- "attach.Sens.slopes";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.input <- read.csv(file = FILE.input);
    rownames(DF.input) <- DF.input[,'DGUID'];
    DF.input <- getData.greenness.ndvi_remove.superfluous.rows.columns(
        DF.input = DF.input
        );

    cat("\nstr(DF.input)\n");
    print( str(DF.input)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- attach.Sens.slopes_get.DF.output(
        DF.input   = DF.input,
        CSV.output = CSV.output
        );

    cat("\nstr(DF.output)\n");
    print( str(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
attach.Sens.slopes_get.DF.output <- function(
    DF.input   = NULL,
    CSV.output = NULL
    ) {

    if ( file.exists(CSV.output) ) {

        DF.output <- read.csv(file = CSV.output);
        DF.output[,'DGUID'] <- as.character(DF.output[,'DGUID']);
        colnames(DF.output) <- gsub(x = colnames(DF.output), pattern = "^X", replacement = "");

    } else {

        colnames.non.values <- c('DGUID','dim1','dim2');
        DF.output <- as.data.frame(t(apply(
            X      = DF.input[,setdiff(colnames(DF.input),colnames.non.values)],
            MARGIN = 1,
            FUN    = single.time.series.analysis
            )));
        colnames.Sens.slopes <- colnames(DF.output);
        DF.output[,'DGUID']  <- rownames(DF.output);

        DF.output <- dplyr::left_join(
            x  = DF.output,
            y  = DF.input,
            by = "DGUID"
            );

        colnames.reordered <- c(colnames.non.values,colnames.Sens.slopes);
        colnames.reordered <- c(colnames.reordered,setdiff(colnames(DF.output),colnames.reordered));
        DF.output <- DF.output[,colnames.reordered];

        write.csv(file = CSV.output, x = DF.output, row.names = FALSE);

        }

    return( DF.output );

    }
