
getData.upload <- function(
    data.directory = NULL,
    data.snapshot  = NULL,
    CSV.upload     = NULL
    ) {

    thisFunctionName <- "getData.upload";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.upload <- read.csv(
        file = file.path(data.directory,data.snapshot,CSV.upload),
        );
    DF.upload <- dplyr::left_join(
        x  = DF.upload,
        y  = DF.DGUID.dim1[,c('DGUID','dim1')],
        by = 'dim1'
        );
    DF.upload[,'ReferencePeriod'] <- as.character(DF.upload[,'ReferencePeriod']);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.upload );

    }
