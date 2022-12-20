
getData.codr <- function(
    data.directory = NULL,
    data.snapshot  = NULL,
    CSV.codr       = NULL,
    sep.codr       = NULL
    ) {

    thisFunctionName <- "getData.codr";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.codr <- read.csv(
        file = file.path(data.directory,data.snapshot,CSV.codr),
        sep  = sep.codr
        );
    print(str(DF.codr));

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "PÉRIODE.DE.RÉFÉRENCE",
        replacement = "REF_DATE"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "GÉO",
        replacement = "GEO"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "Verdure.urbaine",
        replacement = "Urban.greenness"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "VALEUR",
        replacement = "VALUE"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "DÉCIMALES",
        replacement = "DECIMALS"
        );

    DF.codr[,'Urban.greenness'] <- gsub(
        x           = DF.codr[,'Urban.greenness'],
        pattern     = "Verdure moyenne",
        replacement = "Average greenness"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.codr );

    }
