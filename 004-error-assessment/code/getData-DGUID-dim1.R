
getData.DGUID.dim1 <- function(
    data.directory = NULL,
    data.snapshot  = NULL
    ) {

    thisFunctionName <- "getData.DGUID.dim1";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.DGUID.dim1 <- base::as.data.frame(readxl::read_excel(
        path  = file.path(data.directory,data.snapshot,"Dim1_DGUID_Relationship.xlsx"),
        sheet = "Sheet1"
        ));
    colnames(DF.DGUID.dim1) <- gsub(
        x           = colnames(DF.DGUID.dim1),
        pattern     = "Dim1",
        replacement = "dim1"
        );

    DF.DGUID.dim1[,'NAME_FRA'] <- gsub(
        x           = DF.DGUID.dim1[,'NAME_FRA'],
        pattern     = "les grand centres de population urbain",
        replacement = "les grands centres de population urbains"
        );

    DF.DGUID.dim1[,'NAME_FRA'] <- gsub(
        x           = DF.DGUID.dim1[,'NAME_FRA'],
        pattern     = "les grand centres de population",
        replacement = "les grands centres de population"
        );

    DF.DGUID.dim1[,'NAME_FRA'] <- gsub(
        x           = DF.DGUID.dim1[,'NAME_FRA'],
        pattern     = "les moyen centres de population",
        replacement = "les moyens centres de population"
        );

    DF.DGUID.dim1[,'NAME_FRA'] <- gsub(
        x           = DF.DGUID.dim1[,'NAME_FRA'],
        pattern     = "les petit centres de population",
        replacement = "les petits centres de population"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.DGUID.dim1 );

    }
