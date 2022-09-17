
getData.greenness <- function(
    path          = NULL,
    CSV.nick      = "DF-nick.csv",
    CSV.greenness = "DF-greenness.csv",
    CSV.ndvi      = "DF-ndvi.csv"
    ) {

    thisFunctionName <- "getData.greenness";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( all(file.exists(CSV.greenness),file.exists(CSV.ndvi)) ) {
        cat("\n");
        cat(paste0("The files ",CSV.greenness," and ",CSV.ndvi," already exist; do nothing."));
        cat("\n");
        cat(paste0("\n# ",thisFunctionName,"() exits."));
        cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
        return( NULL );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.nick <- getData.greenness_nick(path = path);
    cat("\nstr(DF.nick)\n");
    print( str(DF.nick)   );

    write.csv(file = CSV.nick, x = DF.nick);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.greenness <- getData.greenness_widen(
        DF.input = DF.nick,
        variable = "greenness"
        );

    cat("\nstr(DF.greenness)\n");
    print( str(DF.greenness)   );

    DF.ndvi <- getData.greenness_widen(
        DF.input = DF.nick,
        variable = "ndvi"
        );

    cat("\nstr(DF.ndvi)\n");
    print( str(DF.ndvi)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    write.csv(file = CSV.greenness, x = DF.greenness);
    write.csv(file = CSV.ndvi,      x = DF.ndvi     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.output <- list(
        greenness = DF.greenness,
        ndvi      = DF.ndvi
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( LIST.output );

    }

##################################################
getData.greenness_widen <- function(
    DF.input = NULL,
    variable = NULL
    ) {
    DF.output <- DF.input[DF.input[,'variable'] == variable,c('pcpuid','pcname','pruid','pcclass','year','value')];
    DF.output <- tidyr::spread(
        data  = DF.output,
        key   = 'year',
        value = value
        );
    return( DF.output );
    }

getData.greenness_nick <- function(
    path = NULL
    ) {

    DF.xlsx <- readxl::read_xlsx(path = path);
    DF.xlsx <- as.data.frame(DF.xlsx);
    colnames(DF.xlsx) <- tolower(colnames(DF.xlsx));

    cat("\nstr(DF.xlsx)\n");
    print( str(DF.xlsx)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.xlsx$pcpuid  <- as.numeric(DF.xlsx$pcpuid);
    DF.xlsx$year    <- as.numeric(DF.xlsx$year   );
    DF.xlsx$pcclass <- as.numeric(DF.xlsx$pcclass);
    DF.xlsx$pruid   <- as.numeric(DF.xlsx$pruid  );
    DF.xlsx$value   <- as.numeric(DF.xlsx$value  );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames(DF.xlsx) <- gsub(
        x           = colnames(DF.xlsx),
        pattern     = "^dim$",
        replacement = "variable"
        );

    DF.xlsx[DF.xlsx[,"variable"] == "1", "variable"] <- "greenness";
    DF.xlsx[DF.xlsx[,"variable"] == "2", "variable"] <- "ndvi";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nlength(unique(DF.xlsx$objectid))\n");
    print( length(unique(DF.xlsx$objectid))   );

    cat("\nlength(unique(DF.xlsx$pcpuid))\n");
    print( length(unique(DF.xlsx$pcpuid))   );

    cat("\ntable(DF.xlsx$year)\n");
    print( table(DF.xlsx$year)   );

    cat("\ntable(DF.xlsx$variable)\n");
    print( table(DF.xlsx$variable)   );

    cat("\nlength(unique(DF.xlsx$pcname))\n");
    print( length(unique(DF.xlsx$pcname))   );

    cat("\ntable(DF.xlsx$pcclass)\n");
    print( table(DF.xlsx$pcclass)   );

    cat("\ntable(DF.xlsx$pruid)\n");
    print( table(DF.xlsx$pruid)   );

    cat("\nlength(unique(DF.xlsx$dguid))\n");
    print( length(unique(DF.xlsx$dguid))   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nstr(DF.xlsx)\n");
    print( str(DF.xlsx)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    test.dguid <- gsub(
        x           = DF.xlsx$dguid,
        pattern     = "S",
        replacement = "_"
        );

    cat("\nlength(unique(test.dguid))\n");
    print( length(unique(test.dguid))   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.unique <- unique(DF.xlsx[,c('pcpuid','dguid')]);

    cat("\nnrow(DF.unique)\n");
    print( nrow(DF.unique)   );

    cat("\nlength(unique(DF.unique$pcpuid))\n");
    print( length(unique(DF.unique$pcpuid))   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- DF.xlsx[,c('pcpuid','pcname','pruid','pcclass','year','variable','value')];
    # cat("\nstr(DF.output)\n");
    # print( str(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( DF.output );

    }