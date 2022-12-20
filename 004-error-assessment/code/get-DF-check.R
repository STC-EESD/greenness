
get.DF.check <- function(
    DF.DGUID.dim1 = NULL,
    DF.codr       = NULL,
    DF.upload     = NULL,
    colname.name  = NULL
    ) {

    thisFunctionName <- "get.DF.check";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.temp <- DF.DGUID.dim1[,c('DGUID',colname.name)];
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = colname.name,
        replacement = "GEO"
        );
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = "DGUID",
        replacement = "DGUID.replacement"
        );

    DF.codr <- dplyr::left_join(
        x  = DF.codr,
        y  = DF.temp[,c('GEO','DGUID.replacement')],
        by = 'GEO'
        );

    is.selected <- is.na(DF.codr[,'DGUID']) | (DF.codr[,'DGUID'] == "");
    # is.selected <- is.na(DF.codr[,'GEO']) | (DF.codr[,'GEO'] == "");
    print( unique(DF.codr[is.selected,c('GEO','DGUID','DGUID.replacement')]) );

    is.missing.DGUID <- (DF.codr[,'DGUID'] == "");
    DF.codr[is.missing.DGUID,'DGUID'] <- DF.codr[is.missing.DGUID,'DGUID.replacement'];
    DF.codr <- unique(DF.codr[,c('REF_DATE','DGUID','Urban.greenness','VALUE','DECIMALS')]);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    print( str(DF.upload) );
    print( str(DF.codr  ) );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DGUIDs.NA.uplaod <- unique(DF.upload[is.na(DF.upload[,"Value"]),"DGUID"]);
    DGUIDs.NA.codr   <- unique(DF.codr[  is.na(DF.codr[,  "VALUE"]),"DGUID"]);

    length(DGUIDs.NA.uplaod);
    length(DGUIDs.NA.codr  );

    setdiff(DGUIDs.NA.codr,DGUIDs.NA.uplaod);
    setdiff(DGUIDs.NA.uplaod,DGUIDs.NA.codr);

    setdiff(DGUIDs.NA.codr,No.DGUIDs);
    setdiff(No.DGUIDs,DGUIDs.NA.codr);

    setdiff(DGUIDs.NA.uplaod,No.DGUIDs);
    setdiff(No.DGUIDs,DGUIDs.NA.uplaod);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.check <- DF.codr[,c('REF_DATE','DGUID','Urban.greenness','VALUE','DECIMALS')];

    DF.check[,'ReferencePeriod'] <- paste0(DF.check[,'REF_DATE'],"0101");

    DF.check[,'dim2'] <- 2L;
    DF.check[DF.check[,'Urban.greenness'] == "Average greenness",'dim2'] <- 1L;

    DF.check <- DF.check[,c('ReferencePeriod','DGUID','dim2','VALUE','DECIMALS')];
    colnames(DF.check) <- gsub(
        x           = colnames(DF.check),
        pattern     = "VALUE",
        replacement = "value.codr"
        );

    # DF.check[,'value.codr.check'] <- as.integer(DF.check[,'value.codr'] * (10^DF.check[,'DECIMALS']));
    DF.check[,'value.codr.check'] <- DF.check[,'value.codr'] * (10^DF.check[,'DECIMALS']);

    DF.check <- dplyr::left_join(
        x  = DF.check,
        y  = DF.upload[,c('ReferencePeriod','DGUID','dim2','Value')],
        by = c('ReferencePeriod','DGUID','dim2')
        );
    colnames(DF.check) <- gsub(
        x           = colnames(DF.check),
        pattern     = "Value",
        replacement = "value.upload"
        );

    print( str(DF.check) );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.check[,'diff'] <- abs(DF.check[,'value.codr.check'] - DF.check[,'value.upload']);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.check );

    }
