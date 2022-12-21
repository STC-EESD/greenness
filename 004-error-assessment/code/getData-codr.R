
getData.codr <- function(
    data.directory = NULL,
    data.snapshot  = NULL,
    release        = NULL,
    CSV.codr       = NULL,
    sep.codr       = NULL,
    DF.DGUID.dim1  = NULL
    ) {

    thisFunctionName <- "getData.codr";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.codr <- read.csv(
        file = file.path(data.directory,data.snapshot,release,CSV.codr),
        sep  = sep.codr
        );
    DF.codr <- getData.codr_translate(DF.codr = DF.codr);

    # cat("\nstr(DF.codr)\n");
    # print( str(DF.codr)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.codr <- getData.codr_widen(DF.codr = DF.codr);
    DF.codr <- getData.codr_fill.missing.DGUID(
        DF.codr       = DF.codr,
        DF.DGUID.dim1 = DF.DGUID.dim1
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.codr );

    }

##################################################
getData.codr_fill.missing.DGUID <- function(
    DF.codr       = NULL,
    DF.DGUID.dim1 = NULL
    ) {

    DF.temp <- DF.DGUID.dim1[,c('DGNAME','DGUID')];
    colnames(DF.temp) <- gsub(
        x           = colnames(DF.temp),
        pattern     = "DGUID",
        replacement = "DGUID.replacement"
        );

    # 'Saint-Alexandre, Quebec, small population centre' appears twice in DF.DGUID.dim1,
    # with two different DGUID: '2021S05101277','2021S05101438'
    # Keep these two rows will lead to duplicated rows in DF.output,
    # due to the left join by DGNAME.
    is.duplicated.DGNAME <- DF.temp[,'DGUID.replacement'] %in% c('2021S05101277','2021S05101438');
    DF.temp <- DF.temp[!is.duplicated.DGNAME,];

    DF.output <- merge(
        x  = DF.codr,
        y  = DF.temp,
        by = 'DGNAME'
        );

    is.empty.DGUID <- (DF.output[,'DGUID'] == "");

    # cat("\nDF.output[is.empty.DGUID,]\n");
    # print( DF.output[is.empty.DGUID,]   );

    DF.output[is.empty.DGUID,'DGUID'] <- DF.output[is.empty.DGUID,'DGUID.replacement'];
    DF.output <- DF.output[,setdiff(colnames(DF.output),"DGUID.replacement")];

    return( DF.output );

    }

getData.codr_widen <- function(
    DF.codr = NULL
    ) {

    require(tidyr);

    retained.colnames <- c('REF_DATE','DGUID','GEO','Urban.greenness','VALUE','DECIMALS');
    DF.codr <- DF.codr[,retained.colnames];

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "REF_DATE",
        replacement = "year"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "GEO",
        replacement = "DGNAME"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "Urban.greenness",
        replacement = "variable"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "VALUE",
        replacement = "value"
        );

    colnames(DF.codr) <- gsub(
        x           = colnames(DF.codr),
        pattern     = "DECIMALS",
        replacement = "precision"
        );

    DF.output <- base::as.data.frame(tidyr::pivot_wider(
        data        = DF.codr[,c('year','DGUID','DGNAME','variable','value','precision')],
        id_cols     = c('year','DGUID','DGNAME'),
        names_from  = c('variable'),
        values_from = c("value","precision")
        ));

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "Average greenness",
        replacement = "greenness"
        );

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "Average normalized difference vegetation index \\(NDVI\\)",
        replacement = "NDVI"
        );

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "value_greenness",
        replacement = "greenness.value.codr"
        );

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "precision_greenness",
        replacement = "greenness.precision"
        );

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "value_NDVI",
        replacement = "NDVI.value.codr"
        );

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "precision_NDVI",
        replacement = "NDVI.precision"
        );

    reordered.colnames <- c(
        'year',
        'DGUID',
        'DGNAME',
        'greenness.precision',
        'NDVI.precision',
        'greenness.value.codr',
        'NDVI.value.codr'
        );

    DF.output <- DF.output[,reordered.colnames];

    return( DF.output );

    }

getData.codr_translate <- function(
    DF.codr = NULL
    ) {

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

    return( DF.codr );

    }
