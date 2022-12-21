
getData.Albers <- function(
    data.directory       = NULL,
    data.snapshot        = NULL,
    release              = NULL,
    CSV.Albers.greenness = NULL,
    CSV.Albers.NDVI      = NULL,
    colname.suffix       = NULL
    ) {

    thisFunctionName <- "getData.Albers";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.Albers.greenness <- getData.Albers_greenness(
        data.directory       = data.directory,
        data.snapshot        = data.snapshot,
        release              = release,
        CSV.Albers.greenness = CSV.Albers.greenness
        );

    cat("\nstr(DF.Albers.greenness)\n");
    print( str(DF.Albers.greenness)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.Albers.NDVI <- getData.Albers_NDVI(
        data.directory  = data.directory,
        data.snapshot   = data.snapshot,
        release         = release,
        CSV.Albers.NDVI = CSV.Albers.NDVI
        );

    cat("\nstr(DF.Albers.NDVI)\n");
    print( str(DF.Albers.NDVI)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    years <- sort(unique(c(
        unique(DF.Albers.greenness[,'year']),
        unique(DF.Albers.NDVI[,     'year'])
        )));

    DGUIDs <- sort(unique(c(
        unique(DF.Albers.greenness[,'DGUID']),
        unique(DF.Albers.NDVI[,     'DGUID'])
        )));

    DF.grid <- expand.grid(DGUID = DGUIDs, year = years);
    DF.grid <- DF.grid[,c('year','DGUID')];

    DF.grid[,'DGUID'] <- as.character(DF.grid[,'DGUID']);

    cat("\nstr(DF.grid)\n");
    print( str(DF.grid)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.Albers.greenness <- merge(
        x     = DF.grid,
        y     = DF.Albers.greenness,
        by    = c('year','DGUID'),
        all.x = TRUE
        );
    # write.csv(x = DF.Albers.greenness, file = 'DF-Albers-greenness.csv');

    cat("\nstr(DF.Albers.greenness)\n");
    print( str(DF.Albers.greenness)   );

    cat("\nsummary(DF.Albers.greenness)\n");
    print( summary(DF.Albers.greenness)   );

    DF.Albers.NDVI <- merge(
        x     = DF.grid,
        y     = DF.Albers.NDVI,
        by    = c('year','DGUID'),
        all.x = TRUE
        );
    # write.csv(x = DF.Albers.NDVI, file = 'DF-Albers-NDVI.csv');

    cat("\nstr(DF.Albers.NDVI)\n");
    print( str(DF.Albers.NDVI)   );

    cat("\nsummary(DF.Albers.NDVI)\n");
    print( summary(DF.Albers.NDVI)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- merge(
        x     = DF.Albers.greenness,
        y     = DF.Albers.NDVI,
        by    = c('year','DGUID'),
        all.x = TRUE
        );
    # write.csv(x = DF.output, file = 'DF-deleteme.csv', row.names = FALSE);
    # Sys.sleep(5);
    # DF.output <- read.csv('DF-deleteme.csv');

    cat("\nstr(DF.output)\n");
    print( str(DF.output)   );

    cat("\nsummary(DF.output)\n");
    print( summary(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    reordered.colnames <- c(
        'year',
        'DGUID',
        'count',
        'area',
        'n.pixels.grey',
        'n.pixels.green',
        'greenness.value',
        'NDVI.value'
        );

    DF.output <- DF.output[,reordered.colnames];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.colnames <- setdiff(colnames(DF.output),c('year','DGUID'));
    for ( temp.colname in temp.colnames ) {
        colnames(DF.output) <- gsub(
            x           = colnames(DF.output),
            pattern     = temp.colname,
            replacement = paste0(temp.colname,".",colname.suffix)
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
getData.Albers_greenness <- function(
    data.directory       = NULL,
    data.snapshot        = NULL,
    release              = NULL,
    CSV.Albers.greenness = NULL
    ) {

    DF.output <- base::as.data.frame(utils::read.csv(
        file = file.path(data.directory,data.snapshot,release,CSV.Albers.greenness)
        ));
    DF.output <- DF.output[,c('YEAR','DGUID','VALUE_1','VALUE_2')];

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "YEAR",
        replacement = "year"
        );

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "VALUE_1",
        replacement = "n.pixels.grey"
        );

    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "VALUE_2",
        replacement = "n.pixels.green"
        );

    DF.output[,'greenness.value'] <- DF.output[,'n.pixels.green'] / (DF.output[,'n.pixels.grey'] + DF.output[,'n.pixels.green']);

    return( DF.output );

    }

getData.Albers_NDVI <- function(
    data.directory  = NULL,
    data.snapshot   = NULL,
    release         = NULL,
    CSV.Albers.NDVI = NULL
    ) {

    DF.Albers.NDVI <- base::as.data.frame(utils::read.csv(
        file = file.path(data.directory,data.snapshot,release,CSV.Albers.NDVI)
        ));
    DF.Albers.NDVI <- DF.Albers.NDVI[,c('YEAR','DGUID','COUNT','AREA','MEAN')];

    colnames(DF.Albers.NDVI) <- gsub(
        x           = colnames(DF.Albers.NDVI),
        pattern     = "YEAR",
        replacement = "year"
        );

    colnames(DF.Albers.NDVI) <- gsub(
        x           = colnames(DF.Albers.NDVI),
        pattern     = "COUNT",
        replacement = "count"
        );

    colnames(DF.Albers.NDVI) <- gsub(
        x           = colnames(DF.Albers.NDVI),
        pattern     = "AREA",
        replacement = "area"
        );

    colnames(DF.Albers.NDVI) <- gsub(
        x           = colnames(DF.Albers.NDVI),
        pattern     = "MEAN",
        replacement = "NDVI.value"
        );

    return( DF.Albers.NDVI );

    }
