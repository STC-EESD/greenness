
getData.errors <- function(
    data.directory = NULL,
    data.snapshot  = NULL,
    release        = NULL,
    CSV.codr       = NULL,
    colname.DGNAME = NULL,
    sep.codr       = NULL,
    CSV.output     = "DF-errors.csv",

    CSV.Albers.greenness.10m = NULL,
    CSV.Albers.NDVI.10m      = NULL,
    colname.suffix.10m       = "10m",

    CSV.Albers.greenness.230m = NULL,
    CSV.Albers.NDVI.230m      = NULL,
    colname.suffix.230m       = "230m"
    ) {

    thisFunctionName <- "getData.errors";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(CSV.output) ) {
        cat("\nThe output file",CSV.output,"already exists; loading the file ...\n\n");
        DF.output <- read.csv(CSV.output);
        return( DF.output );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.DGUID.dim1 <- getData.DGUID.dim1(
        data.directory = data.directory,
        data.snapshot  = data.snapshot,
        release        = release
        );

    # cat("\nstr(DF.DGUID.dim1)\n");
    # print( str(DF.DGUID.dim1)   );

    colnames(DF.DGUID.dim1) <- gsub(
        x           = colnames(DF.DGUID.dim1),
        pattern     = colname.DGNAME, # "NAME_ENG",
        replacement = "DGNAME"
        );

    DF.DGUID.dim1 <- DF.DGUID.dim1[,c('dim1','DGUID','DGNAME')];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.codr <- getData.codr(
        data.directory = data.directory,
        data.snapshot  = data.snapshot,
        release        = release,
        CSV.codr       = CSV.codr,
        sep.codr       = sep.codr,
        DF.DGUID.dim1  = DF.DGUID.dim1
        );

    # cat("\nstr(DF.codr)\n");
    # print( str(DF.codr)   );

    # is.selected <- (DF.codr[,'DGUID'] %in% c('2021S05101277','2021S05101438'));
    # cat("\nDF.codr[is.selected,]\n");
    # print( DF.codr[is.selected,]   );
    #
    # is.selected <- grepl(x = DF.codr[,'DGNAME'], pattern = 'Saint-Alexandre');
    # cat("\nDF.codr[is.selected,]\n");
    # print( DF.codr[is.selected,]   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    years  <- sort(unique(DF.codr[,'year']));
    DGUIDs <- sort(unique(c(
        unique(DF.DGUID.dim1[,'DGUID']),
        unique(DF.codr[,      'DGUID'])
        )));

    DF.grid <- expand.grid(DGUID = DGUIDs, year = years);
    DF.grid <- DF.grid[,c('year','DGUID')];

    DF.grid[,'DGUID'] <- as.character(DF.grid[,'DGUID']);

    # cat("\nstr(DF.grid)\n");
    # print( str(DF.grid)   );

    # cat("\nDF.grid[1:50,]\n");
    # print( DF.grid[1:50,]   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- merge(
        x     = DF.grid,
        y     = DF.DGUID.dim1,
        by    = "DGUID",
        all.x = TRUE
        );

    reordered.colnames <- c(
        'year',
        'dim1',
        'DGUID',
        'DGNAME'
        );

    DF.output <- DF.output[,reordered.colnames];

    # cat("\nstr(DF.output) - A\n");
    # print( str(DF.output) );
    #
    # cat("\nDF.output[1:50,]\n");
    # print( DF.output[1:50,]   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- merge(
        x     = DF.output,
        y     = DF.codr[,setdiff(colnames(DF.codr),'DGNAME')],
        by    = c('year','DGUID'),
        all.x = TRUE
        );
    DF.output <- DF.output[order(DF.output$year,DF.output$dim1),];

    # cat("\nstr(DF.output) - B\n");
    # print( str(DF.output) );
    #
    # cat("\nDF.output[1:50,]\n");
    # print( DF.output[1:50,]   );

    # is.selected <- (DF.output[,'DGUID'] %in% c('2021S05101277','2021S05101438'));
    # cat("\nDF.output[is.selected,]\n");
    # print( DF.output[is.selected,]   );
    #
    # is.selected <- grepl(x = DF.output[,'DGNAME'], pattern = 'Saint-Alexandre');
    # cat("\nDF.output[is.selected,]\n");
    # print( DF.output[is.selected,]   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # cat("\nnrow(unique(DF.output[,c('year','DGUID')]))\n");
    # print( nrow(unique(DF.output[,c('year','DGUID')]))   );
    #
    # is.duplicated <- duplicated(x = DF.output[,c('year','DGUID')]);
    # cat("\nDF.output[is.duplicated,]\n");
    # print( DF.output[is.duplicated,]   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.Albers.230m <- getData.Albers(
        data.directory       = data.directory,
        data.snapshot        = data.snapshot,
        release              = release,
        CSV.Albers.NDVI      = CSV.Albers.NDVI.230m,
        CSV.Albers.greenness = CSV.Albers.greenness.230m,
        colname.suffix       = colname.suffix.230m
        );

    # cat("\nstr(DF.Albers.230m)\n");
    # print( str(DF.Albers.230m)   );

    # cat("\nduplicated(DF.Albers.230m[,c('year','DGUID')])\n");
    # print( duplicated(DF.Albers.230m[,c('year','DGUID')])   );

    DF.Albers.10m <- getData.Albers(
        data.directory       = data.directory,
        data.snapshot        = data.snapshot,
        release              = release,
        CSV.Albers.NDVI      = CSV.Albers.NDVI.10m,
        CSV.Albers.greenness = CSV.Albers.greenness.10m,
        colname.suffix       = colname.suffix.10m
        );

    # cat("\nstr(DF.output) - B\n");
    # print( str(DF.output)   );
    #
    # cat("\nstr(DF.Albers.10m)\n");
    # print( str(DF.Albers.10m)   );

    # cat("\nduplicated(DF.Albers.10m[,c('year','DGUID')])\n");
    # print( duplicated(DF.Albers.10m[,c('year','DGUID')])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- merge(
        x     = DF.output,
        y     = DF.Albers.230m,
        by    = c('year','DGUID'),
        all.x = TRUE
        );

    # cat("\nstr(DF.output) - C\n");
    # print( str(DF.output) );

    DF.output <- merge(
        x     = DF.output,
        y     = DF.Albers.10m,
        by    = c('year','DGUID'),
        all.x = TRUE
        );
    DF.output <- DF.output[order(DF.output$year,DF.output$dim1),];

    # cat("\nstr(DF.output) - D\n");
    # print( str(DF.output) );

    DF.output[,'is.aggregate'] <- grepl(
        x       = DF.output[,'DGUID'],
        pattern = "[0-9]{4,4}C[0-9]{6,}"
        );

    leading.colnames   <- c('year','DGUID','dim1','DGNAME','is.aggregate');
    reordered.colnames <- c(
        leading.colnames,
        setdiff(colnames(DF.output),leading.colnames)
        );
    DF.output <- DF.output[,reordered.colnames];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames.greenness <- paste0("greenness.value.",c(colname.suffix.230m,colname.suffix.10m));
    for ( temp.colname in colnames.greenness ) {
        DF.output[,temp.colname] <- apply(
            X      = DF.output[,c(temp.colname,'greenness.precision')],
            MARGIN = 1,
            FUN    = function(x) {
                round(x = 100.0 * x[1], digits = x[2]);
                }
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames.NDVI <- paste0("NDVI.value.",c(colname.suffix.230m,colname.suffix.10m));
    for ( temp.colname in colnames.NDVI ) {
        DF.output[,temp.colname] <- apply(
            X      = DF.output[,c(temp.colname,'NDVI.precision')],
            MARGIN = 1,
            FUN    = function(x) {
                round(x = (x[1]/10000.0) - 1.0, digits = x[2]);
                }
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- attach.error.columns(
        DF.input  = DF.output,
        variables = c('greenness','NDVI'),
        versions  = c('codr','230m','10m')
        );

    # cat("\nstr(DF.output)\n");
    # print( str(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    write.csv(
        file      = CSV.output,
        x         = DF.output,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # cat("\nstr(DF.output)\n");
    # print( str(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }
