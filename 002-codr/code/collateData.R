
collateData <- function(
    DF.DGUID.dim1  = NULL,
    data.directory = NULL,
    data.snapshot  = NULL,
    file.stems     = c('Green','AvNDVI'),
    CSV.outputs    = paste0(file.stems,'.csv')
    ) {

    thisFunctionName <- "collateData";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(tidyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( file.index in seq(1,length(file.stems)) ) {

        CSV.output <- CSV.outputs[file.index];
        if ( file.exists(CSV.output) ) {

            cat(paste0("\nThe file ",CSV.output," already exists; do nothing ...\n"));

        } else {

            file.stem <- file.stems[file.index];
            cat("\n### processing: ",file.stem,"\n\n");

            temp.directories <- grep(
                x       = list.dirs(file.path(data.directory,data.snapshot)),
                pattern = file.stem,
                value   = TRUE
                );
            # cat("\ntemp.directories\n");
            # print( temp.directories   );

            DF.long <- data.frame();
            for ( temp.directory in temp.directories ) {
                # cat("\ntemp.directory\n");
                # print( temp.directory   );
                FILEs.dbf <- list.files(path = temp.directory, pattern = "\\.dbf$");
                # cat("\nFILEs.dbf\n");
                # print( FILEs.dbf   );
                for ( temp.dbf in FILEs.dbf ) {
                    DF.temp <- collateData_read(
                        FILE.dbf = file.path(temp.directory,temp.dbf)
                        );
                    # cat("\nstr(DF.temp)\n");
                    # print( str(DF.temp)   );
                    DF.long <- rbind(DF.long,DF.temp);
                    }
                } # for ( temp.directory in temp.directories )

            cat("\nstr(DF.long)\n");
            print( str(DF.long)   );

            DF.duplicates <- DF.long %>%
                dplyr::group_by(DGUID, dim2, Year) %>%
                dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
                dplyr::filter(n > 1L);
            DF.duplicates <- as.data.frame(DF.duplicates);

            cat("\nDF.duplicates\n");
            print( DF.duplicates   );

            # cat("\nDF.long[is.na(DF.long[,'DGUID']),]\n");
            # print( DF.long[is.na(DF.long[,'DGUID']),]   );

            value.colname <- setdiff(colnames(DF.long),c('DGUID','dim2','Year'));
            DF.wide <- DF.long %>%
                tidyr::pivot_wider(
                    id_cols     = c('DGUID','dim2'),
                    names_from  = 'Year',
                    values_from = all_of(value.colname)
                    );
            DF.wide <- as.data.frame(DF.wide);

            cat("\nstr(DF.wide)\n");
            print( str(DF.wide)   );

            DF.output <- dplyr::left_join(
                x  = DF.DGUID.dim1[,c('DGUID','dim1')],
                y  = DF.wide,
                by = "DGUID"
                );
            DF.output <- as.data.frame(DF.output);
            DF.output[,'dim2'] <- file.index;

            colnames.leading <- c('DGUID','dim1','dim2');
            colnames.ordered <- c(
                colnames.leading,
                setdiff(colnames(DF.output),colnames.leading)
                );
            DF.output <- DF.output[,colnames.ordered];

            years <- setdiff(colnames(DF.output),c('DGUID','dim1','dim2'));
            is.NA.or.zero <- apply(
                X      = DF.output[,years],
                MARGIN = 1,
                FUN    = function(x) {return(
                    all( is.na(x) | (x == 0) )
                    )}
                );
            DF.output[is.NA.or.zero,years] <- NA;

            cat("\nstr(DF.output)\n");
            print( str(DF.output)   );

            write.csv(
                file      = CSV.output,
                x         = DF.output,
                row.names = FALSE,
                quote     = FALSE
                );

            } # if ( file.exists(CSV.output) )

        } # for ( file.index in seq(1,length(file.stems)) )

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
collateData_read <- function(
    FILE.dbf = NULL
    ) {
    if ( grepl(x = base::basename(FILE.dbf), pattern = "greenness", ignore.case = TRUE) ) {
        # cat("\ngreenness: ",FILE.dbf,"\n");
        DF.output <- collateData_read.greenness(
            FILE.dbf = FILE.dbf
            );
    } else if ( grepl(x = base::basename(FILE.dbf), pattern = "ndvi", ignore.case = TRUE) ) {
        # cat("\nndvi: ",FILE.dbf,"\n");
        DF.output <- collateData_read.ndvi(
            FILE.dbf = FILE.dbf
            );
    } else {
        DF.output <- data.frame();
        }
    return( DF.output );
    }

collateData_read.greenness <- function(
    FILE.dbf = NULL
    ) {

    DF.output <- foreign::read.dbf(file = FILE.dbf);
    attr(DF.output,"data_types") <- NULL;

    colnames.retained <- setdiff(colnames(DF.output),c('FID_','UID','VALUE_0'));
    DF.output <- DF.output[,colnames.retained];

    if ( sum(colnames(DF.output) == "DGUID") < 1 ) {
        colnames(DF.output) <- gsub(
            x           = colnames(DF.output),
            pattern     = "^DGUID.+",
            replacement = "DGUID"
            );
        }

    colnames.ordered <- c('DGUID','dim2','Year','VALUE_1','VALUE_2');
    DF.output <- DF.output[,colnames.ordered];

    DF.output[,'DGUID'] <- as.character(           DF.output[,'DGUID'] );
    DF.output[,'dim2' ] <- as.integer(as.character(DF.output[,'dim2' ]));
    DF.output[,'Year' ] <- as.character(           DF.output[,'Year' ] );

    temp.colnames <- c('VALUE_1','VALUE_2');
    for ( temp.colname in temp.colnames ) {
        DF.output[,temp.colname] <- as.numeric(as.character(DF.output[,temp.colname]));
        }

    DF.output[,'PctGreenness'] <- 100 * DF.output[,'VALUE_2'] / (DF.output[,'VALUE_1'] + DF.output[,'VALUE_2']);
    DF.output[,'PctGreenness'] <- base::round(x = DF.output[,'PctGreenness'], digits = 1);

    colnames.retained <- setdiff(colnames(DF.output),c('VALUE_1','VALUE_2'));
    DF.output <- DF.output[,colnames.retained];

    return( DF.output );

    }

collateData_read.ndvi <- function(
    FILE.dbf = NULL
    ) {

    DF.output <- foreign::read.dbf(file = FILE.dbf);
    attr(DF.output,"data_types") <- NULL;

    colnames.retained <- setdiff(colnames(DF.output),c('UID','PR_PC','ZONE_CODE'));
    DF.output <- DF.output[,colnames.retained];

    # Some NDVI dbf files don't have a DGUID column.
    # These NDVI dbf files could instead have simultaneously the columns
    # 'DGUIDP' and 'DGUID_1', and 'DGUIDP' can be largely NA, while
    # 'DGUID_1' is the true unique row identifer.
    # The following code is to determine the unique row identifier and
    # rename it 'DGUID' before proceeding to downstream steps.
    if ( sum(colnames(DF.output) == "DGUID") < 1 ) {
        DGUID.candidates <- grep(
            x       = colnames(DF.output),
            pattern = "DGUID",
            value   = TRUE
            );
        cat("\nFILE.dbf:",base::basename(FILE.dbf));
        cat("\nDGUID.candidates:",paste(DGUID.candidates,collapse=", "),"\n");
        if ( length(DGUID.candidates) == 1 ) {
            DGUID.replacement <- DGUID.candidates;
            # cat("\nnrow(DF.output)\n");
            # print( nrow(DF.output)   );
            # cat("\nlength(unique(DF.output[,DGUID.candidates]))\n");
            # print( length(unique(DF.output[,DGUID.candidates]))   );
        } else {
            n.uniques <- apply(
                X      = DF.output[,DGUID.candidates],
                MARGIN = 2,
                FUN    = function(x) { length(unique(x)) }
                );
            DGUID.replacement <- names(n.uniques)[n.uniques == nrow(DF.output)];
            }
        colnames(DF.output) <- gsub(
            x           = colnames(DF.output),
            pattern     = DGUID.replacement,
            replacement = "DGUID"
            );
        }

    colnames.ordered <- c('DGUID','dim2','Year','MEAN');
    DF.output <- DF.output[,colnames.ordered];

    DF.output[,'DGUID'] <- as.character(           DF.output[,'DGUID'] );
    DF.output[,'dim2' ] <- as.integer(as.character(DF.output[,'dim2' ]));
    DF.output[,'Year' ] <- as.character(           DF.output[,'Year' ] );

    DF.output[,'AvNDVI'] <- (DF.output[,'MEAN'] - 10000) / 10000;
    DF.output[,'AvNDVI'] <- base::round(x = DF.output[,'AvNDVI'], digits = 4);

    colnames.retained <- setdiff(colnames(DF.output),c('MEAN'));
    DF.output <- DF.output[,colnames.retained];

    DF.check <- DF.output[is.na(DF.output[,'DGUID']),];
    if ( nrow(DF.check) > 0 ) {
        cat("\nFILE.dbf = ",FILE.dbf,"\n");
        cat("\nDF.check\n");
        print( DF.check   );
        }

    return( DF.output );

    }
