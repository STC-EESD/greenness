
getData.greenness.ndvi <- function(
    LIST.input = NULL,
    CSV.output = "DF-CODR-greenness-ndvi.csv"
    ) {

    thisFunctionName <- "getData.greenness.ndvi";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(CSV.output) ) {
        cat("\n");
        cat(paste0("The file ",CSV.output," already exists; loading data ..."));
        cat("\n");

        DF.output <- read.csv(file = CSV.output);

        cat(paste0("\n# ",thisFunctionName,"() exits."));
        cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
        return( DF.output );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- data.frame();
    for ( temp.list in LIST.input ) {
        DF.temp <- getData.greenness.ndvi_read(
            temp.input = temp.list
            );
        DF.output <- rbind(DF.output,DF.temp);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    row.ordering <- order(
        DF.output[,'ReferencePeriod'],
        DF.output[,'dim1'],
        DF.output[,'dim2']
        );
    DF.output <- DF.output[row.ordering,];

    cat("\nstr(DF.output)\n");
    print( str(DF.output)   );

    cat("\nDF.output[1:100,]\n");
    print( DF.output[1:100,]   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    write.csv(
        file      = CSV.output,
        x         = DF.output,
        quote     = FALSE,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
getData.greenness.ndvi_remove.superfluous.rows.columns <- function(
    DF.input = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- DF.input;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    rows.all.NAs <- apply(
        X      = DF.output,
        MARGIN = 1,
        FUN    = function(x) { return( all(is.na(x) | (x == "")) ) }
        );

    DF.output <- DF.output[!rows.all.NAs,];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    removed.colnames  <- grep(x = colnames(DF.output), pattern = "(^X$|^X\\.[0-9]+)", value = TRUE);
    retained.colnames <- setdiff(colnames(DF.output),removed.colnames);
    DF.output         <- DF.output[,retained.colnames];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames(DF.output) <- gsub(
        x           = colnames(DF.output),
        pattern     = "^X",
        replacement = ""
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( DF.output );

    }

getData.greenness.ndvi_read <- function(
    temp.input = NULL
    ) {

    FILE.input       <- temp.input[['file'    ]];
    integer.dim2     <- temp.input[['dim2'    ]];
    integer.n.digits <- temp.input[['n.digits']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.wide <- read.csv(file = FILE.input);
    DF.wide <- getData.greenness.ndvi_remove.superfluous.rows.columns(
        DF.input = DF.wide
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    removed.colnames  <- grep(x = colnames(DF.wide), pattern = "(^dim2$|^DGUID$)", value = TRUE);
    retained.colnames <- setdiff(colnames(DF.wide),removed.colnames);
    DF.wide           <- DF.wide[,retained.colnames];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    vector.dim1 <- DF.wide[,'dim1'];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    vector.years <- grep(x = colnames(DF.wide), pattern = "^[0-9]+$", value = TRUE);

    cat("\nstr(vector.years)\n");
    print( str(vector.years)   );
    cat("\nvector.years\n");
    print( vector.years   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- expand.grid(
        dim1            = vector.dim1,
        ReferencePeriod = vector.years
        );
    DF.output <- DF.output[,c('ReferencePeriod','dim1')];

    cat("\nstr(DF.output)\n");
    print( str(DF.output)   );
    cat("\nsummary(DF.output)\n");
    print( summary(DF.output)   );
    cat("\nhead(DF.output)\n");
    print( head(DF.output)   );

    cat("\nDF.output[is.na(DF.output[,'dim1']),]\n");
    print( DF.output[is.na(DF.output[,'dim1']),]   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # DF.long <- DF.wide %>% tidyr::gather(
    #     key   = "ReferencePeriod",
    #     value = "Value",
    #     vector.years
    #     );

    DF.long <- DF.wide %>% tidyr::pivot_longer(
        cols      = all_of(vector.years),
        names_to  = "ReferencePeriod",
        values_to = "Value"
        );
    DF.long <- as.data.frame(DF.long);

    cat("\nstr(DF.long)\n");
    print( str(DF.long)   );
    cat("\nhead(DF.long)\n");
    print( head(DF.long)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- dplyr::left_join(
        x = DF.output,
        y = DF.long,
        by = c('ReferencePeriod','dim1')
        );
    DF.output <- as.data.frame(DF.output);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output[,"ReferencePeriod"] <- paste0(DF.output[,"ReferencePeriod"],"0101");

    DF.output[,"ReferencePeriod2"] <- "";
    DF.output[,"Symbol"          ] <- 0L;
    DF.output[,"SecurityLevel"   ] <- 0L;
    DF.output[,"dim2"            ] <- integer.dim2;

    DF.output[,"Status"] <- 0L;
    DF.output[is.na(DF.output[,"Value"]),"Status"] <- 1L;

    DF.output[,"Value"] <- (10^integer.n.digits) * round(
        x      = DF.output[,"Value"], # + 1e-6,
        digits = integer.n.digits
        );
    DF.output[,"Value"] <- as.character(DF.output[,"Value"]);
    DF.output[is.na(DF.output[,"Value"]),"Value"] <- "";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ordered.colnames <- c(
        "ReferencePeriod",
        "ReferencePeriod2",
        "Value",
        "Symbol",
        "Status",
        "SecurityLevel",
        "dim1",
        "dim2"
        );

    DF.output <- DF.output[,ordered.colnames];

    cat("\nstr(DF.output)\n");
    print( str(DF.output)   );

    cat("\nsummary(DF.output)\n");
    print( summary(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( DF.output );

    }
