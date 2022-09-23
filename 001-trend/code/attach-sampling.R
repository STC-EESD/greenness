
attach.sampling <- function(
    list.input = NULL
    ) {

    thisFunctionName <- "attach.sampling";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.output <- list();
    # for ( temp.variable in names(list.input) ) {
    for ( temp.variable in c('ndvi') ) {
        DF.input <- list.input[[temp.variable]];
        rownames(DF.input) <- DF.input[,'pcpuid'];
        DF.output <- attach.sampling_get.DF.output(
            DF.input   = DF.input,
            CSV.output = paste0("DF-",temp.variable,"-Sens-slopes-sampling.csv")
            );
        LIST.output[[ temp.variable ]] <- DF.output;
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( LIST.output );

    }

##################################################
attach.sampling_get.DF.output <- function(
    DF.input   = NULL,
    CSV.output = NULL
    ) {

    if ( file.exists(CSV.output) ) {

        DF.output <- read.csv(file = CSV.output);
        colnames(DF.output) <- gsub(x = colnames(DF.output), pattern = "^X", replacement = "");

    } else {

        DF.output <- DF.input;
        my.probs  <- seq(0,9,3)/9;; # seq(0,10,2)/10;

        DF.output[,'quintile.abs.slope'] <- base::cut(
            x              = abs(DF.output[,'trend.slope']),
            breaks         = quantile(x = abs(DF.output[,'trend.slope']), prob = my.probs),
            include.lowest = TRUE
            );

        DF.output[,'quintile.pv'] <- base::cut(
            x      = DF.output[,'trend.slope.pv'],
            breaks = quantile(x = DF.output[,'trend.slope.pv'], prob = my.probs),
            include.lowest = TRUE
            );

        DF.output[,'quintile.R2'] <- base::cut(
            x      = DF.output[,'litteR.R.squared'],
            breaks = quantile(x = DF.output[,'litteR.R.squared'], prob = my.probs),
            include.lowest = TRUE
            );

        DF.output[,'flag.slope'] <- as.numeric(DF.output[,'quintile.abs.slope']);
        DF.output[,'flag.pv'   ] <- as.numeric(DF.output[,'quintile.pv'       ]);
        DF.output[,'flag.R2'   ] <- as.numeric(DF.output[,'quintile.R2'       ]);

        colnames.flag <- grep(x = colnames(DF.output), pattern = "^flag", value = TRUE);
        DF.output[,'stratum'] <- apply(X = DF.output[,colnames.flag], MARGIN = 1, FUN = function(x) {return(paste0(x,collapse=""))});
        DF.output[,'stratum'] <- as.character(DF.output[,'stratum']);
        DF.output <- DF.output[,setdiff(colnames(DF.output),colnames.flag)];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.table.stratum <- as.data.frame(table(DF.output[,c('quintile.abs.slope','quintile.pv','quintile.R2','stratum')]));
        colnames(DF.table.stratum) <- c('range.abs.slope','range.pv','range.R2','stratum','n.pop.centres');
        DF.table.stratum[,'stratum'] <- as.character(DF.table.stratum[,'stratum']);
        DF.table.stratum <- DF.table.stratum[order(DF.table.stratum[,'n.pop.centres'],decreasing = TRUE),];

        DF.table.stratum[,'n.selected'] <- 1;
        DF.table.stratum[DF.table.stratum[,'n.pop.centres'] >  10,'n.selected'] <- 2;
        DF.table.stratum[DF.table.stratum[,'n.pop.centres'] > 100,'n.selected'] <- 5;

        cat("\nstr(DF.table.stratum)\n");
        print( str(DF.table.stratum)   );

        cat("\nDF.table.stratum\n");
        print( DF.table.stratum   );

        cat("\nsum(DF.table.stratum[,'n.pop.centres'])\n");
        print( sum(DF.table.stratum[,'n.pop.centres'])   );

        cat("\nsum(DF.table.stratum[,'n.selected'])\n");
        print( sum(DF.table.stratum[,'n.selected'])   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        selected.pcpuids <- c();
        for ( row.index in seq(1,nrow(DF.table.stratum)) ) {
            temp.stratum    <- DF.table.stratum[row.index,'stratum'   ];
            temp.n.selected <- DF.table.stratum[row.index,'n.selected'];
            temp.pool       <- DF.output[DF.output[,'stratum'] == temp.stratum,'pcpuid'];
            temp.selected   <- sample(
                x       = temp.pool,
                size    = min(temp.n.selected,length(temp.pool)),
                replace = FALSE
                );
            cat("\n### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###\n");
            cat("\ntemp.stratum:",   temp.stratum,   "\n");
            cat("\ntemp.n.selected:",temp.n.selected,"\n");
            cat("\ntemp.pool\n");
            print( temp.pool   );
            cat("\ntemp.selected\n");
            print( temp.selected   );
            selected.pcpuids <- c(selected.pcpuids,temp.selected);
            }

        DF.output[,'selected'] <- 0;
        DF.output[DF.output[,'pcpuid'] %in% selected.pcpuids,'selected'] <- 1;

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        colnames.years     <- grep(x = colnames(DF.output), pattern = "^20", value = TRUE);
        colnames.reordered <- c(setdiff(colnames(DF.output),colnames.years),colnames.years);
        DF.output <- DF.output[,colnames.reordered];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        write.csv(file = CSV.output, x = DF.output, row.names = FALSE);

        write.csv(
            file      = "DF-stratum.csv",
            x         = DF.table.stratum,
            row.names = FALSE
            );

        }

    return( DF.output );

    }
