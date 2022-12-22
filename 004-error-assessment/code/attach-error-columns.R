
attach.error.columns <- function(
    DF.input  = NULL,
    variables = c('greenness','NDVI'),
    versions  = c('codr','230m','10m')
    ) {

    thisFunctionName <- "attach.error.columns";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.version.pairs <- t(combn(versions,2));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- DF.input;
    for ( temp.variable in variables ) {
    for ( row.index in seq(1,nrow(DF.version.pairs)) ) {

        version.1 <- DF.version.pairs[row.index,1];
        version.2 <- DF.version.pairs[row.index,2];

        column.1 <- paste(temp.variable,"value",version.1,sep=".");
        column.2 <- paste(temp.variable,"value",version.2,sep=".");

        temp.err     <- paste(temp.variable,"err",    version.1,version.2,sep=".");
        temp.rel.err <- paste(temp.variable,"rel.err",version.1,version.2,sep=".");

        DF.output[,temp.err] <- apply(
            X      = DF.output[,c(column.1,column.2)],
            MARGIN = 1,
            FUN    = function(x) { return( x[2] - x[1] ) }
            );

        DF.output[,temp.rel.err] <- apply(
            X      = DF.output[,c(column.1,column.2,temp.err)],
            MARGIN = 1,
            FUN    = function(x) {
                # return( ifelse(any(is.na(x[c(1,2)])), NA, x[3]/mean(x[1],x[2])) )
                if ( any(is.na(x[c(1,2)])) ) {
                    return( NA );
                } else if ( sum(abs(x[c(1,2)])) < 1e-20 ) {
                    return( 0 );
                } else {
                    return( x[3] / mean(c(x[1],x[2])) );
                    }
                }
            );

        }}

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
