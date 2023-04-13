
command.arguments <- commandArgs(trailingOnly = TRUE);
data.directory    <- normalizePath(command.arguments[1]);
code.directory    <- normalizePath(command.arguments[2]);
output.directory  <- normalizePath(command.arguments[3]);

print( data.directory );
print( code.directory );
print( output.directory );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

start.proc.time <- proc.time();

# set working directory to output directory
setwd( output.directory );

##################################################
require(dplyr);
require(tidyr);

# source supporting R code
code.files <- c(
    );

for ( code.file in code.files ) {
    source(file.path(code.directory,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
my.seed <- 7654321;
set.seed(my.seed);

is.macOS  <- grepl(x = sessionInfo()[['platform']], pattern = 'apple', ignore.case = TRUE);
n.cores   <- ifelse(test = is.macOS, yes = 2, no = parallel::detectCores() - 1);
cat(paste0("\n# n.cores = ",n.cores,"\n"));

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.snapshot <- "2023-04-13.01";

CSV.upload <- "38100158_original.csv";

CSV.codr     <- "38100158.csv";
sep.codr     <- ',';
colname.name <- 'NAME_ENG';

# CSV.codr     <- "38100158_fra.csv";
# sep.codr     <- ';';
# colname.name <- 'NAME_FRA';

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.branch <- read.csv(
    file = file.path(data.directory,"2023-04-13.02","branch-greenness-averages.csv"),
    sep  = sep.codr
    );
print(str(DF.branch));

DF.codr <- read.csv(
    file = file.path(data.directory,data.snapshot,CSV.codr),
    sep  = sep.codr
    );
print(str(DF.codr));

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
branch.validate <- function(
    DF.input   = NULL,
    DF.branch  = NULL,
    year.first = NULL,
    year.last  = NULL
    ) {

    retained.colnames <- c(
        'REF_DATE',
        'GEO',
        'Urban.greenness',
        'VALUE'
        );

    DF.selected <- DF.input[,retained.colnames];
    colnames(DF.selected) <- gsub(
        x           = colnames(DF.selected),
        pattern     = "Urban.greenness",
        replacement = "variable"
        );
    colnames(DF.selected) <- gsub(
        x           = colnames(DF.selected),
        pattern     = "VALUE",
        replacement = "value"
        );

    DF.selected[,'variable'] <- gsub(
        x           = DF.selected[,'variable'],
        pattern     = "Average greenness",
        replacement = "greenness"
        );

    DF.selected[,'variable'] <- gsub(
        x           = DF.selected[,'variable'],
        pattern     = "Average normalized difference vegetation index \\(",
        replacement = ""
        );

    DF.selected[,'variable'] <- gsub(
        x           = DF.selected[,'variable'],
        pattern     = "\\)",
        replacement = ""
        );

    selected.rows <- DF.selected[,'REF_DATE'] %in% seq(year.first,year.last);
    DF.selected <- DF.selected[selected.rows,];

    selected.rows <- grepl(x = DF.selected[,'GEO'], pattern = "all population centres");
    DF.selected <- DF.selected[selected.rows,];

    selected.rows <- grepl(x = DF.selected[,'GEO'], pattern = "small population centres");
    DF.selected <- DF.selected[!selected.rows,];

    print(str(DF.selected));
    print(summary(DF.selected));

    DF.check <- DF.selected %>%
        dplyr::group_by(GEO,variable) %>%
        summarise(value = mean(value));
    DF.check <- as.data.frame(DF.check);
    DF.check[,'GEO'] <- gsub(
        x           = DF.check[,'GEO'],
        pattern     = ", all population centres",
        replacement = ""
        );
    DF.check <- DF.check[DF.check[,'variable'] == 'greenness',];
    cat("\nDF.check\n");
    print( DF.check   );

    DF.check[,'value'] <- round(x = DF.check[,'value'], digits = 1);

    branch.colname <- paste0('branch_',year.first,'_',year.last);
    DF.check <- merge(
        x  = DF.check,
        y  = DF.branch[,c('GEO',branch.colname)],
        by = 'GEO'
        );
    DF.check[,'diff'] <- DF.check[,'value'] - DF.check[,branch.colname];
    cat("\nDF.check\n");
    print( DF.check   );

    write.csv(
        x         = DF.check,
        file      = paste0("DF-check-",year.first,"-",year.last,".csv"),
        row.names = FALSE
        )

    }


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
branch.validate(
    DF.input   = DF.codr,
    DF.branch  = DF.branch,
    year.first = 2000,
    year.last  = 2004
    );

branch.validate(
    DF.input   = DF.codr,
    DF.branch  = DF.branch,
    year.first = 2018,
    year.last  = 2022
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );
