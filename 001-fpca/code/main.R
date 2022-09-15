
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
require(arrow);
require(fda);
require(ggplot2);
require(stringr);
require(tidyr);

# source supporting R code
code.files <- c(
    "fpcFeatureEngine.R"
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
FILE.green <- file.path(data.directory,"2022-09-14.01","38100149.csv");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.green <- read.csv(file = FILE.green);

cat("\nstr(DF.green)\n");
print( str(DF.green)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.ndvi <- DF.green[DF.green[,'Urban.greenness'] == "Average normalized difference vegetation index (NDVI)",c("REF_DATE","DGUID","VALUE")];

colnames(DF.ndvi) <- tolower(colnames(DF.ndvi));
colnames(DF.ndvi) <- gsub(
    x           = colnames(DF.ndvi),
    pattern     = "ref_date",
    replacement = "year"
    );
colnames(DF.ndvi) <- gsub(
    x           = colnames(DF.ndvi),
    pattern     = "value",
    replacement = "ndvi"
    );

cat("\nstr(DF.ndvi)\n");
print( str(DF.ndvi)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );
