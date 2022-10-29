
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
require(deming);
require(fda);
require(ggplot2);
require(litteR);
require(mblm);
require(stringr);
require(tidyr);
require(trend);

# source supporting R code
code.files <- c(
    # "attach-sampling.R",
    # "attach-Sens-slopes.R",
    # "getData-greenness.R",
    # "get-pcpuids-to-plot.R",
    # "initializePlot.R",
    # "single-time-series-analysis.R",
    # "visualize-Sens-slopes.R"
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
# FILE.input <- file.path(data.directory,"2022-10-28.01","AverageNDVI_All_2022.csv");
# LIST.input <- getData.greenness(path = FILE.input);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
FILE.input <- file.path(data.directory,"2022-10-28.01","AverageNDVI_All_2022.csv");

DF.ndvi.wide <- read.csv(file = FILE.input);
cat("\nstr(DF.ndvi.wide)\n");
print( str(DF.ndvi.wide)   );

summary( DF.ndvi.wide[,c('X','X.1','X.2','X.3')] );

removed.colnames  <- grep(x = colnames(DF.ndvi.wide), pattern = "(^dim2$|^DGUID$|^X$|^X\\.[0-9]+)", value = TRUE);
cat("\nremoved.colnames\n");
print( removed.colnames   );

retained.colnames <- setdiff(colnames(DF.ndvi.wide),removed.colnames);
cat("\nretained.colnames\n");
print( retained.colnames   );

DF.ndvi.wide <- DF.ndvi.wide[,retained.colnames];
colnames(DF.ndvi.wide) <- gsub(
    x           = colnames(DF.ndvi.wide),
    pattern     = "^X",
    replacement = ""
    );
cat("\nstr(DF.ndvi.wide)\n");
print( str(DF.ndvi.wide)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.ndvi.long <- DF.ndvi.wide %>% tidyr::gather(
    key   = "dim1",
    value = "ndvi"
    );

DF.ndvi.long[,"ReferencePeriod2"] <- "";
DF.ndvi.long[,"Symbol"          ] <- 0L;
DF.ndvi.long[,"SecurityLevel"   ] <- 0L;
DF.ndvi.long[,"dim2"            ] <- 2L;

cat("\nstr(DF.ndvi.long)\n");
print( str(DF.ndvi.long)   );

cat("\nsummary(DF.ndvi.long)\n");
print( summary(DF.ndvi.long)   );

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );
