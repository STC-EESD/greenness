
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
    "getData-greenness.R",
    "single-time-series-analysis.R"
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
FILE.input <- file.path(data.directory,"2022-09-16.01","FromNickPC_2000_2022_GreennessAverageNDVI.xlsx");
LIST.input <- getData.greenness(path = FILE.input);

cat("\nstr(LIST.input)\n");
print( str(LIST.input)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.temp <- LIST.input[['greenness']];
# rownames(DF.temp) <- DF.temp[,'pcpuid'];
# temp.series <- as.numeric(DF.temp[1,setdiff(colnames(DF.temp),c('pcpuid','pcname','pruid','pcclass'))]);
# temp.ts <- stats::ts(data = temp.series, start = c(2000,1), frequency = 1);
# results.SenSlope <- trend::sens.slope(x = temp.ts);

DF.temp <- LIST.input[['greenness']];
rownames(DF.temp) <- DF.temp[,'pcpuid'];
temp.output <- t(apply(
    X      = DF.temp[,setdiff(colnames(DF.temp),c('pcpuid','pcname','pruid','pcclass'))],
    MARGIN = 1,
    FUN    = single.time.series.analysis
    ));

cat("\nstr(temp.output)\n");
print( str(temp.output)   );

cat("\ntemp.output[1:10,]\n");
print( temp.output[1:10,]   );

cat("\nsummary(temp.output)\n");
print( summary(temp.output)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );
