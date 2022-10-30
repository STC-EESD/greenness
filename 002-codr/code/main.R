
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
require(ggplot2);
require(gtools);
require(litteR);
require(mblm);
require(stringr);
require(tidyr);
require(trend);

# source supporting R code
code.files <- c(
    "attach-Sens-slopes.R",
    "getData-greenness-ndvi.R",
    "initializePlot.R",
    "permutation-test-Sens-slope.R",
    "single-time-series-analysis.R",
    "visualize-Sens-slopes.R"
    # "attach-sampling.R",
    # "get-pcpuids-to-plot.R",
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
data.snapshot <- "2022-10-28.01";
LIST.input <- list(
    greenness = list(
        file     = file.path(data.directory,data.snapshot,"Greenness_All_2022.csv"),
        dim2     = 1L,
        n.digits = 1L
        ),
    ndvi = list(
        file     = file.path(data.directory,data.snapshot,"AverageNDVI_All_2022.csv"),
        dim2     = 2L,
        n.digits = 4L
        )
    );

DF.greeness.ndvi <- getData.greenness.ndvi(
    LIST.input = LIST.input
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.ndvi.Sens.slope <- attach.Sens.slopes(
    FILE.input = LIST.input[['ndvi']][['file']]
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
visualize.Sens.slopes(
    variable       = "NDVI",
    DF.input       = DF.ndvi.Sens.slope,
    DGUIDs.to.plot = c('2021C151005','2021C151004','2021C151003','2021C151002')
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
LIST.output <- permutation.test.Sens.slope(
    DF.input       = DF.ndvi.Sens.slope,
    DGUID.to.test  = '2021C151005',
    n.permutations = 1000L
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
