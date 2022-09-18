
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
    "attach-Sens-slopes.R",
    "getData-greenness.R",
    "get-pcpuids-to-plot.R",
    "initializePlot.R",
    "single-time-series-analysis.R",
    "visualize-Sens-slopes.R"
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
LIST.Sens.slopes <- attach.Sens.slopes(list.input = LIST.input);

cat("\nstr(LIST.Sens.slopes)\n");
print( str(LIST.Sens.slopes)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
pcpuids.to.plot <- get.pcpuids.to.plot(
    list.Sens.slopes = LIST.Sens.slopes
    );

cat("\nlength(pcpuids.to.plot)\n");
print( length(pcpuids.to.plot)   );

cat("\npcpuids.to.plot\n");
print( pcpuids.to.plot   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
visualize.Sens.slopes(
    list.input      = LIST.Sens.slopes,
    pcpuids.to.plot = pcpuids.to.plot
  # pcpuids.to.plot = NULL
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# row.index = 993;
# test_single.time.series.analysis(
#     DF.input  = LIST.input[['ndvi']],
#     row.index = row.index
#     );
#
# cat("\nLIST.Sens.slopes[['ndvi']][row.index,]\n");
# print( LIST.Sens.slopes[['ndvi']][row.index,]   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );
