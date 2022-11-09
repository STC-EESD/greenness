
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
    # "attach-Sens-slopes.R",
    # "collateData.R",
    # "getData-greenness-ndvi.R",
    # "initializePlot.R",
    # "permutation-test-Sens-slope.R",
    # "single-time-series-analysis.R",
    # "visualize-Sens-slopes.R"
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
data.snapshot <- "2022-11-09.01";

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.DGUID.dim1 <- base::as.data.frame(readxl::read_excel(
    path  = file.path(data.directory,data.snapshot,"Dim1_DGUID_Relationship.xlsx"),
    sheet = "Sheet1"
    ));
colnames(DF.DGUID.dim1) <- gsub(
    x           = colnames(DF.DGUID.dim1),
    pattern     = "Dim1",
    replacement = "dim1"
    );
DF.DGUID.dim1 <- DF.DGUID.dim1[,c('DGUID','dim1')];
print( str(DF.DGUID.dim1) );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.upload <- read.csv(
    file = file.path(data.directory,data.snapshot,"38100158_data.csv"),
    );
DF.upload <- dplyr::left_join(
    x  = DF.upload,
    y  = DF.DGUID.dim1[,c('DGUID','dim1')],
    by = 'dim1'
    );
DF.upload[,'ReferencePeriod'] <- as.character(DF.upload[,'ReferencePeriod']);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.codr <- read.csv(
    file = file.path(data.directory,data.snapshot,"38100158_2000-01-01_2022-01-01.csv"),
    );
str( DF.codr[DF.codr[,'DGUID'] == "",] );
DF.codr <- DF.codr[DF.codr[,'DGUID'] != "",]

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
print( str(DF.upload) );
print( str(DF.codr) );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DGUIDs.NA.uplaod <- DF.upload[is.na(DF.upload[,"Value"]),"DGUID"];
DGUIDs.NA.codr   <- DF.codr[  is.na(DF.codr[,  "VALUE"]),"DGUID"];

setdiff(DGUIDs.NA.uplaod,DGUIDs.NA.codr);

setdiff(DGUIDs.NA.codr,DGUIDs.NA.uplaod);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.check <- DF.codr[,c('REF_DATE','DGUID','Urban.greenness','VALUE','DECIMALS')];

DF.check[,'ReferencePeriod'] <- paste0(DF.check[,'REF_DATE'],"0101");

DF.check[,'dim2'] <- 2L;
DF.check[DF.check[,'Urban.greenness'] == "Average greenness",'dim2'] <- 1L;

DF.check <- DF.check[,c('ReferencePeriod','DGUID','dim2','VALUE','DECIMALS')];
colnames(DF.check) <- gsub(
    x           = colnames(DF.check),
    pattern     = "VALUE",
    replacement = "value.codr"
    );

# DF.check[,'value.codr.check'] <- as.integer(DF.check[,'value.codr'] * (10^DF.check[,'DECIMALS']));
DF.check[,'value.codr.check'] <- DF.check[,'value.codr'] * (10^DF.check[,'DECIMALS']);

DF.check <- dplyr::left_join(
    x  = DF.check,
    y  = DF.upload[,c('ReferencePeriod','DGUID','dim2','Value')],
    by = c('ReferencePeriod','DGUID','dim2')
    );
colnames(DF.check) <- gsub(
    x           = colnames(DF.check),
    pattern     = "Value",
    replacement = "value.upload"
    );

print( str(DF.check) );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.check[,'diff'] <- abs(DF.check[,'value.codr.check'] - DF.check[,'value.upload']);

print( summary(DF.check[!is.na(DF.check[,'value.codr']),'diff']) );

print( summary(DF.check[ is.na(DF.check[,'value.codr']),]) );

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );
