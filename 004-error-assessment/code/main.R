
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
require(cowplot);
require(dplyr);
require(ggplot2);
require(sf);
require(tidyr);
require(tmap);
require(units);

# source supporting R code
code.files <- c(
    "attach-error-columns.R",
    "get-DF-check.R",
    "getData-errors.R",
    "getData-Albers.R",
    "getData-codr.R",
    "getData-DGUID-dim1.R",
    "getData-upload.R",
    "initializePlot.R",
    "summarize-visualize-errors.R"
    # "attach-Sens-slopes.R",
    # "collateData.R",
    # "getData-greenness-ndvi.R",
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
data.snapshot <- "2022-12-21.01";
release       <- "Release_20221117";

# CSV.upload <- "38100158_original.csv";

CSV.codr       <- "38100158.csv";
sep.codr       <- ',';
colname.DGNAME <- 'NAME_ENG';

# CSV.codr     <- "38100158_fra.csv";
# sep.codr     <- ';';
# colname.name <- 'NAME_FRA';

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.No.DGUIDs <- base::as.data.frame(readxl::read_excel(
#     path  = file.path(data.directory,data.snapshot,release,"Dim1_DGUID_Relationship.xlsx"),
#     sheet = "No_DGUIDs"
#     ));
# No.DGUIDs <- unique(DF.No.DGUIDs[,'DGUID']);
# print( str(DF.No.DGUIDs) );
# print( No.DGUIDs );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.Albers.230m <- getData.Albers(
#     data.directory       = data.directory,
#     data.snapshot        = data.snapshot,
#     release              = release,
#     CSV.Albers.greenness = "MODISCOMP7d_2000_2022_Albers_Greenness_230m_v2_All.csv",
#     CSV.Albers.NDVI      = "MODISCOMP7d_2000_2022_Albers_AverageNDVI_230m_v2_All.csv",
#     colname.suffix       = "230m"
#     );
#
# cat("\nstr(DF.Albers.230m)\n");
# print( str(DF.Albers.230m)   );
#
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.Albers.10m <- getData.Albers(
#     data.directory       = data.directory,
#     data.snapshot        = data.snapshot,
#     release              = release,
#     CSV.Albers.greenness = "MODISCOMP7d_2000_2022_Albers_Greenness_10m_v2_PC.csv",
#     CSV.Albers.NDVI      = "MODISCOMP7d_2000_2022_Albers_AverageNDVI_10m_v2_PC.csv",
#     colname.suffix       = "230m"
#     );
#
# cat("\nstr(DF.Albers.10m)\n");
# print( str(DF.Albers.10m)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.errors <- getData.errors(
    data.directory = data.directory,
    data.snapshot  = data.snapshot,
    release        = release,
    CSV.codr       = CSV.codr,
    colname.DGNAME = colname.DGNAME,
    sep.codr       = sep.codr,
    CSV.output     = "DF-errors.csv",
    CSV.Albers.greenness.10m  = "MODISCOMP7d_2000_2022_Albers_Greenness_10m_v2_PC.csv",
    CSV.Albers.NDVI.10m       = "MODISCOMP7d_2000_2022_Albers_AverageNDVI_10m_v2_PC.csv",
    CSV.Albers.greenness.230m = "MODISCOMP7d_2000_2022_Albers_Greenness_230m_v2_All.csv",
    CSV.Albers.NDVI.230m      = "MODISCOMP7d_2000_2022_Albers_AverageNDVI_230m_v2_All.csv"
    );

cat("\nstr(DF.errors)\n");
print( str(DF.errors)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
summarize.visualize.errors(
    DF.errors = DF.errors
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.upload <- getData.upload(
#     data.directory = data.directory,
#     data.snapshot  = data.snapshot,
#     release        = release,
#     CSV.upload     = CSV.upload
#     );
#
# length(setdiff(unique(DF.upload[,'DGUID']),No.DGUIDs));
# length(setdiff(No.DGUIDs,unique(DF.upload[,'DGUID'])));

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# row.index.warning <- which(grepl(x = DF.codr[,'REF_DATE'], pattern = "warning", ignore.case = TRUE));
# print(row.index.warning);
#
# str( DF.codr[seq(row.index.warning,nrow(DF.codr)),] );
# DF.codr <- DF.codr[seq(1,row.index.warning-1),];

# length(setdiff(unique(DF.codr[,'DGUID']),No.DGUIDs));
# length(setdiff(No.DGUIDs,unique(DF.codr[,'DGUID'])));

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# length(unique(DF.codr[DF.codr[,'DGUID'] %in% No.DGUIDs,"DGUID"]));
#
# DGUIDs.NA.codr <- unique(DF.codr[is.na(DF.codr[,"greenness.value.codr"]),"DGUID"]);
#
# setdiff(DGUIDs.NA.codr,No.DGUIDs);
# setdiff(No.DGUIDs,DGUIDs.NA.codr);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.codr[DF.codr[,'DGUID'] %in% No.DGUIDs,"DGUID"] <- "";
# DF.codr <- DF.codr[DF.codr[,'DGUID'] != "",];
#
# write.csv(
#     file = gsub(x = CSV.codr, pattern = "\\.csv", replacement = "-NO-DGUIDs.csv"),
#     x    = DF.codr
#     );
#
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# DF.check <- get.DF.check(
#     DF.DGUID.dim1 = DF.DGUID.dim1,
#     DF.codr       = DF.codr,
#     DF.upload     = DF.upload,
#     colname.name  = colname.name
#     );
#
# print( summary(DF.check[!is.na(DF.check[,'value.codr']),'diff']) );
#
# print( summary(DF.check[ is.na(DF.check[,'value.codr']),]) );
#
# temp.DGUIDs <- unique(DF.check[is.na(DF.check[,'value.codr']),'DGUID']);
# setdiff(temp.DGUIDs,No.DGUIDs);
# setdiff(No.DGUIDs,temp.DGUIDs);
#
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# data.snapshot <- "2022-12-19.01";
#
# SF.boundaries <- sf::st_read(
#     dsn = file.path(data.directory,data.snapshot,"lpc_000b21a_e","lpc_000b21a_e.shp")
#     );
# SF.boundaries[,'area'] <- as.numeric(sf::st_area(sf::st_geometry(SF.boundaries))) / 1e6;
# cat("\nstr(SF.boundaries)\n");
# print( str(SF.boundaries)   );
# cat("\nsummary(SF.boundaries)\n");
# print( summary(SF.boundaries)   );
#
# SF.centroids <- SF.boundaries;
# sf::st_geometry(SF.centroids) <- sf::st_centroid(sf::st_geometry(SF.boundaries));
# cat("\nstr(SF.centroids)\n");
# print( str(SF.centroids)   );
# cat("\nsummary(SF.centroids)\n");
# print( summary(SF.centroids)   );
#
# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# SHP.canada <- sf::st_read(
#     dsn = file.path(data.directory,data.snapshot,"lpr_000a21a_e","lpr_000a21a_e.shp")
#     );
# cat("\nstr(SHP.canada)\n");
# print( str(SHP.canada)   );
#
# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# my.tmap <- tmap::tm_shape(SHP.canada) + tmap::tm_borders();
# my.tmap <- my.tmap + tmap::tm_shape(SF.centroids) + tmap::tm_dots(
#     size  = "area",
#     col   = "orange",
#     alpha = 0.5
#     );
#
# cat("\nstr(my.tmap)\n");
# print( str(my.tmap)   );
#
# tmap::tmap_save(
#     tm       = my.tmap,
#     filename = "tmap-canada.png",
#     width    = 16,
#     # height =  8,
#     units    = "in",
#     dpi      = 300
#     );
#
# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# my.ggplot <- initializePlot();
# my.ggplot <- my.ggplot + ggplot2::geom_sf(
#     data   = SHP.canada,
#     colour = "black",
#     size   = 0.9,
#     fill   = NA
#     );
# my.ggplot <- my.ggplot + ggplot2::geom_sf(
#     data    = SF.centroids,
#     mapping = aes(geometry = geometry, size = area),
#     color   = "orange",
#     alpha   = 0.5
#     );
# # my.ggplot <- my.ggplot + ggplot2::scale_size_continuous(range = c(1^2, 2000^2));
#
# ggplot2::ggsave(
#     plot     = my.ggplot,
#     filename = "ggplot2-canada.png",
#     width    = 16,
#     height   = 16,
#     units    = "in",
#     dpi      = 300
#     );
#
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
print( warnings() );

print( getOption('repos') );

print( .libPaths() );

print( sessionInfo() );

print( format(Sys.time(),"%Y-%m-%d %T %Z") );

stop.proc.time <- proc.time();
print( stop.proc.time - start.proc.time );
