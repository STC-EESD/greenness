#!/bin/bash

# https://www150.statcan.gc.ca/n1/tbl/csv/38100158-eng.zip

SHP_FILES=( \
    "https://www150.statcan.gc.ca/n1/en/tbl/csv/38100158-eng.zip" \
    # "https://www150.statcan.gc.ca/n1/en/tbl/csv/38100158-eng.zip?st=mEiOZ0O_" \
    )

### ~~~~~~~~~~ ###
dataRepository=~/minio/standard/shared/randd-eesd/001-data-repository/001-acquired/nfis-change
if [ `uname` != "Darwin" ]
then
    cp $0 ${dataRepository}
fi

### ~~~~~~~~~~ ###
for tempzip in "${SHP_FILES[@]}"
do

    echo;echo downloading: ${tempzip}
    wget ${tempzip}
    sleep 5

    tempstem=`basename ${tempzip} .zip`
    tempzip=${tempstem}.zip

    echo unzipping: ${tempzip}
    # unzip ${tempzip} -d ${tempstem}
    unzip ${tempzip}
    sleep 5

    # if [ `uname` != "Darwin" ]
    # then
    #     tempstem=`basename ${tempzip} .zip`
    #     tempzip=${tempstem}.zip

    #     echo unzipping: ${tempzip}
    #     unzip ${tempzip} -d ${tempstem}
    #     sleep 5

    #     # # Copy multiple local folders recursively to MinIO cloud storage.
    #     # echo copying ${tempstem} to ${dataRepository}
    #     # mc-original cp --recursive ${tempstem} ${dataRepository}
    #     # sleep 5

    #     # echo deleting ${tempstem}
    #     # rm -rf ${tempstem}
    #     # sleep 5
    # fi

done
echo

### ~~~~~~~~~~ ###
echo; echo done; echo

### ~~~~~~~~~~ ###
# if [ `uname` != "Darwin" ]
# then
#     if compgen -G "std*" > /dev/null; then
#         cp std* ${dataRepository}
#     fi
# fi
