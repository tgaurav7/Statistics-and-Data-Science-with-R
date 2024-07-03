# # !/bin/bash
# procesALLyears.sh

# bash script to call the process.TTC.R file with datafiles from different procesALLyears

# performing a loop for different datafiles
for filename in *csv
do
  # calling the process.TTC.R with different files as argument
  Rscript process.TTC.R $filename
done
