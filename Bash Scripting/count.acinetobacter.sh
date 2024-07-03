#!/bin/bash
# count.acinetobacter.sh

echo Working with data file ${1}.

k=$(grep Acinetobacter ${1} | wc -l)
t=$(grep Acinetobacter ${1} | grep WP_005 | wc -l)

echo The total number of Acinetobacter entries is $k.
echo The number of Acinetobacter entries with an accession code containing WP_005 is $t


