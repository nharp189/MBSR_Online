#!/bin/bash

Files=`ls *.mt`
echo $Files
UniqueID=`awk '{print $1}' <input.txt`
SubNumber=`awk '{print $2}' <input.txt`
OrigFile=`awk '{print $3}' <input.txt`
Lines=`wc -l input.txt | awk '{print $1}'`
echo "There are $Lines lines in the input.txt file"
echo "$UniqueID"

for i in Files; 
do 
sed -i.bak "s/$UniqueID/$SubNumber/g" i;
done




