#!/bin/bash

Files=`ls *.mt`
echo $Files
Lines=`wc -l input.txt | awk '{print $1}'`
echo "There are $Lines lines in the input.txt file"

for i in Lines
do
UniqueID=`awk '{print $1}' <input.txt`
SubNumber=`awk '{print $2}' <input.txt`
OrigFile=`awk '{print $3}' <input.txt`
NewFile=`awk '{print $4}' <input.txt`
sed -i "s/$UniqueID/$SubNumber/g" $OrigFile>$NewFile
done




