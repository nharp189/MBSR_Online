#!/bin/csh
#
set folders = folders.txt

set linecount = `wc -l $folders | awk '{print $1}'`

@ k = 1
while ($k <= linecount)

	set subdir = `awk -v i=$k 'FNR == i {print $1}' $folders`
	set vcnum = `awk -v i=$k 'FNR == i {print $2}' $folders'
	sed -i ' "s/$var1/$var2/g" 	

sed -i 's/original/new/g' file.txt
