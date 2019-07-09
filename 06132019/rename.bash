#!/bin/bash


UniqueID = `awk '{print $1}' <input`
SubNumber = `awk '{print $2} <input`
OrigFile = `awk '{print $3}' <input`
NewFile = `awk '{print $4} <input`

For 1:i in OrigFile do {

sed "s/UniqueID/SubNumber/g" OrigFile>NewFile


}


