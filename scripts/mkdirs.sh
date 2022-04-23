#!/bin/bash

path="/home/zihua/scripts"
names=`cat "$path/name.txt"`
folder="物理19-4班$1"

cd /mnt/c/Users/刘伟/OneDrive\ -\ cumt.edu.cn/桌面/
mkdir -p $folder
cd $folder

for name in $names;do
	mkdir -p $name
done
