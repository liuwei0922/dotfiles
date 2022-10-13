#!/bin/bash

files=`find . -name "*.null"`

for file in $files;do
	mv $file "${file%.null}.jpg"
done

find . -name "*.jpg"
