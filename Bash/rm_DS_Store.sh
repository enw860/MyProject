#!/bin/bash

#remove all .DS_Store of current dir and its sub dir

find . -name ".DS_Store"
temp=`find . -name ".DS_Store"`

if [ "$temp" = "" ]
then
	echo 'There is no .DS_Store files in this dir'
else
	echo 'Remove all .DS_Store file (y or n)?'

	read varname
	if [ "$varname" = "y" ] 
	then
		find . -name ".DS_Store" -delete
		echo 'Done'
	else
		echo 'Never mind'
	fi
fi
exit 0