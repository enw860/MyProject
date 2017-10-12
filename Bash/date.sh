#!/bin/bash

#Description: this bash is using for sorting file or files in dir to 
#sepecific dir by created date

#should be exactly 2 arguement
if [ $# != 2 ]
then 
	echo "USAGE: ./date.sh sorce_dir dest_dir"
	exit 1
fi

#import the sorce and destnation file and dir
dir="$1"
dest="$2"

#make sure dest dir exist
if [ ! -d $dest ]
then 
	#make sure arguement for dest is not a file
	if [ -f $dest ]
	then
		echo "not valid dest dir"
		ecit 1
	fi

	#create the dest dir if it is not exited
	mkdir $dest
fi

#looping the sorce if it is a dir
if [ -d $dir ]
then
	#looping each files and sorting them to sepecific dir is dest dir
	#base on the created date
	for f in `ls $dir`
	do 
		#file path
		file="$dir/$f"
		
		#recursively looping the file
		if [ -d $file ]
		then
			./date.sh $file $dest
		else
			if [ -f $file ]
			then
				info=`ls -Ul $file`
				set echo $info

				if [ ! -d $dest/$7_$8 ] 
				then 
					mkdir $dest/$7_$8
				fi 

				cp $file $dest/$7_$8
			else
				echo "skip: $file not a file"
			fi
		fi
	done
else
	#just in the case that input sorce is a file
	file=$dir
	if [ -f $file ]
	then
		info=`ls -Ul $file`
		set echo $info

		if [ ! -d $dest/$7_$8 ] 
		then 
			mkdir $dest/$7_$8
		fi 

		cp $file $dest/$7_$8
	else 
		echo "skip:$file not a file"
	fi
fi
exit 0