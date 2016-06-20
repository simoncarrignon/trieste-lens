#!/bin/bash
#simple to batch processing data (./imageclean.sh folder where folder is the folder output)

datafolder=$1
for year in  `ls -d ./$datafolder/*` ;
do
	yearb=`basename $year`
	echo "starting processing data of:"
	echo " $yearb"
	if [ ! -d "result/$yearb" ]; then
		mkdir "result/$yearb"
	fi
	for picture in `ls $year/*.JPG` ;
	do
		id=`basename $picture`
		#echo "    pic:"
		echo "    $id"
		./textcleaner -g -e stretch -f 100 -o 5 -t 30 -u -s 1 -T -p 5 $picture result/$yearb/$id #those parameters could be changed and may give better result
		#echo "    done"
	done
done

