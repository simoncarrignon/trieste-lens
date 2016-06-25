import csv
import re
from os import listdir
from os.path import isfile, join

path="trade/processed/"
years =listdir(path)
print(years)
ofile="year,country,boat,volume,type\n"

for year in years:
    print(year)
    filename=path+str(year)+"/"+str(year)+"_1.csv"
    f = open(filename, 'r')
    csvfile=csv.reader(f)
    
    for row in csvfile:
        #print(row)
        text=row[0]
        text=text.lower()
        m1=re.search("(.+) vessel",text)
        m2=re.search("steam",text)
        if(m1):
            print("vessel")
            print(m1.group(1))
            cname=m1.group(1)
            btype="vessel"
            #ofile=ofile+str(year)+","+cname+","+row[1]+","+row[2]+",vessel"+"\n"
        elif(m2):
            btype="steamboat"
            #ofile=ofile+str(year)+","+cname+","+row[1]+","+row[2]+",steamboat"+"\n"
        else:
            print("nothing")
            cname=text
        btype="vessel"
        ofile=ofile+str(year)+","+cname+","+row[1]+","+row[2]+","+btype+"\n"
        #if( ):
        #    print(text)
        #    spl=text.split(" ")
        #    print(spl[0]+"--"+row[1])
            
    f.close()
        
of = open("final_table.csv", 'w')
of.write(ofile)
of.close

