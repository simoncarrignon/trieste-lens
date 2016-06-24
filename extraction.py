import csv
import re
from os import listdir
from os.path import isfile, join

path="trade/processed/"
years = [f for f in listdir(path) if isfile(join(path, f))]
print(years)
ofile="year,place,flow,boat"
for year in years:
    print(year)
    filename=path+str(year)+"/"+str(year)+"_1.csv"
    f = open(filename, 'r')
    csvfile=csv.reader(f)
    
    for row in csvfile:
        #print(row)
        text=row[0]
        m1=re.search("(.+) vessel",text)
        m2=re.search("steam",text)
        if(m1):
            print("vessel")
            print(m1.group(1))
            cname=m1.group(1)
            ofile=ofile+str(year)+","+cname+","+row[1]+","+row[2]+",vessel"+"\n"
        if(m2):
            ofile=ofile+str(year)+","+cname+","+row[1]+","+row[2]+",steamboat"+"\n"
        #if( ):
        #    print(text)
        #    spl=text.split(" ")
        #    print(spl[0]+"--"+row[1])
            
    f.close()
        
of = open("final_table.csv", 'w')
of.write(ofile)
of.close

