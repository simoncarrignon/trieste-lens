#This python script process the database to create unified csv file
import csv
import re
from os import listdir
from os.path import isfile, join

def read_csv(filename):
    f = open(filename, 'r')
    csvfile=csv.reader(f)
    
    ofile=""
    for row in csvfile:
        #print(row)
        text=row[0]
        text=text.lower()
        m1=re.search("(.+) vessel",text)
        m2=re.search("steam",text)
        if(m1):
            #print("vessel")
            print(m1.group(1))
            cname=m1.group(1)
            btype="vessel"
            #ofile=ofile+str(year)+","+cname+","+row[1]+","+row[2]+",vessel"+"\n"
        elif(m2):
            btype="steam"
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
    return(ofile)

path="trade/processed/"
years =listdir(path)
print(years)
ofilei="year,country,boat,volume,type\n"
ofilee="year,country,boat,volume,type\n"

for year in years:
    print(year)
    filenamei=path+str(year)+"/"+str(year)+"_1.csv"
    filenamee=path+str(year)+"/"+str(year)+"_2.csv"
    ofilei=ofilei+read_csv(filenamei)
    ofilee=ofilee+read_csv(filenamee)
        
        
of = open("final_table-import.csv", 'w')
of.write(ofilei)
of.close

of = open("final_table-export.csv", 'w')
of.write(ofilee)
of.close
