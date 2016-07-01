import csv
import sys
from bs4 import BeautifulSoup
import urllib,urllib2
import geopy,geopy.distance 
from optparse import OptionParser


#Split a file given a event (long,lat) and a range of the even
def split_set(mainfile,eventname,coor,dist):
    print(eventname+":"+dist)
    f = open(mainfile, 'r')
    csvfile=csv.DictReader(f)
    field=csvfile.fieldnames
    print(field)
    inf = open("splits/in-"+eventname+"-"+mainfile,'w')
    inwrite=csv.DictWriter(inf,fieldnames=field)
    inwrite.writeheader()
    ouf = open("splits/out-"+eventname+"-"+mainfile,'w')
    ouwrite=csv.DictWriter(ouf,fieldnames=field)
    ouwrite.writeheader()
    #of.close
    #of = open("out-"+eventname+"-"+mainfile,'w')
    #of.write(row)
    #of.close
    for row in csvfile:
        if(row["long"] != 'NA'):
            cur=(row["long"],row["lat"])
            cdist=geopy.distance.vincenty(coor,cur).miles
            print(str(cdist)+" vs "+str(dist))
            if(int(cdist) < int(dist)):
                print("in: "+row["country"])
                inrange=row
                inwrite.writerow(row)
            else:
                print("out: "+row["country"])
                outrange=row
                ouwrite.writerow(row)
    ouf.close
    inf.close

def main():
    parser = OptionParser()
    parser.add_option("-f", "--file", dest="filename",
                              help="split FILE given differents EVENTS", metavar="FILE")
    parser.add_option("-e", "--events", dest="efilename",
                              help="use file EVENTS as list of events", metavar="EVENTS")
    
    (options, args) = parser.parse_args()
    #import_file=
    #export_file=
    filename=options.filename 
    efilename=options.efilename 
    evname="test"
    efile=open(efilename,'r')
    evdict=csv.DictReader(efile)
    for row in evdict:
        evcoor=(row["long"],row["lat"])
        split_set(filename,row["id"],evcoor,row["mag"])


main()
