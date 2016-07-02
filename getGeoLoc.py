import csv
import sys
from bs4 import BeautifulSoup
import urllib,urllib2
import geopy,geopy.distance 
from optparse import OptionParser


#get_html return the html code of a given page with adresse: url 
def get_html(url):
    request = urllib2.Request(url)
    request.add_header('User-Agent','Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)')
    request.add_header('Content-Type','')
    response = urllib2.urlopen(request)
    return(response)

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename",
                          help="write report to FILE", metavar="FILE")

(options, args) = parser.parse_args()
filename=options.filename
rootname=filename.split('.')[0]

f = open(filename, 'r')
csvfile=csv.reader(f)
header=csvfile.next()
output="nautical distance,country,port,long,lat\n"

#while True:
#    try:
#        geolocator = geopy.Nominatim()
#        location = geolocator.geocode("Trieste")
#        break
#    except:
#        pass
#
#print("Trieste:"+str(location.latitude)+","+str( location.longitude)+"\n")
#trieste=(location.latitude,location.longitude)
#output=output+"trieste"+","+str(location.latitude)+","+str( location.longitude)+","+"0"+"\n"

for row in csvfile:
    place=""
    if(row[2]==""):
        place=row[1]
    else:
        place=row[2]
        
    print(place)
    while True:
        try:
            geolocator = geopy.Nominatim()
            location = geolocator.geocode(place)
            print("here")
            break
        except :
            e = sys.exc_info()[0]
            if e == geopy.exc.GeocoderQueryError:
                break
            print("fail"+str(e))
            pass
    if(location is not None ):
        cur=(location.latitude,location.longitude)
        #dist=geopy.distance.vincenty(trieste,cur)
        #print("distance:"+str(dist))
        output=output+row[0]+",\""+row[1]+"\",\""+row[2]+"\","+str(location.latitude)+","+str( location.longitude)+"\n"

of = open(""+rootname+"-loc.csv", 'w')
of.write(output)
of.close


