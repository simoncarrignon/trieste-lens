import csv
import sys
from bs4 import BeautifulSoup
import urllib,urllib2
import geopy,geopy.distance 


#get_html return the html code of a given page with adresse: url 
def get_html(url):
    request = urllib2.Request(url)
    request.add_header('User-Agent','Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)')
    request.add_header('Content-Type','')
    response = urllib2.urlopen(request)
    return(response)

f = open("countries.csv", 'r')
csvfile=csv.reader(f)
output="country,lat,long,distance\n"
while True:
    try:
        geolocator = geopy.Nominatim()
        location = geolocator.geocode("Trieste")
        break
    except:
        pass

print("Trieste:"+str(location.latitude)+","+str( location.longitude)+"\n")
trieste=(location.latitude,location.longitude)
output=output+"trieste"+","+str(location.latitude)+","+str( location.longitude)+","+"0"+"\n"

for row in csvfile:
    while True:
        try:
            geolocator = geopy.Nominatim()
            location = geolocator.geocode(row[0])
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
        dist=geopy.distance.vincenty(trieste,cur)
        print("distance:"+str(dist))
        output=output+row[0]+","+str(location.latitude)+","+str( location.longitude)+","+str(dist)+"\n"

of = open("localisation.csv", 'w')
of.write(output)
of.close


