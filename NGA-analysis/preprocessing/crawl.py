'''
# Chris Tan, October 14, 2014
Nigeria polling booths crawler

https://gist.github.com/csytan/01f430c6336da1ab2db8
'''

import csv
import json
import os
import time
import urllib



def load(**kwargs):
    url = 'http://booths.nigeriaelections.org/getStuff.php'
    response = urllib.urlopen(url, urllib.urlencode(kwargs))
    return json.loads(response.read())


def write_csv(items):
    with open('/Users/carbz/Dropbox/booths.csv', 'wb') as csvfile:
        writer = csv.writer(csvfile)
        keys = items[0].keys()
        # write header
        writer.writerow(keys)
        # write lines
        for item in items:
            writer.writerow([item[key].encode('utf8') for key in keys])


def crawl():
    points = []
    
    print('loading states...')
    states = load(stuff='states').keys()
    
    for state in states:
        print('loading lgas for ' + state)
        lgas = load(stuff='lgas', code=state)
        
        for lga_id, lga in lgas.items():
            print('loading lga wards for ' + lga['LgaName'])
            wards = load(stuff='wards', code=state, code2=lga_id)
            
            for ward_id, ward in wards.items():
                print('loading booths for ' + ward['WardName'])
                booths = load(stuff='booths', code=state, code2=lga_id, code3=ward_id)
                booths = booths.values()
                for b in booths:
                    b['state'] = state
                    b['lga'] = lga['LgaName']
                    b['ward'] = ward['WardName']
                points += booths
`
    time.sleep(5)
    
    write_csv(points)


crawl()