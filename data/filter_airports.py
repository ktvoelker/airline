#!/usr/bin/env python3
import csv
import json


with open('airports.json', 'r') as airports_file:
    airports = json.load(airports_file)

with open('airport_cities.json', 'r') as links_file:
    links = json.load(links_file)

with open('airports.dat', 'r') as openflights_file:
    openflights = csv.reader(openflights_file)
    commercial_codes = set()
    for row in openflights:
        if row[3] == 'United States':
            commercial_codes.add(row[4])


codes = list(airports.keys())
for code in codes:
    if code not in links or code not in commercial_codes:
        del airports[code]


print(json.dumps(airports, indent=2))
