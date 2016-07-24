#!/usr/bin/env python3
import json


with open('airports.json') as airports_file:
    airports = json.load(airports_file)

with open('cities.json') as cities_file:
    cities = json.load(cities_file)


for city in cities.values():
    for airport in city['airports']:
        del airports[airport]


airports_list = list(airports.items())
airports_list.sort(key=lambda p: p[0])
for code, airport in airports_list:
    print('%s %s' % (code, airport['name']))
