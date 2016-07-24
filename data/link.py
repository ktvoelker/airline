#!/usr/bin/env python3
import json
import re
import sys


RE_DROP = re.compile(r'[^a-z]')


def normalize(xs):
    return RE_DROP.sub('', xs.lower())


with open('cities.json', 'r') as cities_file:
    cities = json.load(cities_file)


counties = {}

for city_id, city in cities.items():
    for county in city['counties']:
        state = normalize(county['state'])
        name = normalize(county['county'])
        if state not in counties:
            counties[state] = {}
        counties[state][name] = city_id


with open('airports.json', 'r') as airports_file:
    airports = json.load(airports_file)


airport_cities = {}
unknown_counties = 0

for code, airport in airports.items():
    state = normalize(airport['state'])
    county = normalize(airport['county'])
    if state not in counties:
        raise ValueError('Unknown state in airport: %s.' % airport)
    if county not in counties[state]:
        unknown_counties += 1
        sys.stderr.write('Unknown airport county: %s %s\n' % (state, county))
        continue
    city_id = counties[state][county]
    airport_cities[code] = city_id


if unknown_counties > 0:
    sys.stderr.write('Unknown counties: %d.' % unknown_counties)
print(json.dumps(airport_cities, indent=2))
