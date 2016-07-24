#!/usr/bin/env python3
import csv
import json
import re
import sys


RE_GEO = re.compile(r'^(\d+)-(\d+)-(\d+\.\d+)([NSEW])$')


def geo_sign(xs: str) -> int:
    return 1 if xs in {'N', 'E'} else -1


def parse_geo(xs: str) -> float:
    m = RE_GEO.match(xs)
    if m is None:
        raise ValueError('Could not parse geo %s' % xs)
    degrees = int(m.group(1))
    minutes = int(m.group(2))
    seconds = float(m.group(3))
    return geo_sign(m.group(4)) * (degrees + (minutes / 60) + (seconds / 3600))


facilities = {}

with open('APM-Report-45560.csv', 'r') as apm_file:
    apm = csv.reader(apm_file)
    next(apm)
    for row in apm:
        facility = row[0]
        departures = row[3]
        arrivals = row[4]
        facilities[facility] = {
            'departures': int(departures),
            'arrivals': int(arrivals),
        }

with open('airports.csv', 'r') as airports_file:
    airports = csv.reader(airports_file)
    next(airports)
    for row in airports:
        long_name = row[6]
        facility = row[9]
        latitude = row[11]
        longitude = row[12]
        if facility not in facilities:
            continue
        facilities[facility]['name'] = long_name
        facilities[facility]['latitude'] = parse_geo(latitude)
        facilities[facility]['longitude'] = parse_geo(longitude)

for name, facility in facilities.items():
    if 'latitude' not in facility:
        sys.stderr.write('Warning: no location for %s.\n' % name)

print(json.dumps(facilities, indent=2))
