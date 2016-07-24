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

with open('airports.csv', 'r') as airports_file:
    airports = csv.reader(airports_file)
    next(airports)
    for row in airports:
        try:
            fac_id = row[9]
            if len(fac_id) != 3 or not fac_id.isalpha():
                continue
            facilities[fac_id] = {
                'state': row[1],
                'city': row[4],
                'county': row[5],
                'name': row[6],
                'latitude': parse_geo(row[11]),
                'longitude': parse_geo(row[12])
            }
        except ValueError:
            sys.stderr.write('Bad line %s\n' % row)

with open('APM-Report-45560.csv', 'r') as apm_file:
    apm = csv.reader(apm_file)
    next(apm)
    for row in apm:
        facility = facilities[row[0]]
        facility['departures'] = row[3]
        facility['arrivals'] = row[4]

print(json.dumps(facilities, indent=2))
