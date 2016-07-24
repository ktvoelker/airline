#!/usr/bin/env python3
import csv
import json
import re


RE_COUNTY = re.compile(r'^(.*?)( (County|Parish|city))?, ([A-Z]{2})$')
CBSA_TYPES = {
    'metropolitan statistical area',
    'micropolitan statistical area',
}


def parse_county(xs: str) -> (str, str):
    m = RE_COUNTY.match(xs)
    if m is None:
        raise ValueError('Failed to parse county: %s.' % xs)
    return {'state': m.group(4), 'county': m.group(1)}


with open('CSA-EST2015-alldata.csv', 'r', encoding='ISO-8859-1') as raw_handle:
    raw = csv.reader(raw_handle)
    next(raw)

    csas = {}
    cbsa_ids = set()

    for row in raw:
        ty = row[5].lower()
        if ty == 'combined statistical area':
            csas[row[0]] = {
                'name': row[4],
                'population': row[13],
                'counties': [],
            }
        elif ty in CBSA_TYPES:
            cbsa_ids.add(row[1])
        elif ty == 'metropolitan division':
            continue
        elif ty == 'county or equivalent':
            csas[row[0]]['counties'].append(parse_county(row[4]))
        else:
            raise ValueError('Unknown row type in row: %s.' % row)


with open('CBSA-EST2015-alldata.csv', 'r', encoding='ISO-8859-1') as raw_handle:
    raw = csv.reader(raw_handle)
    next(raw)

    cbsas = {}

    for row in raw:
        ty = row[4].lower()
        if ty == '':
            continue
        elif ty in CBSA_TYPES:
            if row[0] in cbsa_ids:
                continue
            cbsas[row[0]] = {
                'name': row[3],
                'population': row[12],
                'counties': [],
            }
        elif ty == 'metropolitan division':
            continue
        elif ty == 'county or equivalent':
            if row[0] in cbsas:
                cbsas[row[0]]['counties'].append(parse_county(row[3]))
        else:
            raise ValueError('Unknown row type in row: %s.' % row)


cities = csas.copy()
cities.update(cbsas)


print(json.dumps(cities, indent=2))
