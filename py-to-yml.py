#!/usr/bin/env python
import yaml

class JACoW:
    pass

mypaper = {}
mypaper['title'] = {
    'caption': 'This is a title',
    'funds': ['Fund A', 'Fund B']
}

inst1 = {
    'name': 'Inst. A',
    'address': 'Somewhere',
    'zip': '121212'
}

inst2 = {
    'name': 'Inst. B',
    'address': 'Somewhere',
    'zip': '131313'
}

inst3 = {
    'name': 'Inst. C',
    'address': 'Somewhere',
    'zip': '141414'
}

author1 = {
    'name': {
        'firstName': 'John',
        'lastName': 'Doe',
        'initials': 'J.',
        'dateOfBirth': 'whocares'
           },
    'type': 'primary',
    'email': 'john.doe@first.author',
    'addresses': [ inst1, inst2, inst3 ]
}

author2 = {
    'name': {
        'firstName': 'Jane',
        'lastName': 'Doe',
        'initials': 'J.',
        'dateOfBirth': 'whocares'
           },
    'type': 'secondary',
    'email': 'jane.doe@second.author',
    'addresses': [ inst2, inst3 ]
}

author3 = {
    'name': {
        'firstName': 'Jack',
        'lastName': 'Doe',
        'initials': 'J.',
        'dateOfBirth': 'whocares'
           },
    'email': 'jack.doe@third.author',
    'addresses': None
}

author4 = {
    'name': {
        'firstName': 'Judy',
        'lastName': 'Doe',
        'initials': 'J.',
        'dateOfBirth': 'whocares'
           },
    'type': 'Primary',
    'email': 'judy.doe@fourth.author',
    'addresses': [ inst1, inst3 ]
}

mypaper['authors'] = [author1,author2,author3,author4]

mypaper_yaml = yaml.safe_dump(mypaper,sort_keys=False)

with open('mypaper.yaml', 'wt') as mypaper_info:
    mypaper_info.write(mypaper_yaml)
