#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import json
import argparse
from jinja2 import Template
from collections import OrderedDict

# Parse args

parser = argparse.ArgumentParser()
parser.add_argument('--key', '-k', type=str, required=False,
                    help='A key of dict which will be updated')
parser.add_argument('--state-file', '-sf', type=str, required=True,
                    help='File where state saves')
parser.add_argument('--template', '-t', type=str, required=False,
                    help='Jinja2 template to render from')
parser.add_argument('--delete', '-d', dest='delete', action='store_true',
                    required=False, default=False,
                    help='Delete values or full key from statefile')

(args, another_args) = parser.parse_known_args()

# Parse inserted fields

update_data = {}
data_key = False
for arg in another_args:
    if data_key:
        update_data[data_key] = arg
        data_key = False
    elif arg[:2] == '--':
        data_key = arg[2:]
        update_data[data_key] = None

# Read old state, update it and write

try:
    state_file_fd = open(args.state_file, 'r')
    state = json.loads(state_file_fd.read(),
                       object_pairs_hook=OrderedDict)
except:
    state = {}
if args.key:

    if args.delete and args.key in state:
        if len(update_data.items()) > 0:
            for sub_key in update_data:
                state[args.key].pop(sub_key)
        else:
            state.pop(args.key)

    if not args.delete:
        state[args.key] = update_data

    with open(args.state_file, 'w+') as state_file_fd:
        state_file_fd.write(json.dumps(state, sort_keys=True, indent=2,
                            separators=(',', ': ')))

# Commit state to template and print render

if args.template:
    with open(args.template, 'r') as template_file_fd:
        print Template(template_file_fd.read()).render(state=state)

      