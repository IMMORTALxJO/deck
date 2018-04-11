#!/usr/bin/python
import os
import json
import argparse
from jinja2 import Template
from collections import OrderedDict

# Parse args
parser = argparse.ArgumentParser()
parser.add_argument('--state-file','-sf', type=str, required=True, help='Path to state file')
parser.add_argument('--key','-k', type=str, required=False, help='Name of key to update')
parser.add_argument('--template','-t', type=str, required=False, help='Jinja2 template file')
args, another_args = parser.parse_known_args()

# Parse inserted fields
update_data = {}
data_key = False
for arg in another_args:
  if data_key:
    update_data[ data_key ] = arg
    data_key = False
  elif arg[:2] == '--':
    data_key = arg[2:]

# Read old state, update it and write
try:
  state_file_fd = open( args.state_file, 'r' )
  state = json.loads( state_file_fd.read(), object_pairs_hook=OrderedDict )
except:
  state={}
if args.key:
  state[ args.key ] = update_data
  with open( args.state_file, 'w+' ) as state_file_fd:
    state_file_fd.write( json.dumps( state, sort_keys=True, indent=2, separators=(',', ': ') ) )

# Commit state to template and print render
if args.template:
  with open( args.template, 'r' ) as template_file_fd:
    print( Template( template_file_fd.read() ).render( state=state ) )
