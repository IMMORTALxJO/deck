#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import flask, os, sys, json, argparse, logging
from flask_restful import Resource, Api
from jinja2 import Template
from collections import OrderedDict
from pathlib import Path

# Parse args
parser = argparse.ArgumentParser()
parser.add_argument('--name', type=str, required=False,
                    default='default_page', help='Name of your template')
parser.add_argument('--port', type=int, required=False,
                    default=8080, help='Port to listen on')
parser.add_argument('--host', type=str, required=False,
                    default='0.0.0.0', help='Host to listen on')

parser.add_argument('--state', type=str, required=False,
                    help='Path to state ( default ~/.templinator/<template_name>/state.json')
parser.add_argument('--template', type=str, required=False,
                    help='Path to template ( default ~/.templinator/<template_name>/template.j2')
parser.add_argument('--page', type=str, required=False,
                    help='Path to rendered page ( default ~/.templinator/<template_name>/index.html')
parser.add_argument('--debug', required=False, action='store_true', help='Enable debug logs')

parser.set_defaults(debug=False)
options = parser.parse_args()

# Generate required vars
user_home_path = Path.home()

### Logs setup
logging.basicConfig(
    level=logging.INFO,
    format="[%(asctime)s] %(levelname)s %(message)s",
    stream=sys.stdout)
log = logging.getLogger('Templinator')
if options.debug: 
  log.setLevel(logging.DEBUG)
else:
  log.setLevel(logging.INFO)

def create_file( full_path ):
  # Create file and parent directory
  file = Path( full_path )
  if file.exists():
    if file.is_file():
      log.info('%s already exists', full_path )
    else:
      raise Exception('%s is not a file' % full_path)
  else:
    parent_dir = Path( file.parents[0] )
    if not parent_dir.exists():
      parent_dir.mkdir( parents=True, exist_ok=True )
      log.info('Direcotry %s created', parent_dir )
    file.touch(exist_ok=True)
    log.info('File %s created', full_path )
  if not os.access( full_path, os.W_OK ):
    raise Exception('%s is not writable' % full_path)

def update_page():
  # Take template+state and generate page
  raw_state = open( options.state ).read()
  raw_template = open( options.template ).read()
  log.debug( 'Template current:\n%s', raw_template )
  if raw_state == '':
    raw_state = '{}'
  json_state = json.loads(raw_state, object_pairs_hook=OrderedDict)
  log.debug( 'State current:\n%s', json_state )
  page_content = Template( raw_template ).render(state=json_state)
  log.debug( 'Generated content:\n%s', page_content )
  with open(options.page, 'w+') as f:
    f.write( page_content )
  log.info( 'Page regenerated' )

# Create state/template and page files
if options.state is None:
  options.state = os.path.join(user_home_path, ".templinator", options.name, 'state.json')
if options.template is None:
  options.template = os.path.join(user_home_path, ".templinator", options.name, 'template.j2')
if options.page is None:
  options.page = os.path.join(user_home_path, ".templinator", options.name, 'index.html')

for file in ( options.state, options.template, options.page ):
  create_file( file )

# Create webserver
app = flask.Flask(__name__)
api = Api(app)

# API for state
class StateEndpoint(Resource):

  def get( self ):
    # print state json
    with open(options.state) as f:
      content = f.read()
      if content == '':
        content = '{}'
      return json.loads(content, object_pairs_hook=OrderedDict)

  def post( self ):
    # rewrite the whole state
    json_input = flask.request.get_json()
    with open(options.state, 'w') as f:
      f.write( json.dumps( OrderedDict(json_input), sort_keys=True, indent=2, separators=(',', ': ')) )
    update_page()
    return json_input

  def put( self ):
    # rewrite the whole state
    json_input = OrderedDict(flask.request.get_json())
    log.debug('State input: %s', str(json_input) )
    content = open(options.state, 'r').read()
    if content == '':
      content = '{}'
    log.debug('State file content: %s', content)
    json_cur = json.loads(content, object_pairs_hook=OrderedDict)
    log.debug('State current: %s', dict(json_cur) )
    updated_json = { **json_cur , **json_input }
    log.debug('Update result: %s', str(updated_json) )
    if updated_json != json_cur:
      with open(options.state, 'w+') as f:   
        f.write( json.dumps(updated_json, sort_keys=True, indent=2, separators=(',', ': ')) )
      update_page()
    return updated_json


api.add_resource(StateEndpoint, '/%s/state' % options.name , endpoint = 'state')

# API for template
class TemplateEndpoint(Resource):

  def get( self ):
    # print template
    with open(options.template) as f:
      return flask.Response( f.read(), mimetype='text/plain' )

  def post( self ):
    # rewrite template
    textplain_input = flask.request.get_data().decode('utf-8')
    with open(options.template, 'w+') as f:
      f.write( textplain_input )
    update_page()
    return flask.Response( textplain_input, mimetype='text/plain' )


api.add_resource(TemplateEndpoint, '/%s/template' % options.name , endpoint = 'template')

# Endpoint for page
class PageEndpoint(Resource):

  def get( self ):
    # print template
    with open(options.page) as f:
      return flask.Response( f.read(), mimetype='text/html' )

api.add_resource(PageEndpoint, '/%s' % options.name , endpoint = 'page')

if __name__ == '__main__':
  update_page()
  log.info( 'State API running on http://%s:%d/%s/state', options.host, options.port, options.name )
  log.info( 'Template API running on http://%s:%d/%s/template', options.host, options.port, options.name )
  log.info( 'Page serving on http://%s:%d/%s', options.host, options.port, options.name )
  app.run( debug=options.debug, host=options.host, port=options.port )