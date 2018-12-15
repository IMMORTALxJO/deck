#!/usr/bin/env python
import docker
from flask import Flask, render_template, jsonify
from flask_cors import CORS
import argparse

parser = argparse.ArgumentParser()

parser.add_argument('--port', type=int, required=False,
                    default=8769, help='Port to listen on')
parser.add_argument('--host', type=str, required=False,
                    default='127.0.0.1', help='Host to listen on')
parser.add_argument('-a','--add-host', action='append', help='Add additional host with docker-ui', required=False)
parser.add_argument('--debug', required=False, action='store_true', help='Enable debug logs')

parser.set_defaults(debug=False)
parser.set_defaults(add_host=[])
options = parser.parse_args()

dockerClient = docker.from_env()
app = Flask(__name__, static_url_path='/static', static_folder='./static',)

@app.route('/')
def printLanding():
  return app.send_static_file('index.html')

@app.route('/hosts/list')
def getHosts():
  return jsonify( options.add_host )

@app.route('/containers/list')
def getContainers():
  output = []
  for container in dockerClient.containers.list(all=True):
    computedStatus = container.attrs['State']['Status'].capitalize() 
    if container.attrs['State']['OOMKilled']:
      computedStatus = 'OOMKilled'
    if container.attrs['State']['Paused']:
      computedStatus = 'Paused'
    if container.attrs['State']['Restarting']:
      computedStatus = 'Restarting'
    output.append({
      "id": container.id,
      "name": container.name,
      "computedStatus": computedStatus,
      "shortId": container.short_id,
      "state": container.attrs['State']
    })
  return jsonify( output )

@app.route('/containers/stop/<id>')
def stopContainer(id=None):
  target = dockerClient.containers.list( all=True, filters={"id":id } )
  target[0].stop(timeout=30)
  return getContainers()

@app.route('/containers/start/<id>')
def startContainer(id=None):
  target = dockerClient.containers.list( all=True, filters={"id":id } )
  target[0].start()
  return getContainers()

if __name__ == "__main__":
  app.run( debug=options.debug, host=options.host, port=options.port )