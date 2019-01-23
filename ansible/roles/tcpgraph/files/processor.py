#!/usr/bin/env python
import sys
import json
import argparse
import ipaddress
import urllib2
import json

parser = argparse.ArgumentParser()
parser.add_argument('--dump', action='append', required=True)
parser.add_argument('--templinator-render-id', required=True)
parser.add_argument('--nodename', action='append', required=False)
parser.set_defaults(dump=[])
parser.set_defaults(nodename=[])
options = parser.parse_args()

nodes = {}
edges = {}
predefined_nodenames = []
output = {
    "nodes": [],
    "edges": []
}

output_file = sys.argv[1]
dump_files = options.dump
for host in options.nodename:
  nodename, network = host.split(':')
  predefined_nodenames.append({
      "nodename": nodename,
      "network": ipaddress.ip_network(unicode(network))
  })


predefined_nodenames.sort(key=lambda x: len(list(x["network"].hosts())))

for file in dump_files:
  with open(file, 'r') as f:
    for line in f:
      line = line.rstrip()
      if line == '':
        continue
      src, dest = line.split(' ')
      src = str.join('.', src.split(':')[0].split('.')[:4])
      dest = str.join('.', dest.split(':')[0].split('.')[:4])
      if src in ('255.255.255.255', '127.0.0.1') or dest in ('255.255.255.255', '127.0.0.1'):
        continue
      src_ip = ipaddress.ip_address(unicode(src))
      dest_ip = ipaddress.ip_address(unicode(dest))
      for host in predefined_nodenames:
        if src_ip and src_ip in host['network']:
          src = host['nodename']
          src_ip = False
        if dest_ip and dest_ip in host['network']:
          dest = host['nodename']
          dest_ip = False
      if not src in nodes:
        nodes[src] = 1
      else:
        nodes[src] += 1
      if not dest in nodes:
        nodes[dest] = 1
      else:
        nodes[dest] += 1
      edge = (src, dest)
      if dest > src:
        edge = (dest, src)
      if not edge in edges:
        edges[edge] = 1
      else:
        edges[edge] += 1

for edge in edges:
  src, dest = edge
  output["edges"].append({
      "src": src,
      "dest": dest,
      "weight": edges[edge]
  })

output["edges"].sort(key=lambda x: x["weight"])

for node in nodes:
  output["nodes"].append({
      "name": node,
      "weight": nodes[node]
  })

output["nodes"].sort(key=lambda x: x["weight"])

if options.templinator_render_id != 'no-render-id':
  req = urllib2.Request(
      'https://staging.templinator.com/states/%s' % options.templinator_render_id)
  req.add_header('Content-Type', 'application/json')
  urllib2.urlopen(req, json.dumps(output))

print(json.dumps(output))
