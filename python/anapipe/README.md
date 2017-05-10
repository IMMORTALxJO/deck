## ANAPIPE
realtime log statistics in CLI

[user@host ~]# ./anapipe -h
ANAPIPE help:
 -f   | --flush    : statistics will reset after each display (default FALSE)
 -s N | --sleep N  : update statistics every N seconds (default 1)
 -c N | --count N  : show the first N positions (default 10)
 -h   | --help | ? : print this help

# Example of usage

[user@host ~]# cat /var/log/nginx/access.log | ./anapipe 0 0-6

┌─ ANAPIPE v2 ────────────────────────────────────────────────────────────────────────────┐
├── 0 ────────────────────────────────────────────────────────────────────────────────────┤
│  Keys : 5                                                                               │
│  Reqs : 58                                                                              │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│  54 77.221.128.82                                                                       │
│  1  175.139.123.161                                                                     │
│  1  223.197.208.226                                                                     │
│  1  89.248.168.30                                                                       │
│  1  163.172.99.10                                                                       │
├── 0-6 ──────────────────────────────────────────────────────────────────────────────────┤
│  Keys : 29                                                                              │
│  Reqs : 58                                                                              │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│  19 77.221.128.82-/elasticsearch/ng-*/_field_stats?level=indices                        │
│  8  77.221.128.82-/elasticsearch/_mget                                                  │
│  4  77.221.128.82-/elasticsearch/_msearch                                               │
│  2  77.221.128.82-/                                                                     │
│  1  77.221.128.82-/plugins/kibana/assets/settings.svg                                   │
│  1  77.221.128.82-/elasticsearch/.kibana/index-pattern/_search?stored_fields=           │
│  1  77.221.128.82-/api/console/api_server?sense_version=%40%40SENSE_VERSION&apis=es_5_0 │
│  1  77.221.128.82-/bundles/node_modules/font-awesome/fonts/fontawesome-webfont.woff2    │
│  1  77.221.128.82-/ui/fonts/open_sans/open_sans_v13_latin_700.woff2                     │
│  1  77.221.128.82-/plugins/kibana/assets/visualize.svg                                  │
└─────────────────────────────────────────────────────────────────────────────────────────┘