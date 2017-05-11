## ANAPIPE
realtime log statistics in linux CLI
```
[user@host ~]# ./anapipe -h
 -f   | --flush       : statistics will reset after each display (default FALSE)
 -s N | --sleep N     : update statistics every N seconds (default 1)
 -i N | --interval N  : show the first N positions (default 10)
 -h   | --help | ?    : print this help
 -c   | --colour      : set min counter valuer for colourfull keys
```
### Usage example
```
[user@host ~]# tail -f /var/log/nginx/access.log | ./anapipe 0 0-6
┌─ ANAPIPE v2 ────────────────────────────────────────────────────┐
├── 0 ────────────────────────────────────────────────────────────┤
│  Keys : 4                                                       │
│  Reqs : 10                                                      │
├─────────────────────────────────────────────────────────────────┤
│  7 8.221.88.82                                                  │
│  1 175.139.123.161                                              │
│  1 89.248.168.30                                                │
│  1 163.172.99.10                                                │
├── 0-6 ──────────────────────────────────────────────────────────┤
│  Keys : 5                                                       │
│  Reqs : 10                                                      │
├─────────────────────────────────────────────────────────────────┤
│  6 8.221.88.82-/elasticsearch/*/_field_stats?level=indices      │
│  1 175.139.123.161-/                                            │
│  1 163.172.99.10-/recordings/index.php                          │
│  1 8.221.88.82-/elasticsearch/_msearch                          │
│  1 89.248.168.30-/                                              │
└─────────────────────────────────────────────────────────────────┘
```
