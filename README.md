# hipio

hipio maps `<anything>.<IP Address>.<domain>` to the corresponding `<IP Address>`, e.g. `127.0.0.1.<domain>` maps to `127.0.0.1` 

This is a stand-alone alternative to xip.io and nip.io, no PowerDNS required.

And it has tests.

```
hipio - Wildcard DNS Server for any IP Address

Usage: hipio DOMAIN [PORT] [-a RECORD] [--ns RECORD]
             --soa-email EMAIL
  hipio maps <anything>.<IP Address>.<domain> to the corresponding <IP Address>,
  e.g. 127.0.0.1.<domain> maps to 127.0.0.1

Available options:
  -h,--help                Show this help text
  DOMAIN                   Root wildcard domain.
  PORT                     Listening port. (default: 53)
  -a RECORD                A record for DOMAIN
  --ns RECORD              NS record for DOMAIN
  --soa-email EMAIL        Email address for SOA record. Example:
                           admin.example.com
```

hipio logs DNS request activity to standard output.
