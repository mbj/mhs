FROM alpine:3.11

# Setup for userns mapped single user
RUN echo $'build:x:0:0:build:/opt/build:/bin/ash\n\
nobody:x:65534:65534:nobody:/:/sbin/nologin\n\A'\
> /etc/passwd
RUN echo $'build:x:0:build\n\
nobody:x:65534:\n\A'\
> /etc/group

# Setup apk public key
RUN echo $'-----BEGIN PUBLIC KEY-----\n\
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtZRLJDokvEpadk3M1KqW\n\
hJ3sMVJzmP1XNMKsG/PxnfWaYGpGzPlkAgSKbHmbn+McWL6/B2GwhwqO4YCZ02rV\n\
P9BBrzlnTak6OFHaxj9nOB0YV0uXMJWW5foNsmmNhPCDzbLDP/F7HmRcuBiosucb\n\
Xiw1JxuRF99tQeksoMxn4jaqIRLpZr2u2QHGU3SAw9FkL9uHtF3h3GE13sgjWXYO\n\
w+ST3GtURxI6RdL/2L09ShCxt2NvwBNvevNxoZOaCMgu/7c+DnIw7q4yII083XjZ\n\
RKgPgxSylguY+X3uuPaV9ZIX8hCuAuFF1fzbTvl/plyeptB9HF6vtXe4CbsZvdYU\n\
9QIDAQAB\n\
-----END PUBLIC KEY-----\n'\
>> /etc/apk/keys/mbj@schirp-dso.com-5e5c5d2b.rsa.pub

RUN echo '@mbj https://mbj-apk.s3.dualstack.us-east-1.amazonaws.com' >> /etc/apk/repositories

# Install dependencies
RUN apk add                  \
  --no-cache                 \
  --                         \
  cache-s3@mbj=0.1.5-r0      \
  curl                       \
  ghc=8.6.5-r3               \
  git                        \
  libpq@mbj=12.2-r0          \
  make                       \
  musl-dev                   \
  ncurses-dev                \
  ncurses-static             \
  postgresql-dev@mbj=12.2-r0 \
  openssl-libs-static        \
  stack@mbj=2.1.3-r0         \
  tar                        \
  xz                         \
  zlib-dev                   \
  zlib-static

# Setup build directory
RUN mkdir -p -m 0700 /opt/build
