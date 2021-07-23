FROM alpine:3.12

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

RUN echo $'\
@edge http://dl-cdn.alpinelinux.org/alpine/edge/community\n\
@edge http://dl-cdn.alpinelinux.org/alpine/edge/main\n\
@mbj https://mbj-apk.s3.dualstack.us-east-1.amazonaws.com\n'\
>> /etc/apk/repositories

# Setup build directory
RUN mkdir -p -m 0700 /opt/build

# Install dependencies
RUN apk add                  \
  --no-cache                 \
  --                         \
  curl                       \
  ghc@edge=8.8.3-r0          \
  git                        \
  libpq@mbj=12.2-r0          \
  libffi@edge                \
  llvm10@edge                \
  make                       \
  musl-dev                   \
  ncurses-dev                \
  ncurses-static             \
  postgresql-dev@mbj=12.2-r0 \
  openssl-libs-static        \
  stack@mbj=2.3.1-r0         \
  tar                        \
  xz                         \
  zlib-dev                   \
  zlib-static
