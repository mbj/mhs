FROM alpine:3.16

# Setup for userns mapped single user
RUN echo $'build:x:0:0:build:/opt/build:/bin/ash\n\
nobody:x:65534:65534:nobody:/:/sbin/nologin\n\A'\
> /etc/passwd
RUN echo $'build:x:0:build\n\
nobody:x:65534:\n\A'\
> /etc/group

# Setup apk public key
RUN echo $'-----BEGIN PUBLIC KEY-----\n\
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAmDBxtI4tBKeyOg/jVIRF\n\
I82kpx10xhnxty/Rv/9AoMH231twNowKzJkZeZ9dgri7z7k7Ub1mKmYAdZKSC1Vu\n\
fnuLjbPiIZfZdH+PL2pInV5Yr55QXyoTd+9vC5+L7fI1OrgFVmiRWxKl2nDAUGNl\n\
xQPmHpStAKi+Np9eWgc2xTiImLgnPM7Ofic+4+jJladIA2MMJU7nFYbBY+AypNEa\n\
jEEk+gX7qsv/aP7trT98uuik/xfKK8E2IP4uIisTsPAlbhd/bEob4NkSUrHBCLzX\n\
PnmDDWaQUSHdbuW2x56d76jR9AOPznAi6YgOmhhWYxtn6SiQcOxbD3uIAGF5wQtO\n\
9QIDAQAB\n\
-----END PUBLIC KEY-----'\
>> /etc/apk/keys/mbj.rsa.pub

RUN echo $'\
@edge http://dl-cdn.alpinelinux.org/alpine/edge/community\n\
@edge http://dl-cdn.alpinelinux.org/alpine/edge/main\n\
@mbj https://mbj-apk.s3.dualstack.us-east-1.amazonaws.com\n'\
>> /etc/apk/repositories

# Setup build directory
RUN mkdir -p -m 0700 /opt/build

# Install dependencies
RUN apk add           \
  --no-cache          \
  --                  \
  curl                \
  gcc                 \
  git                 \
  grep                \
  libffi              \
  gmp-dev             \
  make                \
  musl-dev            \
  ncurses-dev         \
  ncurses-static      \
  stack@mbj=2.7.5-r0  \
  tar                 \
  xz                  \
  zlib-dev            \
  zlib-static
