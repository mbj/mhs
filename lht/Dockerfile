FROM alpine:3.13

# Setup for userns mapped single user
RUN echo $'build:x:0:0:build:/opt/build:/bin/ash\n\
nobody:x:65534:65534:nobody:/:/sbin/nologin\n\A'\
> /etc/passwd
RUN echo $'build:x:0:build\n\
nobody:x:65534:\n\A'\
> /etc/group

# Setup apk public key
RUN echo $'-----BEGIN PUBLIC KEY-----\n\
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAoWXZ75rvmn8I5GoTmxsA\n\
LGv5eWrNf/lrOeiPAoux3H2nCwO0+Qd/Fd0u453B8yq5pmcuyebYvjSle4vH0C7m\n\
g1x2PN57sZLawJbO47ywKw7yN5iWhLTq8R8Zbou9m7o0fSuukGPKzleOFn2MxLw3\n\
0uqcv45Vcsw2msgwfXluycjyCWfoV+dvCeu1pqVLLaG8FP108Pn2yn7IzIvt5abd\n\
pg+6jh4W8t1P2LZ3D0YI8P3+0+d9tcVQe2TAaNFjALWB6GY0D9vncVI+WjPi2NxE\n\
ZHUir6fe0rAx7hLAhyo9SPSCeOQhO8oBijoIE3C5d+cZebUNNuhPGZwKRPmeKDOj\n\
hQIDAQAB\n\
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
RUN apk add                  \
  --no-cache                 \
  --                         \
  curl                       \
  gcc                        \
  git                        \
  libffi                     \
  make                       \
  musl-dev                   \
  ncurses-dev                \
  ncurses-static             \
  openssl-libs-static        \
  stack@mbj=2.7.3-r0         \
  tar                        \
  xz                         \
  zlib-dev                   \
  zlib-static
