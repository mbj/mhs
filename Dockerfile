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
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvCahdd8oZywDQbvSaukg\n\
7HvfgAGW6kRk0n6Ae1f+e5dIzbFBW8AQTULubf6brfEoJLSgHijnDEY56nJsDJAK\n\
Nq4NTeYHgOQxZ1K3I55Fk/53vstyJPUTzy/BhX/2lNZohpaGGuM+20baPJ3hf6GA\n\
Wa5JGagrvbIQdivQC1AnAiOallJ7nixCjItJZuY8r3I3gqoIP3IPyRJipTlQ6seo\n\
wQm7DWpTeRiLk/kbx0XutIIGs/9/xkmwaNd266eMD6N5M01Z/SkmC6QAnnUlEoez\n\
YpC9sLubFQFbUqJ5SJ9BYQ5xkEl5JHa7yScyimsK02xHRQY+yeo5FK0jtvPoLekX\n\
AwIDAQAB\n\
-----END PUBLIC KEY-----\n\A'\
>> /etc/apk/keys/mrh-apk.rsa.pub

RUN echo https://mrh-state-alpinepackagesbucket-eu8bh5k00ruo.s3.dualstack.us-east-1.amazonaws.com >> /etc/apk/repositories

# Install dependencies
RUN apk add                       \
  --no-cache                      \
  --                              \
  cache-s3=0.1.5-r0               \
  curl=7.67.0-r0                  \
  ghc=8.6.5-r0                    \
  git=2.24.1-r0                   \
  make=4.2.1-r2                   \
  musl-dev=1.1.24-r0              \
  ncurses-dev=6.1_p20191130-r0    \
  ncurses-static=6.1_p20191130-r0 \
  stack=1.9.3-r0                  \
  xz=5.2.4-r0                     \
  zlib-dev=1.2.11-r3              \
  zlib-static=1.2.11-r3


# Setup build directory
RUN mkdir -p -m 0700 /opt/build
