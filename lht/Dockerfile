FROM registry.hub.docker.com/library/alpine:3.18

# Setup for userns mapped single user
RUN echo $'build:x:0:0:build:/opt/build:/bin/ash\n\
nobody:x:65534:65534:nobody:/:/sbin/nologin\n\A'\
> /etc/passwd
RUN echo $'build:x:0:build\n\
nobody:x:65534:\n\A'\
> /etc/group

# Setup build directory
RUN mkdir -p -m 0700 /opt/build

# Install dependencies
RUN apk add           \
  --no-cache          \
  --                  \
  curl                \
  gcc                 \
  g++                 \
  git                 \
  gmp-dev             \
  grep                \
  libffi              \
  make                \
  musl-dev            \
  ncurses-dev         \
  ncurses-static      \
  openssl-libs-static \
  postgresql-dev      \
  tar                 \
  xz                  \
  zlib-dev            \
  zlib-static

# Download stack
RUN curl                                                                                                          \
    --location                                                                                                    \
    --output /usr/local/bin/stack                                                                                 \
    https://github.com/commercialhaskell/stack/releases/download/v2.11.1/stack-2.11.1-linux-x86_64-bin            \
  && echo '5c7a9f86c126d4af15d03ddc03f95f3ceafedd3b9d3dcc7818f2ea60ed6a0ef9  /usr/local/bin/stack' | sha256sum -c \
  && chmod +x /usr/local/bin/stack
