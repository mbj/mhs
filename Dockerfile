FROM alpine:3.10

ENV PGDATA=/var/lib/postgresql/data

RUN apk      \
  add        \
  --no-cache \
  --         \
  openssl    \
  postgresql

WORKDIR /var/lib/postgresql

USER postgres

RUN initdb         \
  --data-checksums \
  --encoding=UTF-8 \
  --no-locale      \
  --no-sync        \
  && echo 'host all all 0.0.0.0/0 trust' > data/pg_hba.conf
