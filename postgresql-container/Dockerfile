FROM postgres:13.4-alpine

RUN echo "DBT container init" \
  && export PG_HOME=/var/lib/postgresql \
  && export PG_DATA="$PG_HOME/data" \
  && export MASTER_PASSWORD_FILE="$PG_HOME/master-password.txt" \
  && apk add --no-cache -- openssl postgresql13-contrib \
  && apk add --no-cache --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing -- daemontools \
  && setuidgid postgres openssl rand -base64 -out "$MASTER_PASSWORD_FILE" 32 \
  && setuidgid postgres initdb --auth=password --data-checksums --encoding=UTF8 --no-locale --pwfile "$MASTER_PASSWORD_FILE" -D "$PG_DATA" \
  && echo "host all all 0.0.0.0/0 password" | install -o postgres -g postgres /dev/stdin "$PG_DATA/pg_hba.conf"
