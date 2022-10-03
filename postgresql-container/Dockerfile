FROM registry.hub.docker.com/library/alpine:3.16

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
@mbj https://mbj-apk.s3.dualstack.us-east-1.amazonaws.com\n'\
>> /etc/apk/repositories

RUN echo "DBT container init" \
  && export PG_HOME=/var/lib/postgresql \
  && export PG_DATA="$PG_HOME/data" \
  && export MASTER_PASSWORD_FILE="$PG_HOME/master-password.txt" \
  && apk add --no-cache -- openssl postgresql13@mbj postgresql13-contrib@mbj \
  && apk add --no-cache --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing -- daemontools \
  && setuidgid postgres openssl rand -base64 -out "$MASTER_PASSWORD_FILE" 32 \
  && setuidgid postgres initdb --auth=password --data-checksums --encoding=UTF8 --no-locale --pwfile "$MASTER_PASSWORD_FILE" -D "$PG_DATA" \
  && echo "host all all 0.0.0.0/0 password" | install -o postgres -g postgres /dev/stdin "$PG_DATA/pg_hba.conf"
