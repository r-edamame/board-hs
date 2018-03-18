
from samdoshi/haskell-stack:latest

WORKDIR /root/app

RUN git clone https://github.com/vishnubob/wait-for-it

RUN mv wait-for-it/wait-for-it.sh /usr/bin && chmod +x /usr/bin/wait-for-it.sh

COPY . .

RUN stack build
