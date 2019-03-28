
FROM node:6.13

RUN npm install -g elm@0.18.0

ADD . /root/front

WORKDIR /root/front

RUN elm-package install -y
