
FROM node:6.13

RUN npm install -g elm

ADD . /root/front

WORKDIR /root/front

RUN elm-package install -y
