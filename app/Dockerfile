
FROM ruby:2.2

ADD . /root/app

WORKDIR /root/app

RUN bundle install -j 4

RUN git clone https://github.com/vishnubob/wait-for-it
RUN mv wait-for-it/wait-for-it.sh /usr/bin && chmod +x /usr/bin/wait-for-it.sh
