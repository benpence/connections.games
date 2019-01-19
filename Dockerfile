FROM haskell:8

RUN apt-get update
RUN apt-get install -y apt-transport-https
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install -y nodejs

WORKDIR /root
ADD ./ /root/

RUN npm install
RUN echo '{ "allow_root": true }' > /root/.bowerrc
RUN PATH="$PATH:./node_modules/.bin/" bower install
RUN PATH="$PATH:./node_modules/.bin/" pulp browserify --src-path src/main/purescript/ --optimise --to src/main/resources/static/index.js

RUN stack setup
RUN stack build 

ENTRYPOINT ["stack", "exec", "connections-main"]
