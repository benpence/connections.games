# About

An online version of the [Codenames game](https://en.wikipedia.org/wiki/Codenames_(board_game\)). Largely based on the experience playing https://www.horsepaste.com/.

# Install 

    # Client
    $ npm install
    $ PATH="$PATH:./node_modules/.bin/" bower install

    # Server
    $ stack install

# Build

    # Client
    $ PATH="$PATH:./node_modules/.bin/" pulp browserify \
        --src-path src/main/purescript/ \
        --optimise \
        --to src/main/resources/static/index.js

    # Server
    $ stack build

# Run

    $ stack exec connections-main
    # Browse to http://localhost:3000/
