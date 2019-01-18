# About

An online version of the <a href="https://en.wikipedia.org/wiki/Codenames_(board_game)">Codenames game</a>. Largely based on the experience playing https://www.horsepaste.com/.

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
