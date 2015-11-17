# Shakespeare's Tries

Generates an autocomplete Trie from Shakespeare's full corpus.

Written for November 2015's [West London Hack Night](http://www.meetup.com/West-London-Hack-Night/).

## Building & Running

You'll need [stack](https://github.com/commercialhaskell/stack). Then call

``` sh
stack build
stack exec shakespeare-trie
```

You can enter queries and it will show the first 20 lines that begin with that string.

## Run test

``` sh
stack tests
```
