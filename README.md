# Logik (Mastermind) game

Simple game in haskell with gloss library.

## Run

Run with `cabal`:
```
cabal run logik -- [game type]
```
`game type` can be:
```
-g      guesser (player is guessing the password, computer is answering)
-a      answerer (computer is guessing the password, player is answering)
-gui    guesser with GUI 
```