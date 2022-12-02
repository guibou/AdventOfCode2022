My advent of code for 2021

Each days are in `src/DayN.hs`.

`src/DayX.hs` is the template to start each day, you can generate a new file using `src/startDay X`.

Starts stack with `nix-shell --run 'cabal new-repl'` to start a session and `:l YourDay`. You can also use `direnv` and everything will be setup automagically.

`Utils` contains a shitload of utils ;)

# Benchmarks

run `./bench.sh` to generate a benchmark of all tests.

the image `bench.png` here is the timing of all days:

~[Benchmark results](bench.png)

I'm targeting to run all problems in less than 1s.
