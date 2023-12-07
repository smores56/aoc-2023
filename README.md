Advent of Code 2023
===================

My [Advent of Code][aoc] 2023 solutions, written in [Roc][roc].

## Setup

Since Roc is a young language, it's not available in most package managers. Hence I'm using [direnv][direnv] along with [Nix][nix] to automatically set up a shell with `roc` installed (and `roc_ls`, the LSP implementation) whenever I `cd` to this repo. Once you install [direnv][direnv] and [Nix][nix], you can just run `direnv allow` and then wait a bit for `Roc` to compile from scratch.

### Loading Inputs

Inputs are automatically loaded from the Advent of Code website and cached in the `.input/` folder. For you to load your specific inputs, you need to be logged in. You can follow [these instructions][get-aoc-session] to learn how to find your session cookie, at which point you can copy it without quotes to a file named `.session`.

### New Days

Solutions for each day live in the `days` folder, each called `DayX.roc`. You can copy this template to kickstart a day's solutions off:

```elm
interface DayXX
    exposes [part1, part2]
    imports []

part1 = \lines ->
    "TODO"

part2 = \lines ->
    "TODO"
```

There are 3 other places where you'll need to add code for hooking up the new day's solutions:
- Add `DayXX` to the `exposes` clause of the `days/main.roc` package declaration
- Add a `days.DayXX` import to the top of `main.roc`
- Add a `XX -> Ok (DayXX.part1, DayXX.part2)` case to the `solutionsForDay` function

## Running

Assuming you have [Roc][roc] installed, just run `roc main.roc <DAY>` with `DAY` being the solutions you want to run.

### Example

```
~/dev/aoc-2023
❯ roc main.roc 1
Advent of Code 2023 solutions for day 1:
Result for part 1 in 4ms: ...
Result for part 2 in 407ms: ...
```


[aoc]: https://adventofcode.com/
[roc]: https://www.roc-lang.org/
[direnv]: https://direnv.net
[nix]: https://nixos.org/
[get-aoc-session]: https://github.com/wimglenn/advent-of-code-wim/issues/1
