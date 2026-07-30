# Pips solver: web app

A browser front end for the solver in [`Pips`](../Pips). Pick a date and
a difficulty, and it fetches that day's [New York Times
Pips](https://www.nytimes.com/games/pips) puzzle, draws it, and animates
every solution it finds.

Live at <https://brianberns.github.io/Pips/>.

## Design

The whole app is F#, compiled to JavaScript by
[Fable](https://fable.io/). No F# is rewritten in another language along
the way: the solver that runs in the browser is the same code that runs
on .NET.

### Sharing the model

The project simply references `Pips.fsproj`. Fable compiles that
project's source files to JavaScript along with this project's own,
which is why the solver needs no browser-specific version.

Where the model does have to differ in the browser, it says so with a
compiler directive. `Array2DSafe` is the only place so far: a real 2D
array (`'t[,]`) on .NET, and a jagged array (`'t[][]`) under Fable, which
is all JavaScript has. Both expose the same functions, so `Board` is none
the wiser.

### State

State is managed with [Elmish](https://elmish.github.io/elmish/), the F#
take on Model-View-Update. `App.fs` holds the whole thing:

* `Model` — selected date and difficulty, the puzzles fetched for that
  date, any solutions found, and what the board is currently showing.
* `Msg` — every event the app can respond to.
* `update` — the only place the model changes.
* `subscribe` — starts a timer when, and only when, solutions are being
  animated.

Two consequences worth knowing:

* Puzzles for all three difficulties arrive in one response, so changing
  difficulty doesn't hit the network again.
* The search runs on the UI thread and blocks it. `solvePuzzle` yields a
  frame before starting, which lets the browser paint the "Solving…"
  state first.

### Views

Views are written with [Feliz](https://fable-hub.github.io/Feliz/), which
produces React elements from F#. There's no canvas: the board is real
HTML, styled by `index.css`.

* `Region.fs` renders each cell as a CSS grid item, and the lines around
  it as separate elements centered on the grid. A cell draws its own
  left and top lines, so the line between two cells is drawn exactly
  once; cells with no neighbor to the right or below draw those edges
  themselves. Each line overhangs its ends by half its width, so lines
  meeting at a corner overlap rather than leaving a notch — which is why
  they aren't just CSS borders on the cells. An unconstrained region is
  left gray; every other region is filled with a hue. A region's
  constraint, if it has one, sits in a diamond badge on the lattice
  point at its first cell, filled with that region's own color — color,
  not position, is what says whose badge it is.
* `Domino.fs` renders a domino as two halves, each a 3x3 grid holding up
  to six pips. Dominoes are always laid out horizontally, then rotated a
  quarter turn at a time into place, so a vertical domino is a rotated
  horizontal one rather than a separate layout. A placed domino is a
  fixed ivory, not derived from the theme, and translucent, so the
  region underneath still reads once it's covered.
* `Puzzle.fs` assembles the board — including choosing each region's
  hue by greedy graph coloring, so that no two regions meeting at an
  edge or even just a corner are colored alike — and lays a solution's
  dominoes over it.
* `Program.fs` renders the controls and mounts the app.

Sizes all derive from one custom property, `--cell-size`, so the puzzle
scales with the viewport, and a domino is the same markup whether it sits
on the board or in the tray of unplaced dominoes. Colors come from custom
properties too, which is all dark mode needs.

### Fetching puzzles

The New York Times endpoint doesn't allow cross-origin requests, so the
app calls [`Pips.Server`](../Pips.Server) — an Azure function that
fetches `https://www.nytimes.com/svc/pips/v1/{date}.json` and passes it
back. `Daily.convert` turns the response into `Puzzle` values.

## Development

Fable compiles each `.fs` file to a `.fs.js` file beside it, and
[Vite](https://vite.dev/) serves those. Run both in watch mode:

```
dotnet fable watch --configuration Release --sourceMaps --run npx vite
```

Then open the URL Vite prints (<http://localhost:5173/> by default).
Editing an F# file recompiles it and reloads the browser.

Note the `--configuration Release`. The model asserts its invariants
freely — `Puzzle.place` revalidates the entire puzzle every time a
domino goes down — and those asserts are compiled out of a Release
build. The source maps keep the F# readable in the browser's debugger
either way.

## Publishing

GitHub Pages serves this site from the `docs` folder on `main`, so a
production build writes directly there:

```
dotnet fable --run npx vite build
```

That produces `docs/index.html`, `docs/index.js`, and `docs/index.css`.
The file names are fixed rather than content-hashed, so each publish
shows up as a diff of three files instead of a rename. Commit the result
and push; Pages picks it up within a minute or so.

To confirm what's live:

```
gh api repos/brianberns/Pips/pages/builds/latest --jq '{status,commit}'
```
