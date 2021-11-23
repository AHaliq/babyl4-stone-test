# reflex-stone

Frontend client only web app for baby-l4

**!WARNING!**
Make sure you clone recursively as this repo contains a submodule

Demo: https://ahaliq.github.io/babyl4-stone-test/

## User Guide

Refer to [Obelisk](https://github.com/obsidiansystems/obelisk) docs for nix setup.

Running locally using GHC and jsaddle-warp:

```bash
make dev
```

Sometimes after killing ghcid, it may still run in the background and cause memory leaks.
Find the process via

```
make pids
```

and kill it via

```bash
kill <pid>
```

The site can be generated to `./result` via

```bash
make build
```

You can simulate deployment via

```bash
make server
```

## Developer Guide

### Project Structure

The project is structured as follows
```
- baby-l4
- frontend
- result
- static
- ...
```

- `./baby-l4` is the actual compiler repo
- `./frontend` is the actual haskell project for the web app
- `./result` all the files compiled and necessary for deployment
- `./static` files that will be copied relative to the resulting html file

### Reflex FRP Guide

Similar to how React has components, reflex has widgets.

Reflex has monads `Behaviour`, `Event` and `Dynamic` which acts as streams of events.

These streams are declaratively constructued and passed to other widgets that would like to depend on them.

We can declaratively also define the widgets based on these streams and construct new streams from user interaction with widgets.

For an example checkout `./frontend/src/Widgets/TabbedWindow.hs` at line `37` theres a helper function that constructs a button html element. Look at its return type, it returns an `Event` monad.

This helper is called in the `widget` function, which is called in other modules' `widget` functions compositionally up to the `main` function that renders the page given a root widget.

Explore the code base and refer to the [hackage](https://hackage.haskell.org/package/reflex) docs for more information on their functions to have a better understanding of how html elements are created.

### Managing CSS

There is a helper module `./frontend/src/Utils/Style.hs` that define functions that can be called as arguments of one another to build a string of a css file. We can then define a stylesheet as in `./frontend/src/Widgets/Page/TwoWindow/Style.hs`.

This is due to legacy issues where we did not have an easy method to include static assets such as css files to the project.

Now however it is possible to write a css file as per normal, add it to the `./static` directory and simply create a `style` element with an `src` attribute.

### D3.js

The data types that the AST for babyl4 is composed of all have an instance of `D3Json` where they can be rendered as a json string that `d3.js` can interpret.

The actual implementation and functionality of D3 is placed in a javascript file in `./static` rather than making an interface from haskell for simplicity and easy prototyping. This could potentially be moved into haskel land in the future.

### Colorscheme

Currently we have a js script that loads colourschemes light and dark and will load their values as css variables for the whole project to use.

### Further work

- Integration with [ide](https://github.com/smucclaw/ide/blob/base/syntaxes/l4.tmLanguage.json) repo for syntax highlighting
- Dark / Light mode toggle button
- Compile button for ASTTab rather than re compile on every keypress / dynamic update
- Z3 integration with `Z3.wasm` and a refactor of `SimpleSMT` calls in `baby-l4`