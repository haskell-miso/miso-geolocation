:ramen: 📍 miso-geolocation
====================

A [Geolocation API](https://developer.mozilla.org/en-US/docs/Web/API/Geolocation_API) example. See live [here](https://geolocation.haskell-miso.org).

> [!TIP] 
> This requires installing [nix](https://nixos.org) with [Nix Flakes](https://wiki.nixos.org/wiki/Flakes) enabled.
> Although not required, we recommend using [miso's binary cache](https://github.com/dmjio/miso?tab=readme-ov-file#binary-cache).

### Example

```haskell
-----------------------------------------------------------------------------
data Action
  = GetLocation
  | ErrorLocation GeolocationError
  | SetLocation Geolocation
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp app
-----------------------------------------------------------------------------
type Model = Maybe Geolocation
-----------------------------------------------------------------------------
app :: App Model Action
app = (component Nothing updateModel viewModel)
#ifndef WASM
  { styles =
    [ Href "assets/style.css"
    ]
  }
#endif
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel = \case
  GetLocation -> do
    geolocation SetLocation ErrorLocation
  SetLocation location -> do
    this ?= location
  ErrorLocation (GeolocationError _ err) ->
    io_ (consoleError err)
```

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop --experimental-features nix-command --extra-experimental-features flakes
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
