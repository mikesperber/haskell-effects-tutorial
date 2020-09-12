# Tutorial "Control Your Effects in Haskell"

... at [BOB 2020](https://bobkonf.de/2020/sperber.html) and
[MuniHac 2020](https://munihac.de/2020.html#MichaelSperber).

To prepare for the tutorial, you have two choices:

## If you're proficient with Haskell and have Haskell and cabal installed

```
cabal install --dependencies-only
```

You probably will need ghc 8.8 - Polysemy doesn't work with 8.10.

## If you're not

- install [Docker](https://www.docker.com/)
- on Docker for Mac, give it >=6GB via Preferences -> Resources -> Memory
- install [Visual Studio Code](https://code.visualstudio.com/download)
- install the "ghcide" extension: click on the
  "Extensions" icon on the left, search for "ghcide", select
  "ghcide", click on install
- install the "Remote - Containers" extension: click on the
  "Extensions" icon on the left, search for "Containers", select
  "Remote - Containers", click  on install
- click on the files icon in the upper-left corner
- click on the "View" item, select "Command Palette", type
  "containers", select "Remote - Containers: Open Folder in Container"
- select the `prep` or the `live` folder
- it should say that it's building a docker image; this can take a
  while (possibly an hour or more, 20mins on my machine)

## If you run into problems

... contact [Mike Sperber](https://www.deinprogramm.de/sperber/).






