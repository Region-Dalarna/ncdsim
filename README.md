# ncdsim

Detta repository innehåller en Shinyapplikation (`ncdsim`) för Samhällsanalys, Region Dalarna.

Appkoden under `app/` är importerad från ett befintligt repo via `git subtree`:

- **Källa:** https://github.com/FohmAnalys/ncdsim.git
- **Branch vid import:** main

## Struktur

- All appkod ligger i katalogen `app/` (importerad via git subtree)
- `_publicering_till_server.yml` i root styr vilken Shiny-server som är default för `shinyapp_publicera()`

- Deployment sker via GitHub Actions:
  - `.github/workflows/deploy.yml` – publicerar vid push till `publicera-publik` eller `publicera-intern`
  - `.github/workflows/avpublicera.yml` – tar bort appen från vald server (manuell trigger)

  Appmapp på servern: `/srv/shiny-server/ncdsim`.

## Hämta uppdateringar från källrepot

Från R (kan köras var du än står):

```r
system2("git", c("-C", "c:/gh/ncdsim",
                 "subtree", "pull", "--prefix=app",
                 "https://github.com/FohmAnalys/ncdsim.git", "main", "--squash"))
```

Från terminalen (måste köras från repots mapp):

```
cd c:/gh/ncdsim
git subtree pull --prefix=app https://github.com/FohmAnalys/ncdsim.git main --squash
```

