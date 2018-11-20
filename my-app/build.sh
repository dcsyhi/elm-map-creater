# !/bin/bash
cd src/
elm-format --yes --elm-version=0.19 Main.elm
cd ..
elm-app build


