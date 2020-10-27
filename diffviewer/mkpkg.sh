#!/bin/sh
./node_modules/.bin/electron-packager . DiffViewer --platform=darwin --arch=x64 --icon=icon.icns $*
