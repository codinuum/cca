#!/bin/sh
rev=$(git log --format=format:%h HEAD^! || echo unknown)
echo "let version=\"revision $rev\"" > version.ml
