#!/usr/bin/env bash
python -m pip install pypandoc

python -c "from pypandoc.pandoc_download import download_pandoc;download_pandoc(version='2.5')"

files=`git diff --name-only HEAD~1 HEAD | grep README.md`
files="racket/intro/README.md"
python scripts/publish_ssh.py --files $files
