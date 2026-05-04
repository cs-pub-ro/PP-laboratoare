#!/usr/bin/env bash
python -m pip install pypandoc

python -c "from pypandoc.pandoc_download import download_pandoc;download_pandoc(version='2.5')"

readme_files=`git diff --name-only HEAD~1 HEAD | grep README.md`
img_files=`git ls-files | grep img-ocw`
if [[ $readme_files ]]; then
    python scripts/publish_ssh.py --files $readme_files --images $img_files
fi
