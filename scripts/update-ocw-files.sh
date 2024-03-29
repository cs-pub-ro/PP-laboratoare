#!/usr/bin/env bash
python -m pip install dokuwiki

rm -rf .tmp/
mkdir -p .tmp/
NAMESPACE='pp/23/laboratoare'
src_files=`git diff --name-only HEAD~1 HEAD | xargs dirname | grep -E "schelet|solutii" | uniq`

for file in $src_files
do
  target=$(basename $file)
  lab=$(basename $(dirname $file))
  lang=$(dirname $(dirname $file))
  path=$(pwd)/.tmp/$lab-$lang-$target.zip
  pwd_point=$(pwd)
  cd $file
  zip $path -r .
  cd $pwd_point
  python scripts/upload_file.py --username $USERNAME --password $PASSWORD --file $path --namespace $NAMESPACE/$lang/$lab-$target.zip
done
