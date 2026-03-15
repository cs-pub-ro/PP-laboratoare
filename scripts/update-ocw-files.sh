#!/usr/bin/env bash

REMOTE_BASE='/home/pp/pp-pages/26/laboratoare'
REMOTE_HOST='pp@ocw.cs.pub.ro'
SSH_OPTS='-o PubkeyAcceptedKeyTypes=+ssh-rsa -o HostKeyAlgorithms=+ssh-rsa -o StrictHostKeyChecking=no'

rm -rf .tmp/
mkdir -p .tmp/
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

  remote_dir="$REMOTE_BASE/$lang"
  ssh $SSH_OPTS $REMOTE_HOST "mkdir -p $remote_dir"
  scp $SSH_OPTS $path "$REMOTE_HOST:$remote_dir/$lab-$target.zip"
done
