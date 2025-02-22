#!/usr/bin/env bash

topics=("racket" "haskell" "prolog")
archive_types=("solutii" "schelet")

git checkout archives25 || git checkout -b archives25

git rebase master

mkdir -p ../archives

for str in ${topics[@]}; do
    echo $str
    for entry in ../"$str"/*
    do
        echo "$entry"
        arrIN=(${entry//// })
        lab=${arrIN[2]}
        echo "$lab" 

        for type in ${archive_types[@]}; do
            ls "$entry/$type"
            zip ../archives/"$str-$lab-$type.zip" "$entry/$type"/*
        done
    done
done

git config --global user.name "PP Bot"
git config --global user.email "mail@bigpp.com"
git status
git add -f ../archives
git status
git commit -m "Updated archives"
git push origin HEAD:archives25 --force
