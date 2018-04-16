#!/bin/bash

Dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $Dir

echo "Enter commit message"
read Message

if [ -z "$Message" ]
  then
    echo "aborting due to missing commit message"
    exit 1
fi

git add -A && git commit -m "$Message"

echo "Push? (y/n)"
read Push
case "$Push" in ([yY]) git push ;; esac

$SHELL