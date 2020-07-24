#!/bin/bash
WDIR=$(pwd)
if [ -z "$1" ]
  then
    echo "Expected version number."
    exit
fi
VER=$1
ORIG_BRANCH=$(git symbolic-ref --short HEAD)
echo Creating release ${VER}
echo Running in ${WDIR} \(${ORIG_BRANCH}\)
if [ -n "$(git status --porcelain)" ]; then
  echo "There are uncommited changes!";
  git status
  exit
fi
echo Update current branch \(${ORIG_BRANCH}\) remote
git push origin ${ORIG_BRANCH} 
BRANCH="release/${VER}"
echo Creating release branch ${BRANCH}
git branch ${BRANCH}
git checkout ${BRANCH} 
git push origin ${BRANCH}
git checkout ${ORIG_BRANCH}
echo done
# Cleanup
cd ${WDIR}
