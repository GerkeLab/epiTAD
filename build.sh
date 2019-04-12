#!/bin/bash -e

if [[ "$1" == "" ]]; then
  echo "Please enter version as first argument without 'v' (e.g. 0.1.0)"
  echo "Optional: second argument is the name of the remote, e.g. origin or upstream."
  echo "Optional: third argument is the commit message to include when bumping the version, default: Version <version>"
  exit 1
else
  VERSION=$1
fi

REMOTE=${2:-"origin"}
MESSAGE=${3:-"Version $VERSION"}

# Check that files are committed
if [[ $(git status --short | egrep "^.M") != "" ]]; then
  echo "Please stage, stash, or commit changes to tracked files."
  exit 1
fi

set -x

# Update version file, add to commit
echo "version: $VERSION" > VERSION
git add VERSION
git commit -m "Version $VERSION"
git tag -a "v$VERSION" -m "$MESSAGE"
git push $REMOTE
git push --tags $REMOTE

# Build and push Docker image
docker build --tag gerkelab/epitad:$VERSION --tag gerkelab/epitad:latest .
docker push gerkelab/epitad:$VERSION
docker push gerkelab/epitad:latest
