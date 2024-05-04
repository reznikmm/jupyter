#!/bin/bash

# SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT

set -x -e
GITHUB_ACCESS_TOKEN=$1
TAG="${GITHUB_REF##*/}" # Release name/tag

# Refetch tags
git tag --delete $TAG; git fetch --tags

git show --no-patch --format=%n $TAG | \
 sed -e '1,/Release notes/d' > release_notes.md

# Try to create a release
jq --null-input --arg tag $TAG \
   --rawfile body release_notes.md \
   '{"tag_name": $tag, "name": $tag, "body": $body}' | \
curl -v \
  -X POST \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: token $GITHUB_ACCESS_TOKEN" \
  -H 'Content-Type: application/json' \
  https://api.github.com/repos/$GITHUB_REPOSITORY/releases \
  -d "@-"

rm -f release_notes.md

# Get asset upload url, drop "quotes" around it and {parameters} at the end
upload_url=$( curl \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: token $GITHUB_ACCESS_TOKEN" \
  https://api.github.com/repos/$GITHUB_REPOSITORY/releases/tags/$TAG | \
  jq -r '.upload_url | rtrimstr("{?name,label}")')

echo "upload_url=$upload_url"

for FILE in *.zip ; do
  # Upload $FILE as an asset to the release
  curl \
    -X POST \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: token $GITHUB_ACCESS_TOKEN" \
    -H 'Content-Type: application/zip' \
    --data-binary @$FILE \
    $upload_url?name=$FILE
done
