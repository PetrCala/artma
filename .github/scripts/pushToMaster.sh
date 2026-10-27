#!/bin/bash

# Pushes the release commits on the current branch to master, retrying on failure.
#
# GitHub occasionally answers a push with a server-side error ("remote: fatal
# error in commit_refs") even though the ref update went through, which aborted
# the v0.5.1 release halfway. A retry then finds master already at the local
# commit and succeeds as a no-op, while a push that genuinely failed is rebased
# onto master and tried again.

set -u

MAX_ATTEMPTS=4

for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
  if git pull --rebase origin master && git push origin master; then
    exit 0
  fi

  if [ "$attempt" -lt "$MAX_ATTEMPTS" ]; then
    delay=$((attempt * 10))
    echo "::warning::Push to master failed (attempt $attempt of $MAX_ATTEMPTS); retrying in ${delay}s."
    sleep "$delay"
  fi
done

echo "::error::Could not push to master after $MAX_ATTEMPTS attempts."
exit 1
