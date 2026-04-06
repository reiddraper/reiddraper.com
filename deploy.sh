#!/usr/bin/env sh
set -eu

# Build the site
cabal run site -- build

# Stage gh-pages in a temporary worktree so main stays untouched
WORKTREE=$(mktemp -d)
trap 'git worktree remove --force "$WORKTREE" 2>/dev/null || true' EXIT
git worktree add "$WORKTREE" gh-pages

# Replace contents (preserve .git)
find "$WORKTREE" -mindepth 1 -maxdepth 1 ! -name '.git' -exec rm -rf {} +
cp -R _site/. "$WORKTREE/"

# Commit and push
cd "$WORKTREE"
git add -A
if git diff --cached --quiet; then
    echo "No changes to deploy."
else
    git commit -m "Deploy $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    git push origin gh-pages
fi
