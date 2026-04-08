#!/bin/sh
# Test for source database refresh optimization.
#
# This test directory has a catch-all default.do that handles build/*
# targets and errors on everything else. The test verifies that
# initializeSourceDatabase is skipped for unchanged source files
# (the DB directory is not deleted and recreated unnecessarily),
# while still refreshing when source files actually change.

set -e

##############################################################################
# Helpers
##############################################################################
get_db_dir() {
    DB_KEY=$(printf '%s' "$1" | md5sum | awk '{print toupper($1)}')
    echo "$HOME/.redo/database/$(echo $DB_KEY | cut -c1-3)/$(echo $DB_KEY | cut -c4-9)/$(echo $DB_KEY | cut -c10-21)/$(echo $DB_KEY | cut -c22-)"
}
get_stamp_dir() {
    DB_KEY=$(printf '%s' "$1" | md5sum | awk '{print toupper($1)}')
    echo "$HOME/.redo/stamps/$(echo $DB_KEY | cut -c1-3)/$(echo $DB_KEY | cut -c4-9)/$(echo $DB_KEY | cut -c10-21)/$(echo $DB_KEY | cut -c22-)"
}

##############################################################################
# Setup
##############################################################################
mkdir -p src build
SRC_PATH="$(cd src && pwd)/data.txt"
BUILD_PATH="$(cd build && pwd)/data.txt"
rm -rf "$(get_db_dir "$SRC_PATH")" "$(get_stamp_dir "$SRC_PATH")"
rm -rf "$(get_db_dir "$BUILD_PATH")" "$(get_stamp_dir "$BUILD_PATH")"
../flush-cache

echo "content v1" > src/data.txt

##############################################################################
# Initial build — creates source DB with stamp
##############################################################################
redo-ifchange build/data.txt
test "$(cat build/data.txt)" = "built from: content v1" || exit 1

SRC_DB=$(get_db_dir "$SRC_PATH")
test -d "$SRC_DB/y" || exit 2

# Record the DB directory inode to detect if it gets recreated
INODE_BEFORE=$(stat -c %i "$SRC_DB" 2>/dev/null || stat -f %i "$SRC_DB" 2>/dev/null)

##############################################################################
# Rebuild with no changes — DB should NOT be recreated
##############################################################################
../flush-cache
redo-ifchange build/data.txt

test -d "$SRC_DB/y" || exit 3

INODE_AFTER=$(stat -c %i "$SRC_DB" 2>/dev/null || stat -f %i "$SRC_DB" 2>/dev/null)
if [ "$INODE_BEFORE" != "$INODE_AFTER" ]; then
    echo "FAIL: source DB was recreated on unchanged rebuild (inode $INODE_BEFORE -> $INODE_AFTER)" >&2
    exit 4
fi

##############################################################################
# Modify source — DB should refresh and target should rebuild
##############################################################################
../sleep
echo "content v2" > src/data.txt
../flush-cache

redo-ifchange build/data.txt
test "$(cat build/data.txt)" = "built from: content v2" || exit 5
test -d "$SRC_DB/y" || exit 6

echo "PASS: source DB skip optimization" >&2
rm -rf src build
