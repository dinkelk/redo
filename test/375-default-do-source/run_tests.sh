#!/bin/sh
# Tests for source database refresh optimization.
#
# This test directory has a catch-all default.do (like Adamant) that handles
# build/* targets and errors on everything else. Tests verify that source
# files are handled correctly, especially around the optimization that skips
# unnecessary initializeSourceDatabase calls for unchanged source files.

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
clean_redo_state() {
    # Clean redo DB and stamps for a file
    rm -rf "$(get_db_dir "$1")" "$(get_stamp_dir "$1")"
}

PASS=0
FAIL=0
pass() { echo "PASS: $1" >&2; PASS=$((PASS + 1)); }
fail() { echo "FAIL: $1" >&2; FAIL=$((FAIL + 1)); }

##############################################################################
# Test 1: Unchanged source files skip DB refresh
#
# After an initial build, rebuilding with an unchanged source file should
# NOT call initializeSourceDatabase (which deletes and recreates the DB).
# We verify by checking that the source marker persists and the DB directory
# inode is unchanged (not deleted and recreated).
##############################################################################
test_skip_unchanged() {
    mkdir -p src build
    SRC_PATH="$(cd src && pwd)/data.txt"
    BUILD_PATH="$(cd build && pwd)/data.txt"
    clean_redo_state "$SRC_PATH"
    clean_redo_state "$BUILD_PATH"
    ../flush-cache

    echo "content v1" > src/data.txt

    # Initial build — creates source DB with stamp
    redo-ifchange build/data.txt
    test "$(cat build/data.txt)" = "built from: content v1" || { fail "skip-unchanged: wrong initial content"; return; }

    SRC_DB=$(get_db_dir "$SRC_PATH")
    test -d "$SRC_DB/y" || { fail "skip-unchanged: no source marker after initial build"; return; }

    # Record the DB directory's inode to detect if it gets recreated
    INODE_BEFORE=$(stat -c %i "$SRC_DB" 2>/dev/null || stat -f %i "$SRC_DB" 2>/dev/null)

    # Rebuild with no changes — should skip initializeSourceDatabase
    ../flush-cache
    redo-ifchange build/data.txt

    # Source marker must still exist
    test -d "$SRC_DB/y" || { fail "skip-unchanged: source marker lost on rebuild"; return; }

    # DB directory inode should be the same (not deleted and recreated)
    INODE_AFTER=$(stat -c %i "$SRC_DB" 2>/dev/null || stat -f %i "$SRC_DB" 2>/dev/null)
    test "$INODE_BEFORE" = "$INODE_AFTER" || { fail "skip-unchanged: DB was recreated (inode changed: $INODE_BEFORE -> $INODE_AFTER)"; return; }

    pass "skip-unchanged"
    rm -rf src build
}

##############################################################################
# Test 2: Changed source files DO get DB refresh
#
# When a source file changes (different mtime), initializeSourceDatabase
# must run to update the stamp and trigger dependent rebuilds.
##############################################################################
test_refresh_on_change() {
    mkdir -p src build
    SRC_PATH="$(cd src && pwd)/data.txt"
    BUILD_PATH="$(cd build && pwd)/data.txt"
    clean_redo_state "$SRC_PATH"
    clean_redo_state "$BUILD_PATH"
    ../flush-cache

    echo "content v1" > src/data.txt
    redo-ifchange build/data.txt
    test "$(cat build/data.txt)" = "built from: content v1" || { fail "refresh-on-change: wrong initial content"; return; }

    # Modify the source
    ../sleep
    echo "content v2" > src/data.txt
    ../flush-cache

    # Rebuild — should detect change and rebuild dependent target
    redo-ifchange build/data.txt
    test "$(cat build/data.txt)" = "built from: content v2" || { fail "refresh-on-change: change not detected"; return; }

    # Source marker must still exist
    SRC_DB=$(get_db_dir "$SRC_PATH")
    test -d "$SRC_DB/y" || { fail "refresh-on-change: source marker lost after change"; return; }

    pass "refresh-on-change"
    rm -rf src build
}

##############################################################################
# Test 3: New source files get properly initialized
#
# A source file encountered for the first time (no DB, no stamp) must have
# initializeSourceDatabase called to create the DB with source marker.
##############################################################################
test_new_source_init() {
    mkdir -p src build
    SRC_PATH="$(cd src && pwd)/data.txt"
    BUILD_PATH="$(cd build && pwd)/data.txt"
    clean_redo_state "$SRC_PATH"
    clean_redo_state "$BUILD_PATH"
    ../flush-cache

    echo "new content" > src/data.txt

    # First build — source has no DB at all
    redo-ifchange build/data.txt
    test "$(cat build/data.txt)" = "built from: new content" || { fail "new-source-init: wrong content"; return; }

    SRC_DB=$(get_db_dir "$SRC_PATH")
    test -d "$SRC_DB/y" || { fail "new-source-init: source marker not created"; return; }

    pass "new-source-init"
    rm -rf src build
}

##############################################################################
# Run all tests
##############################################################################
test_skip_unchanged
test_refresh_on_change
test_new_source_init

echo "" >&2
echo "Results: $PASS passed, $FAIL failed" >&2
test "$FAIL" -eq 0
