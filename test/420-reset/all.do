exec >&2
. ../skip-if-minimal-do.sh

# redo-reset refuses to run inside a .do file (it detects REDO_SESSION).
# So we unset REDO_SESSION before calling it, and use an isolated HOME
# to avoid interfering with the parent redo session's ~/.redo state.
TEST_HOME=$(mktemp -d)
TEST_DIR=$(mktemp -d)
TEST_TMPDIR=$(mktemp -d)
cleanup() { rm -rf "$TEST_HOME" "$TEST_DIR" "$TEST_TMPDIR"; }
trap cleanup EXIT

# Create a simple .do file for test builds
cat > "$TEST_DIR/target1.do" << 'DOEOF'
echo "built" > "$3"
DOEOF

# Helper to run redo commands in the isolated environment.
# Unsets REDO_KEY so redo doesn't think it's inside a .do build.
# Uses isolated HOME so ~/.redo doesn't conflict with parent session.
iredo() {
    env -u REDO_KEY -u REDO_SESSION -u REDO_INIT_PATH HOME="$TEST_HOME" TMPDIR="$TEST_TMPDIR" "$@"
}

# Helper: get the redo meta directory for isolated env
redo_meta_dir() {
    echo "$TEST_HOME/.redo"
}

# Test 1: redo-reset with no prior state
echo "Test 1: redo-reset with no prior state"
rm -rf "$(redo_meta_dir)"
output=$(iredo redo-reset 2>&1)
rc=$?
if [ "$rc" -ne 0 ]; then
    echo "FAIL: redo-reset exited with $rc: $output" >&2
    exit 1
fi
echo "  PASS"

# Test 2: redo-reset removes state after a build
echo "Test 2: redo-reset removes state after build"
(cd "$TEST_DIR" && iredo redo target1)
[ -e "$TEST_DIR/target1" ] || exit 3
[ -d "$(redo_meta_dir)" ] || { echo "FAIL: meta dir missing after build" >&2; exit 4; }
output=$(iredo redo-reset 2>&1)
rc=$?
if [ "$rc" -ne 0 ]; then
    echo "FAIL: redo-reset exited with $rc" >&2
    exit 5
fi
case "$output" in
    *"Redo state reset"*) ;; # good
    *) echo "FAIL: expected 'Redo state reset', got: $output" >&2; exit 6 ;;
esac
echo "  PASS"

# Test 3: redo-reset removes ~/.redo
echo "Test 3: redo-reset removes meta directory"
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
[ -d "$(redo_meta_dir)" ] || { echo "FAIL: meta dir missing after build" >&2; exit 7; }
iredo redo-reset >/dev/null 2>&1
if [ -d "$(redo_meta_dir)" ]; then
    echo "FAIL: meta dir still exists after reset" >&2
    exit 8
fi
echo "  PASS"

# Test 4: Build works correctly after reset (clean slate)
echo "Test 4: build works after reset"
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
iredo redo-reset >/dev/null 2>&1
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
[ -e "$TEST_DIR/target1" ] || { echo "FAIL: target1 not built after reset" >&2; exit 9; }
content=$(cat "$TEST_DIR/target1")
if [ "$content" != "built" ]; then
    echo "FAIL: expected 'built', got: $content" >&2
    exit 10
fi
echo "  PASS"

# Test 5: redo-reset does not remove build outputs (only state)
echo "Test 5: redo-reset preserves build outputs"
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
[ -e "$TEST_DIR/target1" ] || exit 11
iredo redo-reset >/dev/null 2>&1
if [ ! -e "$TEST_DIR/target1" ]; then
    echo "FAIL: target1 removed by reset (should only remove state)" >&2
    exit 12
fi
echo "  PASS"

# Test 6: redo-reset reports what was removed
echo "Test 6: redo-reset reports removed directories"
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
output=$(iredo redo-reset 2>&1)
case "$output" in
    *"Removed"*) ;; # good
    *) echo "FAIL: expected 'Removed' in output, got: $output" >&2; exit 13 ;;
esac
echo "  PASS"

# Test 7: Multiple resets are idempotent
echo "Test 7: multiple resets are idempotent"
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
iredo redo-reset >/dev/null 2>&1
output=$(iredo redo-reset 2>&1)
rc=$?
if [ "$rc" -ne 0 ]; then
    echo "FAIL: second reset exited with $rc" >&2
    exit 14
fi
echo "  PASS"

# Test 8: redo-reset --help shows usage
echo "Test 8: redo-reset --help"
output=$(iredo redo-reset --help 2>&1)
rc=$?
if [ "$rc" -ne 0 ]; then
    echo "FAIL: --help exited with $rc" >&2
    exit 16
fi
case "$output" in
    *"usage: redo-reset"*) ;; # good
    *) echo "FAIL: expected usage line, got: $output" >&2; exit 17 ;;
esac
echo "  PASS"

# Test 9: After reset, removing target and rebuilding works
echo "Test 9: rebuild from scratch after reset"
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
[ -e "$TEST_DIR/target1" ] || exit 18
iredo redo-reset >/dev/null 2>&1
# After reset, existing target is treated as source (no DB entry).
# Remove it to get a clean rebuild.
rm -f "$TEST_DIR/target1"
(cd "$TEST_DIR" && iredo redo target1)
[ -e "$TEST_DIR/target1" ] || { echo "FAIL: target1 not rebuilt after reset+rm" >&2; exit 19; }
content=$(cat "$TEST_DIR/target1")
if [ "$content" != "built" ]; then
    echo "FAIL: expected 'built', got: $content" >&2
    exit 20
fi
echo "  PASS"

# Test 10: redo-reset --version works
echo "Test 10: redo-reset --version"
output=$(iredo redo-reset --version 2>&1)
rc=$?
if [ "$rc" -ne 0 ]; then
    echo "FAIL: --version exited with $rc" >&2
    exit 21
fi
case "$output" in
    *redo*) ;; # good
    *) echo "FAIL: expected version info, got: $output" >&2; exit 22 ;;
esac
echo "  PASS"

# Test 11: redo-reset output mentions redo state
echo "Test 11: output mentions redo state"
(cd "$TEST_DIR" && rm -f target1 && iredo redo target1)
output=$(iredo redo-reset 2>&1)
case "$output" in
    *"Redo state"*) ;; # good
    *) echo "FAIL: expected 'Redo state' in output, got: $output" >&2; exit 23 ;;
esac
echo "  PASS"
