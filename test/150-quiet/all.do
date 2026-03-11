exec >&2
. ../skip-if-minimal-do.sh

# Test 1: With -q, successful build produces no output
echo "Test 1: quiet mode suppresses output on success"
rm -f target1
output=$(redo -q target1 2>&1)
if [ -n "$output" ]; then
    echo "FAIL: expected no output with -q, got: $output" >&2
    exit 1
fi
[ -e target1 ] || exit 2
echo "  PASS"

# Test 2: Without -q, successful build produces output
echo "Test 2: normal mode shows output"
rm -f target1
output=$(redo target1 2>&1)
if [ -z "$output" ]; then
    echo "FAIL: expected output without -q" >&2
    exit 3
fi
echo "  PASS"

# Test 3: With -q, errors are still printed
echo "Test 3: quiet mode still shows errors"
rm -f failme
set +e
output=$(redo -q failme 2>&1)
rc=$?
set -e
if [ "$rc" -eq 0 ]; then
    echo "FAIL: expected non-zero exit" >&2
    exit 4
fi
if [ -z "$output" ]; then
    echo "FAIL: expected error output with -q on failure" >&2
    exit 5
fi
case "$output" in
    *Error*) ;; # good
    *) echo "FAIL: expected 'Error' in output, got: $output" >&2; exit 6 ;;
esac
echo "  PASS"

# Test 4: -q propagates to child redo processes (nested builds)
echo "Test 4: quiet mode propagates to nested builds"
rm -f outer inner
output=$(redo -q outer 2>&1)
if [ -n "$output" ]; then
    echo "FAIL: expected no output from nested -q build, got: $output" >&2
    exit 7
fi
[ -e outer ] || exit 8
[ -e inner ] || exit 9
echo "  PASS"

# Test 5: --quiet long form works
echo "Test 5: --quiet long form"
rm -f target1
output=$(redo --quiet target1 2>&1)
if [ -n "$output" ]; then
    echo "FAIL: expected no output with --quiet, got: $output" >&2
    exit 10
fi
echo "  PASS"

# Test 6: -q combined with other flags
echo "Test 6: -q combined with -k (keep-going)"
rm -f target1
output=$(redo -q -k target1 2>&1)
if [ -n "$output" ]; then
    echo "FAIL: expected no output with -q -k, got: $output" >&2
    exit 11
fi
echo "  PASS"

# Test 7: -q with multiple targets
echo "Test 7: -q with multiple targets"
rm -f target1 inner
output=$(redo -q target1 inner 2>&1)
if [ -n "$output" ]; then
    echo "FAIL: expected no output with -q and multiple targets, got: $output" >&2
    exit 12
fi
[ -e target1 ] || exit 13
[ -e inner ] || exit 14
echo "  PASS"

# Test 8: -q doesn't affect build correctness (target content is right)
echo "Test 8: -q doesn't affect build output content"
rm -f target1
redo -q target1
content=$(cat target1)
if [ "$content" != "built" ]; then
    echo "FAIL: expected 'built' in target1, got: $content" >&2
    exit 15
fi
echo "  PASS"

# Test 9: Nested error in quiet mode still shows error chain
echo "Test 9: nested error in quiet mode shows error chain"
rm -f outer_fail inner_fail
set +e
output=$(redo -q outer_fail 2>&1)
rc=$?
set -e
if [ "$rc" -eq 0 ]; then
    echo "FAIL: expected non-zero exit from outer_fail" >&2
    exit 16
fi
case "$output" in
    *Error*) ;; # good
    *) echo "FAIL: expected error output, got: $output" >&2; exit 17 ;;
esac
echo "  PASS"

# Test 10: Warnings are NOT suppressed by -q
echo "Test 10: quiet mode still shows warnings"
rm -f warnme
# First build warnme so redo knows it as a target
redo warnme
# Now remove and create as plain source file
rm -f warnme
echo "i am a source" > warnme
# Trying to redo it should produce a warning (source file exists)
set +e
output=$(redo -q warnme 2>&1)
set -e
if [ -z "$output" ]; then
    echo "FAIL: expected warning output with -q, got nothing" >&2
    exit 18
fi
case "$output" in
    *Warning*) ;; # good — warning was printed
    *) echo "FAIL: expected 'Warning' in output, got: $output" >&2; exit 19 ;;
esac
rm -f warnme
echo "  PASS"
