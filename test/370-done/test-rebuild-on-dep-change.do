# Test: After redo-done, if the target itself is modified externally,
# redo should detect it's been modified and warn/skip.

echo "original" > source.txt
echo "externally built" > target.txt
redo-done target.txt source.txt

# Verify up-to-date
../flush-cache
redo-ifchange target.txt
CONTENT="$(cat target.txt)"
if [ "$CONTENT" != "externally built" ]; then
    echo "FAIL: target was rebuilt when it shouldn't have been" >&2
    exit 1
fi

echo "PASS: test-rebuild-on-dep-change" >&2
