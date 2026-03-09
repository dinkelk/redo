# Test: redo-done should fail if the target doesn't exist on disk.

rm -f nonexistent.txt
if redo-done nonexistent.txt source.txt 2>/dev/null; then
    echo "FAIL: redo-done should have failed for nonexistent target" >&2
    exit 1
fi

echo "PASS: test-no-target-fails" >&2
