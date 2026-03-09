# Test: redo-done marks a pre-built target as up-to-date.
# After calling redo-done, redo-ifchange should NOT re-run the .do script.

# Create source dependency
echo "v1" > source.txt

# Build target externally (simulate gprbuild or similar)
echo "externally built" > target.txt

# Tell redo it's done, with source.txt as a dependency
redo-done target.txt source.txt

# Now redo-ifchange should see target.txt as up-to-date and NOT rebuild it
../flush-cache
redo-ifchange target.txt

# Verify the .do script did NOT run (content should still be "externally built")
CONTENT="$(cat target.txt)"
if [ "$CONTENT" != "externally built" ]; then
    echo "FAIL: redo-done target was rebuilt by .do script! Content: $CONTENT" >&2
    exit 1
fi

echo "PASS: test-basic" >&2
