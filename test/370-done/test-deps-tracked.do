# Test: redo-done registers dependencies correctly.
# Changing a dependency should cause the target to be rebuilt.

# Setup
echo "v1" > depfile.txt
echo "externally built v1" > target2.txt

# Register with redo-done
redo-done target2.txt depfile.txt

# Verify it's up-to-date
../flush-cache
redo-ifchange target2.txt
CONTENT="$(cat target2.txt)"
if [ "$CONTENT" != "externally built v1" ]; then
    echo "FAIL: target2.txt was unexpectedly rebuilt" >&2
    exit 1
fi

# Now change the dependency
../sleep
echo "v2" > depfile.txt

# Rebuild — this time redo should detect the dep changed and re-run the .do
../flush-cache
redo target2.txt
CONTENT="$(cat target2.txt)"
if [ "$CONTENT" != "built by do script" ]; then
    echo "FAIL: target2.txt was NOT rebuilt after dep change! Content: $CONTENT" >&2
    exit 1
fi

echo "PASS: test-deps-tracked" >&2
