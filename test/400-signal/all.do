# Test that SIGINT to the process group kills all redo processes cleanly.
# This simulates what happens when a user presses Ctrl+C.

# Create a .do script that sleeps long enough for us to signal it
cat > slow.do <<'EOF'
sleep 30
echo done > $3
EOF

# Start redo in a new process group (setsid) so we can signal it
# without killing this test script
setsid redo slow &
REDO_PID=$!

# Wait for redo to start and spawn the sleep
sleep 2

# Verify redo is running
if ! kill -0 $REDO_PID 2>/dev/null; then
    echo "FAIL: redo exited before we could signal it" >&2
    rm -f slow.do slow
    exit 1
fi

# Send SIGINT to redo's process group (like Ctrl+C)
kill -INT -$REDO_PID 2>/dev/null

# Give processes time to die
sleep 2

# Check that redo exited
if kill -0 $REDO_PID 2>/dev/null; then
    echo "FAIL: redo PID $REDO_PID still alive after SIGINT" >&2
    kill -9 -$REDO_PID 2>/dev/null
    rm -f slow.do slow
    exit 1
fi

# Check no sleep children from that group survived
REMAINING=$(ps -o pgid=,comm= 2>/dev/null | awk -v g="$REDO_PID" '$1 == g && $2 == "sleep"' | wc -l)
if [ "$REMAINING" -gt 0 ]; then
    echo "FAIL: $REMAINING sleep processes still alive after SIGINT" >&2
    kill -9 -$REDO_PID 2>/dev/null
    rm -f slow.do slow
    exit 1
fi

echo "PASS: SIGINT cleanly killed redo and all children" >&2
rm -f slow.do slow
