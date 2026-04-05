# Catch-all default.do that mimics Adamant's project structure:
# NOTE: no shebang — redo adds "sh -e" automatically, so errors propagate.
# - Handles targets in build/ directory by depending on corresponding source
# - Handles "clean" and "all" redo targets
# - Errors on anything else (source files should never reach here)

case "$1" in
    build/*)
        # Build targets: depend on the corresponding source file
        BASENAME=$(basename "$1")
        redo-ifchange "src/$BASENAME"
        echo "built from: $(cat "src/$BASENAME")" > "$3"
        ;;
    clean)
        rm -rf build src *.log
        ;;
    all)
        # Run the test script
        sh run_tests.sh
        ;;
    *)
        echo "default.do: No rule to build '$1'." >&2
        exit 1
        ;;
esac
