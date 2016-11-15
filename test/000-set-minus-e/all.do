rm -f log
redo fatal >/dev/null 2>/dev/null || true

[ "$(cat log)" = "ok" ] || exit 5
