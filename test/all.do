# tests that "set -e" works (.do scripts always run with -e set by default)
rm -f 000-set-minus-e/log
redo 000-set-minus-e/all
if [ "$(cat 000-set-minus-e/log)" != "ok" ]; then
    echo "FATAL! .do file not run with 'set -e' in effect!" >&2
    exit 5
fi

# builds 1xx*/all
/bin/ls 1[0-9][0-9]*/all.do |
sed 's/\.do$//' |
sed 's@120-defaults-flat/all@@1' |
sed 's@121-defaults-nested/all@@1' |
sed 's@140-shuffle/all@@1' |
sed 's@141-keep-going/all@@1' |
xargs redo
110-compile/hello >&2

# builds the rest
/bin/ls [2-9][0-9][0-9]*/all.do |
sed 's/\.do$//' |
sed 's@200-shell/all@@1' |
#sed 's@350-deps/all@@1' |
sed 's@660-stamp/all@@1' |
sed 's@999-installer/all@@1' |
xargs redo-ifchange
