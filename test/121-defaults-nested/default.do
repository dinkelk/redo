# echo root 1:$1 2:$2 3:$3 1and3:${1#$2} dirname3: `dirname $3` 1>&2
# Note: my $3 always appears in the current directory, because we know that it exists.
# This is safer. Python-redo does not do this... but we are going to
echo root $2 ${1#$2} #"$(dirname $3)"
