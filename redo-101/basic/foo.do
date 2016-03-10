redo-ifchange foo.o

gcc foo.o -o $3

# Input arguments:
# $1 - the target name 'foo'
# $2 - the target name with the extension removed, still 'foo'
# $3 - the temporary file that your .do script should write to 'foo.redo.temp1'

