# This .do script uses redo-done to mark a sub-target as built
echo "sub content" > target.txt
redo-done target.txt source.txt
echo "wrapper done" > "$3"
