if [ -e created ]; then
  redo-ifchange created
else
  redo-ifcreate created
fi
redo-ifchange c/all
echo "3"
