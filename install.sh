#!/bin/sh
helper=util/install-helper
./do $helper
chmod +x $helper
$helper
./do
