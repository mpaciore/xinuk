#!/bin/bash
for var in "$@"
do
	cat $var | cut -d ":" -f 2- | sort -n > $var.sorted
	mv $var.sorted $var
done
