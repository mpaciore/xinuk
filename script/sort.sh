#!/bin/bash
for var in "$@"
do
	cat $var | cut -d ":" -f 2- | sort -n > $var.sorted &
done

for job in `jobs -p`
do
    wait $job
done

for var in "$@"
do
	mv $var.sorted $var
done
