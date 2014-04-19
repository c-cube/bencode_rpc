#!/usr/bin/env bash

for i in tests/*.ml ; do
    echo -n "${i}..."
    $i
done
