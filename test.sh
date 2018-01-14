#!/bin/bash

GOOD=$1

for i in $GOOD/*.lat
do
  echo -n "$i  "
  echo ${i%.lat}.output

  ./latc_llvm $i
  lli ${i%.lat}.bc | diff - ${i%.lat}.output
  echo
done
