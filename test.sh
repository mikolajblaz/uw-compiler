#!/bin/bash

GOOD=$1

for i in $GOOD/*.lat
do
  echo "$i"
  INPUT=${i%.lat}.input
  OUTPUT=${i%.lat}.output
  OUT=${i%.lat}.out
  BC=${i%.lat}.bc

  ./latc_llvm $i
  if [ -e $INPUT ]
    then lli $BC < $INPUT > $OUT
    else lli $BC > $OUT
  fi

  diff $OUT $OUTPUT
  echo
done
