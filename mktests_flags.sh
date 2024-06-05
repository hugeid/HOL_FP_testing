#!/bin/bash

# Based on original script by Peter Lammich

set -e
# example: ./mktests_flags.sh -o + -f 64 -t before > testfiles/tests_add64_before.txt

function error() {
  echo "Error: " $@
  exit 1
}

OPS=('+' '-' '*' '/' '*+')
FORMATS=(32 64)
RMODES=('0' '=0') # add '<' and '>' for roundTowardPositive and RoundTowardNegative
NUM_TESTS=""
TININESS="before"
while getopts o:f:r:n:t: flag # o=operation, f=format, r=rounding mode, n=number of tests, t=tininess
do
    case "${flag}" in
        o) OPS=(${OPTARG});;
        f) FORMATS=(${OPTARG});;
        r) RMODES=(${OPTARG});;
        n) N=${OPTARG};;
        t) TININESS=${OPTARG};;
    esac
done

if [ ${N+x} ]; then 
  NUM_TESTS="-n $N";
fi
TESTFLOAT=./TestFloat-3e/build/Linux-x86_64-GCC

# Berkeley TestFloat

TFGEN=$TESTFLOAT/testfloat_gen

function xrm() {
  if   [[ $1 == "=0" ]]; then  echo "near_even";
  elif [[ $1 == "0" ]]; then  echo "minMag";
  elif [[ $1 == ">" ]]; then  echo "max"
  elif [[ $1 == "<" ]]; then  echo "min"
  else error "Unknown rounding mode: '$1'"
  fi
}

function xop() {
  if   [[ $1 == "+" ]]; then echo "add"
  elif [[ $1 == "-" ]]; then  echo "sub"
  elif [[ $1 == "mul" ]]; then  echo "mul"
  elif [[ $1 == "/" ]]; then  echo "div"
  elif [[ $1 == "V" ]]; then  echo "sqrt"
  elif [[ $1 == "*+" ]]; then  echo "mulAdd"
  elif [[ $1 == "eq" ]]; then  echo "eq"
  elif [[ $1 == "lt" ]]; then  echo "lt"
  elif [[ $1 == "le" ]]; then  echo "le"
  else error "Unknown operation: '$1'"
  fi
}

function xtiny() {
  if [[ $1 == "before" ]]; then echo "tininessbefore"
  elif [[ $1 == "after" ]]; then echo "tininessafter"
  else error "Unknown tininess: '$1'"
  fi
}

function gen_tf() {
  ty=$1
  opr=$2
  rmode=$3

#   echo "TFGEN f${ty}_$(xop "$opr")" "-r$(xrm "$rmode")"

  $TFGEN "f${ty}_$(xop "$opr")" "-r$(xrm "$rmode")" "-$(xtiny $TININESS)" $NUM_TESTS \
  | gawk '{ for (i=1;i<NF;++i) $(i) = " 0x" $(i); NF=NF; print }' \
  | sed "s|^|b${ty}${opr} ${rmode} |"
}


for ty in ${FORMATS[@]}; do
  for opr in ${OPS[@]}; do
    for rmode in ${RMODES[@]}; do #for rmode in '0' '=0' '<' '>'; do
      echo "Generating $ty $opr $rmode" >&2
      gen_tf "$ty" "$opr" "$rmode"
    done
  done
done


