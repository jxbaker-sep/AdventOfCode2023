#!/bin/bash

function test() {
  day=$1
  part=$2
  expect=$3

  result=`swipl -g 'sample(Data), do_part$part(Data, Result), writeln(Result)' -g halt day$day.pl` 
  
}

day2_2s=`swipl -g 'sample(Data), do_part1(Data, Result), writeln(Result)' -g halt day2.pl` 