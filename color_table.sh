#!/bin/bash

# SOURCE:
# https://github.com/fikovnik/bin-scripts/blob/master/color-test.sh
# This is a modified version that shows bright colors and color names
#
# This file echoes a bunch of color codes to the terminal to demonstrate
# what's available. Each line is the color code of one forground color,
# out of 17 (default + 16 escapes), followed by a test use of that color
# on all nine background colors (default + 8 escapes).
#
T='gYw'   # The test text
echo -en "\n                       "
for BG in " 40m" "100m" " 41m" "101m" " 42m" "102m" " 43m" "103m" " 44m" "104m" " 45m" "105m" " 46m" "106m" " 47m" "107m"
  do echo -en "${BG}    ";
done

echo -en "\n                       "	  
for BG in " BLK" "bBLK" " RED" "bRED" " GRN" "bGRN" " YLW" "bYLW" " BLU" "bBLU" " MAG" "bMAG" " CYN" "bCYN" " WHT" "bWHI";
#    41m 101m 42m 102m 43m 103m 44m 104m 45m 105m 46m 106m 47m;	  
  do echo -en "${BG}    ";
done

echo -en "\n"
for FGs in '    m ( CLR)' '   1m (bCLR)' '  30m ( BLK)' '1;30m (bBLK)' '  31m ( RED)' '1;31m (bRED)' '  32m ( GRN)' '1;32m (bGRN)' '  33m ( YLW)' '1;33m (bYLW)' '  34m ( BLU)' '1;34m (bBLU)' '  35m ( MAG)' '1;35m (bMAG)' '  36m ( CYN)' '1;36m (bCYN)' '  37m ( WHT)' '1;37m (bWHT)';
  do FGcode=${FGs:0:6}
  FG=${FGcode// /}
  echo -en " $FGs \033[$FG  $T  "
  for BG in 40m 100m 41m 101m 42m 102m 43m 103m 44m 104m 45m 105m 46m 106m 47m 107m;
    do echo -en "$EINS \033[$FG\033[$BG  $T \033[0m\033[$BG \033[0m";
  done
  echo;
done
echo

#
# generates an 8 bit color table (256 colors) for
# reference purposes, using the \033[48;5;${val}m
# ANSI CSI+SGR (see "ANSI Code" on Wikipedia)
#
echo -en "\n   +  "
for i in {0..35}; do
  printf "%2b " $i
done

printf "\n\n %3b  " 0
for i in {0..15}; do
  echo -en "\033[48;5;${i}m  \033[m "
done

#for i in 16 52 88 124 160 196 232; do
for i in {0..6}; do
  let "i = i*36 +16"
  printf "\n\n %3b  " $i
  for j in {0..35}; do
    let "val = i+j"
    echo -en "\033[48;5;${val}m  \033[m "
  done
done

echo -e "\n"
