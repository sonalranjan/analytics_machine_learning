#!/bin/bash

# $1==last_id && { curr_u = $4 "_" $5; if (curr_u != last_u) { printf "%s,%s,%s_%s\n", $1, (cnt[curr_u] + $2),$4,$5; last_u = curr_u; } else { cnt[$curr_u] += $2; } }' $1 

awk -F, ' BEGIN { fmt1_="%s,%s,%s"; fmt_="%s,%s\n"; } \
$0 ~ /.*/ { curr_u_u =sprintf(fmt1_,$1,$4,$5); } \
$5 ~ /^[12][0-9][0-9][0-9]$/ { curr_u_u=sprintf(fmt1_,$1,$4,""); } \
$0 ~ /.*/ { if (last_u_u != curr_u_u) { printf fmt_,last_u_u, freq; last_u_u = curr_u_u; freq = $2; } else { freq+=$2; } } \
END { printf fmt_,last_u_u,freq; } ' $1


