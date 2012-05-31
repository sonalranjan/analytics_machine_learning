#!/bin/bash

# $1==last_id && { curr_u = $4 "_" $5; if (curr_u != last_u) { printf "%s,%s,%s_%s\n", $1, (cnt[curr_u] + $2),$4,$5; last_u = curr_u; } else { cnt[$curr_u] += $2; } }' $1 

mawk -F, ' BEGIN { fmt1_="%s,%s,%s,%s"; fmt_="%s,%s\n"; } \
$5 ~ /[12][0-9][0-9][0-9]/ { $5=""; } \
$6 ~ /[12][0-9][0-9][0-9]/ { gsub(/[0-9]*/,"",$6); } \
$0 ~ /.*/ { \
            if (last_uid != $1) { \
                for (k in uid_url) { \
                    printf(fmt_, k, uid_url[k]); \
                };\
                delete uid_url; \
                last_uid=$1; \
                curr_u_u=sprintf(fmt1_,$1,$4,$5,$6); \
                uid_url[curr_u_u] += $2; \
            } else { \
                curr_u_u=sprintf(fmt1_,$1,$4,$5,$6); \
                uid_url[curr_u_u] += $2; \
            } \
        } \
END { for (k in uid_url) { printf(fmt_, k, uid_url[k]);  } } ' $1


