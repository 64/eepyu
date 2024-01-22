#!/usr/bin/sh
while read num; do
    if [ "$num" == "zzzzzzzz" ] || [ "$num" == "xxxxxxxx" ]; then
        echo "xxxx"
    else
        echo "$num" > /dev/stderr
        riscv-machinsn-decode hexstring "$num"
    fi
done
