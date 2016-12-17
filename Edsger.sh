#!/bin/bash
name="stdin"
./compiler "$@" > /dev/null 2>&1
#rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
for i in "$@"; do
   if [[ "${i}" =~ [^.]*\.eds$ ]]; then
       IFS='.' read -a name <<< ${i}
       name=${name[0]}
   fi
done
if [[ $@ =~ "-O" ]]; then
/usr/lib/llvm-3.5/bin/llc -O3  ${name}.ll
else
/usr/lib/llvm-3.5/bin/llc -O0  ${name}.ll
fi
[[ $@ =~ "-f" ]] && cat ${name}.s
[[ $@ =~ "-i" ]] && cat ${name}.ll
gcc   ${name}.s -o   ${name} lib.a
