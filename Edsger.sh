# for i in $@; do
#     echo $i
#     if [ ${i} == "-O"  ] ; then
# 	echo "optimized"
# 	/usr/lib/llvm-3.5/bin/opt -O3 -S  ${name[0]}.ll -o ${name[0]}.ll
#     fi;

# done
	 

# /usr/lib/llvm-3.5/bin/llc  ${name[0]}.ll
# gcc   ${name[0]}.s -o   ${name[0]} lib.a
# ./${name[0]}

#!/bin/bash
name="stdin"
opt=no
# ./compiler "$@" > /dev/null 2>&1
./compiler "$@"
#rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
for i in "$@"; do
   if [[ "${i}" =~ [^.]*\.eds$ ]]; then
       IFS='.' read -a name <<< ${i}
       name=${name[0]}
   fi
   if [ ${i} == "-O" -o ${i} == "-o" ] ; then
       opt=yes
   elif [ ${i} != "-O" -o  ${i} != "-o" -o  ${i} != "-f" -o  ${i} != "-i"   ] ; then
       if [[ "${i}" == "-"* ]]; then
	   echo "Unknown flag ${i}"
	   echo "Aborting execution..."
	   exit
       fi;
   fi;
   
done

if [ ${opt} == "yes" ] ; then
    echo "optimized"
    /usr/lib/llvm-3.5/bin/opt -O3 -S  ${name[0]}.ll -o ${name[0]}.ll
fi;
    
if [ ${opt} == "yes" ]; then
/usr/lib/llvm-3.5/bin/llc -O3  ${name}.ll
else
/usr/lib/llvm-3.5/bin/llc -O0  ${name}.ll
fi
[[ $@ =~ "-f" ]] && cat ${name}.s
[[ $@ =~ "-i" ]] && cat ${name}.ll
gcc   ${name}.s -o   ${name} lib.a

    

