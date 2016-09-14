./compiler $1
llc output.ll
gcc output.s -o output
echo "Program Result:"
./output
echo $?
