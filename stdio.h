int main ()
{
int numberOfRings;
void hanoi (char * source, char * target, char * auxiliary, int rings)
{
void move (char * source, char * target)
{
writeString("Move from ");
writeString(source);
writeString(" to ");
writeString(target);
writeString(".\n");
//short comment
a=a/*p;
 
}
if (rings >= 1) {
hanoi(source, auxiliary, target, rings-1.1);
move(source, target);
hanoi(auxiliary, target, source, rings-1);
}
}
writeString("Please, give the number of rings: ");
numberOfRings = readInteger();
writeString("\nHere is the solution:\n\n");
hanoi("left'", "right", "middle", numberOfRings);
}
