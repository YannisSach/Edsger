@echo off
if exist edsger.lib del edsger.lib

support\masm -mx src\stdio\writei.asm;
support\masm -mx src\stdio\readi.asm;
support\masm -mx src\stdio\writec.asm;
support\masm -mx src\stdio\readc.asm;
support\masm -mx src\stdio\writeb.asm;
support\masm -mx src\stdio\readb.asm;
support\masm -mx src\stdio\writer.asm;
support\masm -mx src\stdio\readr.asm;
support\masm -mx src\stdio\writes.asm;
support\masm -mx src\stdio\reads.asm;

support\lib edsger.lib /NOIGNORECASE +writei.obj +readi.obj;
support\lib edsger.lib /NOIGNORECASE +writec.obj +readc.obj;
support\lib edsger.lib /NOIGNORECASE +writeb.obj +readb.obj;
support\lib edsger.lib /NOIGNORECASE +writer.obj +readr.obj;
support\lib edsger.lib /NOIGNORECASE +writes.obj +reads.obj;

support\masm -mx src\math\abs.asm;
support\masm -mx src\math\fabs.asm;
support\masm -mx src\math\sqrt.asm;
support\masm -mx src\math\sin.asm;
support\masm -mx src\math\cos.asm;
support\masm -mx src\math\tan.asm;
support\masm -mx src\math\atan.asm;
support\masm -mx src\math\exp.asm;
support\masm -mx src\math\ln.asm;
support\masm -mx src\math\pi.asm;

support\lib edsger.lib /NOIGNORECASE +abs.obj fabs.obj +sqrt.obj;
support\lib edsger.lib /NOIGNORECASE +sin.obj +cos.obj +tan.obj +atan.obj;
support\lib edsger.lib /NOIGNORECASE +exp.obj +ln.obj +pi.obj;

support\masm -mx src\stdlib\trunc.asm;
support\masm -mx src\stdlib\round.asm;
support\masm -mx src\stdlib\ord.asm;
support\masm -mx src\stdlib\chr.asm;
support\masm -mx src\stdlib\exit.asm;

support\lib edsger.lib /NOIGNORECASE +trunc.obj +round.obj;
support\lib edsger.lib /NOIGNORECASE +ord.obj +chr.obj;
support\lib edsger.lib /NOIGNORECASE +exit.obj;

support\masm -mx src\string\strlen.asm;
support\masm -mx src\string\strcmp.asm;
support\masm -mx src\string\strcpy.asm;
support\masm -mx src\string\strcat.asm;

support\lib edsger.lib /NOIGNORECASE +strlen.obj +strcmp.obj;
support\lib edsger.lib /NOIGNORECASE +strcpy.obj +strcat.obj;

support\masm -mx src\auxil\new.asm;
support\masm -mx src\auxil\dispose.asm;
support\masm -mx src\auxil\formati.asm;
support\masm -mx src\auxil\formatr.asm;
support\masm -mx src\auxil\parsei.asm;
support\masm -mx src\auxil\parser.asm;

support\lib edsger.lib /NOIGNORECASE +new.obj +dispose.obj;
support\lib edsger.lib /NOIGNORECASE +formati.obj +formatr.obj;
support\lib edsger.lib /NOIGNORECASE +parsei.obj +parser.obj;

support\lib edsger.lib /NOIGNORECASE, edsger.lst;
ren edsger.lib edsger.lib
ren edsger.lst edsger.lst

del *.obj
del *.bak
