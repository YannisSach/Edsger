; int strlen (char * s);
; ----------------------
; This function returns the number of characters in the null
; terminated string s. A pointer to the string is passed.
; The last character (that is the null) is not counted.


xseg        segment public 'code'
            assume  cs:xseg, ds:xseg, ss:xseg

            public _strlen

_strlen     proc  near
            push  bp
            mov   bp, sp
            mov   si, word ptr [bp+8]      ; 1st parameter
            xor   cx, cx                   ; counter = 0
next:
            mov   dl, byte ptr [si]        ; Load next character
            or    dl, dl
            jz    ok                       ; until it is 0
            inc   si
            inc   cx                       ; counter++
            jmp   short next
ok:
            mov   si, word ptr [bp+6]      ; Address of result
            mov   word ptr [si], cx
            pop   bp
            ret
_strlen     endp

xseg        ends
            end
