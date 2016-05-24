; double atan (double d);
; -----------------------
; This function returns the arc tangent of a real number.

; KNOWN BUGS !!!
; 1. Does not handle exceptions.


xseg          segment public 'code'
              assume  cs:xseg, ds:xseg, ss:xseg

              .8087

              public _atan

_atan         proc   near
              push   bp
              mov    bp, sp
              fld    tbyte ptr [bp+8]
              fld1
              fpatan
              mov    si, word ptr [bp+6]      ; store result
              fstp   tbyte ptr [si]
              pop    bp
              ret
_atan         endp

xseg          ends
              end
