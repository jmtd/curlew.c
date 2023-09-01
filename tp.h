 
 
{ ****************************************************** unixprim *** }
{ *                                                                 * }
{ *   Terminal handling primitives for local SSMP -- UNIX version   * }
{ *                                                                 * }
{ ******************************************************************* }
 
{ %% external references converted for 4.2bsd Pascal (SUN) }
 
 
 
{ UINITSCRN -- initialise screen and any associated data structures }
procedure UINITSCRN (var image : imtype;
                     var mode : modearray);
        nonpascal;
 
 
{ UENDSCRN -- tidy up for end of session }
procedure UENDSCRN;
        nonpascal;
 
 
{ USETCURSOR -- set cursor to specified row & col }
procedure USETCURSOR (row : rowtype;
                      col : coltype);
        nonpascal;
 
 
{ UERASEROW -- erase specified row, over column range }
procedure UERASEROW (row : rowtype;
                     cola, colb : coltype);
        nonpascal;
 
 
{ UROWCOPY -- copy rowa to rowb over column range }
procedure UROWCOPY (rowa, rowb : rowtype;
                    cola, colb : coltype);
        nonpascal;
 
 
{ URIGHTSHIFT -- shift row right num characters over column range }
procedure URIGHTSHIFT (row : rowtype;
                       cola, colb : coltype;
                       num : integer);
        nonpascal;
 
 
{ ULEFTSHIFT -- shift row left num characters over column range }
procedure ULEFTSHIFT (row : rowtype;
                      cola, colb : coltype;
                      num : integer);
        nonpascal;
 
 
{ UCHAR -- output character at current cursor position }
procedure UCHAR (ch : isochar);
        nonpascal;
 
 
{ UALARM -- sound the terminal's alarm bell }
procedure UALARM;
        nonpascal;
 
 
{ UREAD -- read next character from keyboard }
procedure UREAD (var inchar : isochar);
        nonpascal;
 
 
{ USETMODE -- tell keyboard drivers about mode array changes }
procedure USETMODE (mode : smallint;
                    var rc : integer);
        nonpascal;
 
 
{ UGETSIZE -- get screen sizes }
procedure UGETSIZE (var row : rowtype;
                    var col : coltype);
        nonpascal;
 
 
{ UTOKEN -- show token state ("host" light) }
procedure UTOKEN (flag : integer);
        nonpascal;
 
 
{ USCROLLUP -- scroll region up }
procedure USCROLLUP (rowa, rowb : rowtype;
                     cola, colb : coltype;
                     count : integer);
        nonpascal;
 
 
{ USCROLLDOWN -- scrollregion down }
procedure USCROLLDOWN (rowa, rowb : rowtype;
                       cola, colb : coltype;
                       count : integer);
        nonpascal;
 
 
{ UINTHOST -- interrupt the m/c (this task) }
procedure UINTHOST;
        nonpascal;
