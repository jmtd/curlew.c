{ ****************************************************** stprim ***** }
{ *                                                                 * }
{ *         Software Tools :: Primitives external references        * }
{ *                                                                 * }
{ ******************************************************************* }
 
 
{ %% external references converted for 4.2bsd Pascal (SUN) }
 
 
{ SGETSTE -- initialise software tools external interface }
procedure SGETSTE  (var envlocn : integer;
                    var littoiso : itable;
                    var isotolit : chtable);
        nonpascal;
 
 
{ SRELSTE -- release resources acquired by external interface }
procedure SRELSTE  (envlocn : integer;
                    action : integer);
        nonpascal;
 
 
{ SINTRPT -- set attention interrupt (break) processing }
procedure SINTRPT  (envlocn : integer;
                    reqcode : integer);
        nonpascal;
 
 
{ SOPEN -- open a file }
procedure SOPEN    (envlocn : integer;
                    var name : chstring;
                    mode : integer;
                    var fd : filedesc);
        nonpascal;
 
 
{ SCREATE -- open a file, creating if necessary }
procedure SCREATE  (envlocn : integer;
                    var name : chstring;
                    mode : integer;
                    var fd : filedesc);
        nonpascal;
 
 
{ SIOCTRL -- set state of opened file descriptor }
procedure SIOCTRL  (envlocn : integer;
                    newstate : integer;
                    fd : filedesc);
        nonpascal;
 
 
{ SSECURE -- secure disk copy of opened file without closing it }
procedure SSECURE  (envlocn : integer;
                    fd : filedesc);
        nonpascal;
 
 
{ SREWIND -- rewind (read) or reset (write, append) file desc }
procedure SREWIND  (envlocn : integer;
                    fd : filedesc;
                    var rc : integer);
        nonpascal;
 
 
{ SCLOSE -- close an open file }
procedure SCLOSE   (envlocn : integer;
                    fd : filedesc);
        nonpascal;
 
 
{ SREMOVE -- remove (destroy) a file }
procedure SREMOVE  (envlocn : integer;
                    var name : chstring;
                    var rc : integer);
        nonpascal;
 
 
{ SRENAME -- change the name of a file }
procedure SRENAME  (envlocn : integer;
                    var oldname : chstring;
                    var newname : chstring;
                    var rc : integer);
        nonpascal;
 
 
{ SGETLIN -- read from fd until newline }
procedure SGETLIN  (envlocn : integer;
                    var dest : chstring;
                    fd : filedesc;
                    maxlen : integer;
                    var rc : integer);
        nonpascal;
 
 
{ SGETCF -- read one character from fd }
procedure SGETCF   (envlocn : integer;
                    var c : isochar;
                    fd : filedesc);
        nonpascal;
 
 
{ SPUTSTR -- write string to fd }
procedure SPUTSTR  (envlocn : integer;
                    var src : chstring;
                    fd : filedesc);
        nonpascal;
 
 
{ SPUTCF -- write character to fd }
procedure SPUTCF   (envlocn : integer;
                    c : isochar;
                    fd : filedesc);
        nonpascal;
 
 
{ SFFCOPY -- copy from one file descriptor to another }
procedure SFFCOPY  (envlocn : integer;
                    fdin : filedesc;
                    fdout : filedesc;
                    var nrecds : integer;
                    var rc : integer);
        nonpascal;
 
 
{ SIXCOPY -- copy specified record from one f/d to another }
procedure SIXCOPY  (envlocn : integer;
                    fdx : fdpointer;
                    fdin : filedesc;
                    fdout : filedesc;
                    var rc : integer);
        nonpascal;
 
 
{ SGETIDX -- return next get/put i/o pointer for future get }
procedure SGETIDX  (envlocn : integer;
                    var fdx : fdpointer;
                    fd : filedesc;
                    var rc : integer);
        nonpascal;
 
 
{ SSEEK -- set next get pointer for fd }
procedure SSEEK    (envlocn : integer;
                    fdx : fdpointer;
                    fd : filedesc);
        nonpascal;
 
 
{ SITOXPC -- internal to external file pointer conversion }
procedure SITOXPC  (envlocn : integer;
                    var str : chstring;
                    sidx : integer;
                    fdx : fdpointer);
        nonpascal;
 
 
{ SXTOIPC -- external to internal file pointer conversion }
procedure SXTOIPC  (envlocn : integer;
                    var str : chstring;
                    sidx : integer;
                    var fdx : fdpointer);
        nonpascal;
 
 
{ SGETFDS -- get file descriptor status }
procedure SGETFDS  (envlocn : integer;
                    var fdstat : integer;
                    fd : filedesc);
        nonpascal;
 
 
{ SPROMPT -- set standard input prompt string }
procedure SPROMPT  (envlocn : integer;
                    var pstr : chstring);
        nonpascal;
 
 
{ SGETINF -- return information according to index }
procedure SGETINF  (envlocn : integer;
                    index : integer;
                    var info : chstring;
                    maxlen : integer);
        nonpascal;
 
 
{ SSYSCMD -- execute command line }
procedure SSYSCMD  (envlocn : integer;
                    var cmd : chstring;
                    var rc : integer);
        nonpascal;
 
 
{ SMAILER -- interface to e-mail spooler }
procedure SMAILER  (envlocn : integer;
                    index : integer;
                    var info : chstring;
                    var rc : integer);
        nonpascal;
 
 
{ STWAIT -- wait for a given elapsed time interval }
procedure STWAIT   (envlocn : integer;
                    waittime : integer);
        nonpascal;
 
 
{ SLOGUSE -- log use of software tools application }
procedure SLOGUSE  (envlocn : integer;
                    var name : chstring);
        nonpascal;
