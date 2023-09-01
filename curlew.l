.ds f. curlew.t
.TH curlew 1 local
.SH NAME
curlew,cl \- SSMP Screen Editor
.SH SYNOPSIS
.B curlew
file [command]
.SH Essential context editor commands
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 80 0
.nr 38 \wwrite
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wquit
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wedit myotherfile
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wview
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wWrite file buffer contents to edited file
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wEnd the editor session
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wEdit a new disk file "myotherfile"
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wEnter (or exit) full-screen operation
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 14 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'write\h'|\n(41u'Write file buffer contents to edited file
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'quit\h'|\n(41u'End the editor session
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'edit myotherfile\h'|\n(41u'Edit a new disk file "myotherfile"
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'view\h'|\n(41u'Enter (or exit) full-screen operation
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.LP
From full-screen operation, first press ESC then 6 to get a command
prompt.  When complete, press RETURN to enter the command.  If a
filename is given with a "write" command the file buffer will be
written to the specified file instead of the edited file.
.SH Control key action during full-screen operation
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84 85
.nr 80 0
.nr 38 \w^Q
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^W
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^E
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^R
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^T
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^Y
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^U
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^I
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^O
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w^P
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \winsert line
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wchar/line toggle
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete character
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete word
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete line start
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete line end
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete line
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnext tab
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wsplit line
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wcombine with next
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \w^A
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^S
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^D
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^F
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^G
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^H
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^J
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^K
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w^L
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \walign text
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \winsert space
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wdelete blanks
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wprevious tab
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \whome cursor
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcursor left
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcursor down
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcursor up
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcursor right
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \w^Z
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w^X
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w^C
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w^V
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w^B
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w^N
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w^M
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 85 0
.nr 38 \wcursor line start
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wcursor text start
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wcursor text end
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wcursor next word
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wblock toggle
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wins saved text
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \w(RETURN) append
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wor split line
.if \n(85<\n(38 .nr 85 \n(38
.85
.rm 85
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr 45 \n(84+(3*\n(38)
.nr 85 +\n(45
.nr TW \n(85
.if t .if \n(TW>\n(.li .tm Table at line 33 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^Q\h'|\n(41u'insert line\h'|\n(42u'^A\h'|\n(43u'align text\h'|\n(44u'^Z\h'|\n(45u'cursor line start
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^W\h'|\n(41u'char/line toggle\h'|\n(42u'^S\h'|\n(43u'insert space\h'|\n(44u'^X\h'|\n(45u'cursor text start
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^E\h'|\n(41u'delete character\h'|\n(42u'^D\h'|\n(43u'delete blanks\h'|\n(44u'^C\h'|\n(45u'cursor text end
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^R\h'|\n(41u'delete word\h'|\n(42u'^F\h'|\n(43u'previous tab\h'|\n(44u'^V\h'|\n(45u'cursor next word
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^T\h'|\n(41u'delete line start\h'|\n(42u'^G\h'|\n(43u'home cursor\h'|\n(44u'^B\h'|\n(45u'block toggle
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^Y\h'|\n(41u'delete line end\h'|\n(42u'^H\h'|\n(43u'cursor left\h'|\n(44u'^N\h'|\n(45u'ins saved text
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^U\h'|\n(41u'delete line\h'|\n(42u'^J\h'|\n(43u'cursor down\h'|\n(44u'^M\h'|\n(45u'(RETURN) append
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^I\h'|\n(41u'next tab\h'|\n(42u'^K\h'|\n(43u'cursor up\h'|\n(44u'\h'|\n(45u'or split line
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^O\h'|\n(41u'split line\h'|\n(42u'^L\h'|\n(43u'cursor right\h'|\n(44u'\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'^P\h'|\n(41u'combine with next\h'|\n(42u'\h'|\n(43u'\h'|\n(44u'\h'|\n(45u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.LP
Note that "^Q" means hold down "CTRL", press "Q" once and release.
.SH Line number parameters and image relocation
In full-screen operation, first press F6 (ESC then 6) to get the
command prompt.  Press RETURN when the parameter is complete.
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82
.nr 80 0
.nr 38 \wn
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w+n
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-n
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w$
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w#
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/pat/
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\epat\e
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wLine n in the file buffer, for example:
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wn lines after the current line, example:
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wn lines before the current line, example:
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wThe last line in the file buffer.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wThe top-of-file image line number.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNext line matching the pattern, example:
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPrevious line matching pattern, example:
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \w563
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w+60
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w-125
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w/Edinburgh/
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w\ecity\e
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 50 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'n\h'|\n(41u'Line n in the file buffer, for example:\h'|\n(42u'563
.	The current line (or cursor line).
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+n\h'|\n(41u'n lines after the current line, example:\h'|\n(42u'+60
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-n\h'|\n(41u'n lines before the current line, example:\h'|\n(42u'-125
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'$\h'|\n(41u'The last line in the file buffer.\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'#\h'|\n(41u'The top-of-file image line number.\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/pat/\h'|\n(41u'Next line matching the pattern, example:\h'|\n(42u'/Edinburgh/
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\epat\e\h'|\n(41u'Previous line matching pattern, example:\h'|\n(42u'\ecity\e
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-10
.LP
Several line parameters may be grouped together separated by commas or
semicolons.  A semicolon updates the current line pointer.
.SH Patterns
Patterns are an extension of the "Software Tools" scheme where
?=any,
%=start-of-line,
$=end-of-line,
[]=char-class,
*=closure,
@=literal,
~=case-sensitivity-toggle.
The UNIX (tm) set is available via a "set u+" command.
.LP
Examples of patterns used in searches:
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 80 0
.nr 38 \w/procedure/
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/~joe/
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w//
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\e\e
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/%Name:/
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/%$/
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/%[0-9][0-9]*./
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/@*@*@*/
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wnext line containing "procedure"
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnext line containing "joe", "JOE", "Joe", etc.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnext line containing last pattern specified
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wprevious line containing last pattern
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnext line starting with "Name:"
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnext line with no text on it
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnext line starting with one or more digits
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wfollowed by a period, "."
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnext line containing three consecutive
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wasterisks ("@" overrides metacharacters)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 4in
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 79 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/procedure/\h'|\n(41u'next line containing "procedure"
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/~joe/\h'|\n(41u'next line containing "joe", "JOE", "Joe", etc.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'//\h'|\n(41u'next line containing last pattern specified
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\e\e\h'|\n(41u'previous line containing last pattern
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/%Name:/\h'|\n(41u'next line starting with "Name:"
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/%$/\h'|\n(41u'next line with no text on it
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/%[0-9][0-9]*./\h'|\n(41u'next line starting with one or more digits
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'followed by a period, "."
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/@*@*@*/\h'|\n(41u'next line containing three consecutive
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'asterisks ("@" overrides metacharacters)
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.SH Full-screen operation in block mode
Block start (or end) is marked by a ^B.  Move the cursor to the other
end of the block.  The editing keys ^[QWERTYUOPASD], but not RETURN or
DEL, now operate on the block of text.
.LP
For the "character" operations the block is a rectangle, for "line"
operation the group of lines, for "delete word" the serial
intermediate text.  ^Q and ^W save lines or a character rectangle in
block mode; to insert a copy of the text that has been saved press ^N.
Note that RETURN is "start-of-next-line" after ^B.
.SH Context editor commands
.LP
These are:
.LP
append, change, copy, delete, edit, exit, filename, format, global,
help, insert, keep, move, list, print, prompt, quit, read, set, show,
stop, substitute, undo, view, write, xglobal
.LP
Examples:
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 80 0
.nr 38 \wg/type/list
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wg/%[0-9]/list
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1,$sub/Joe/Joseph/g
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1,$sub/% *//
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w+,$delete
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1,-delete
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wg/%$/delete
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wx/%[0-9]/sub/%/\ \ \ \ \ /
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wlist all lines containing "type"
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wlist all lines starting with a digit
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wreplace every "Joe" by "Joseph"
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wremove all leading blanks
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete lines beyond the cursor line
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete lines before the cursor line
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdelete all blank lines
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wfor lines not starting with a digit,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wshift right five spaces
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 4in
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 111 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'g/type/list\h'|\n(41u'list all lines containing "type"
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'g/%[0-9]/list\h'|\n(41u'list all lines starting with a digit
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1,$sub/Joe/Joseph/g\h'|\n(41u'replace every "Joe" by "Joseph"
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1,$sub/% *//\h'|\n(41u'remove all leading blanks
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+,$delete\h'|\n(41u'delete lines beyond the cursor line
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1,-delete\h'|\n(41u'delete lines before the cursor line
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'g/%$/delete\h'|\n(41u'delete all blank lines
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'x/%[0-9]/sub/%/\ \ \ \ \ /\h'|\n(41u'for lines not starting with a digit,
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'shift right five spaces
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.SH Function keys and their default settings
A function keystroke is signalled by pressing "ESC" once then a second
key once.  The second key for each function key is:
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84 85 86 87 88 89
.nr 80 0
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wQ
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF11
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wA
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF21
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wZ
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF31
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w2
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wF2
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wW
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wF12
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wS
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wF22
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wX
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wF32
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \w3
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wF3
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wE
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wF13
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wD
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wF23
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wC
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wF33
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \w4
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wF4
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wR
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wF14
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wF
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wF24
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wV
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wF34
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \w5
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wF5
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wT
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wF15
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wG
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wF25
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wB
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wF35
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 85 0
.nr 38 \w6
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wF6
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wY
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wF16
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wH
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wF26
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wN
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \wF36
.if \n(85<\n(38 .nr 85 \n(38
.85
.rm 85
.nr 86 0
.nr 38 \w7
.if \n(86<\n(38 .nr 86 \n(38
.nr 38 \wF7
.if \n(86<\n(38 .nr 86 \n(38
.nr 38 \wU
.if \n(86<\n(38 .nr 86 \n(38
.nr 38 \wF17
.if \n(86<\n(38 .nr 86 \n(38
.nr 38 \wJ
.if \n(86<\n(38 .nr 86 \n(38
.nr 38 \wF27
.if \n(86<\n(38 .nr 86 \n(38
.nr 38 \wM
.if \n(86<\n(38 .nr 86 \n(38
.nr 38 \wF37
.if \n(86<\n(38 .nr 86 \n(38
.86
.rm 86
.nr 87 0
.nr 38 \w8
.if \n(87<\n(38 .nr 87 \n(38
.nr 38 \wF8
.if \n(87<\n(38 .nr 87 \n(38
.nr 38 \wI
.if \n(87<\n(38 .nr 87 \n(38
.nr 38 \wF18
.if \n(87<\n(38 .nr 87 \n(38
.nr 38 \wK
.if \n(87<\n(38 .nr 87 \n(38
.nr 38 \wF28
.if \n(87<\n(38 .nr 87 \n(38
.87
.rm 87
.nr 88 0
.nr 38 \w9
.if \n(88<\n(38 .nr 88 \n(38
.nr 38 \wF9
.if \n(88<\n(38 .nr 88 \n(38
.nr 38 \wO
.if \n(88<\n(38 .nr 88 \n(38
.nr 38 \wF19
.if \n(88<\n(38 .nr 88 \n(38
.nr 38 \wL
.if \n(88<\n(38 .nr 88 \n(38
.nr 38 \wF29
.if \n(88<\n(38 .nr 88 \n(38
.88
.rm 88
.nr 89 0
.nr 38 \w0
.if \n(89<\n(38 .nr 89 \n(38
.nr 38 \wF10
.if \n(89<\n(38 .nr 89 \n(38
.nr 38 \wP
.if \n(89<\n(38 .nr 89 \n(38
.nr 38 \wF20
.if \n(89<\n(38 .nr 89 \n(38
.nr 38 \w;
.if \n(89<\n(38 .nr 89 \n(38
.nr 38 \wF30
.if \n(89<\n(38 .nr 89 \n(38
.89
.rm 89
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr 45 \n(84+(3*\n(38)
.nr 85 +\n(45
.nr 46 \n(85+(3*\n(38)
.nr 86 +\n(46
.nr 47 \n(86+(3*\n(38)
.nr 87 +\n(47
.nr 48 \n(87+(3*\n(38)
.nr 88 +\n(48
.nr 49 \n(88+(3*\n(38)
.nr 89 +\n(49
.nr TW \n(89
.if t .if \n(TW>\n(.li .tm Table at line 136 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'2\h'|\n(42u'3\h'|\n(43u'4\h'|\n(44u'5\h'|\n(45u'6\h'|\n(46u'7\h'|\n(47u'8\h'|\n(48u'9\h'|\n(49u'0
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F1\h'|\n(41u'F2\h'|\n(42u'F3\h'|\n(43u'F4\h'|\n(44u'F5\h'|\n(45u'F6\h'|\n(46u'F7\h'|\n(47u'F8\h'|\n(48u'F9\h'|\n(49u'F10
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'\h'|\n(44u'\h'|\n(45u'\h'|\n(46u'\h'|\n(47u'\h'|\n(48u'\h'|\n(49u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Q\h'|\n(41u'W\h'|\n(42u'E\h'|\n(43u'R\h'|\n(44u'T\h'|\n(45u'Y\h'|\n(46u'U\h'|\n(47u'I\h'|\n(48u'O\h'|\n(49u'P
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F11\h'|\n(41u'F12\h'|\n(42u'F13\h'|\n(43u'F14\h'|\n(44u'F15\h'|\n(45u'F16\h'|\n(46u'F17\h'|\n(47u'F18\h'|\n(48u'F19\h'|\n(49u'F20
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'\h'|\n(44u'\h'|\n(45u'\h'|\n(46u'\h'|\n(47u'\h'|\n(48u'\h'|\n(49u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'A\h'|\n(41u'S\h'|\n(42u'D\h'|\n(43u'F\h'|\n(44u'G\h'|\n(45u'H\h'|\n(46u'J\h'|\n(47u'K\h'|\n(48u'L\h'|\n(49u';
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F21\h'|\n(41u'F22\h'|\n(42u'F23\h'|\n(43u'F24\h'|\n(44u'F25\h'|\n(45u'F26\h'|\n(46u'F27\h'|\n(47u'F28\h'|\n(48u'F29\h'|\n(49u'F30
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'\h'|\n(44u'\h'|\n(45u'\h'|\n(46u'\h'|\n(47u'\h'|\n(48u'\h'|\n(49u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Z\h'|\n(41u'X\h'|\n(42u'C\h'|\n(43u'V\h'|\n(44u'B\h'|\n(45u'N\h'|\n(46u'M\h'|\n(47u'\h'|\n(48u'\h'|\n(49u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u \n(86u \n(87u \n(88u \n(89u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F31\h'|\n(41u'F32\h'|\n(42u'F33\h'|\n(43u'F34\h'|\n(44u'F35\h'|\n(45u'F36\h'|\n(46u'F37\h'|\n(47u'\h'|\n(48u'\h'|\n(49u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-20
.LP
The default settings are (key, keystrokes, command, action):
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83
.nr 80 0
.nr 38 \wF1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF5
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF6
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF7
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF8
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF9
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF10
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF11
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF12
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF13
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF14
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF15
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF16
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF17
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF18
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF19
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF20
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF21
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF22
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF23
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF24
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF25
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF26
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF27
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF28
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF29
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wF30
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wESC 1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 2
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 3
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 4
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 5
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 6
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 7
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 8
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 9
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC 0
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC Q
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC W
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC E
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC T
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC Y
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC U
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC I
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC O
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC P
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC A
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC S
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC D
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC F
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC G
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC H
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC J
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC K
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC L
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wESC ;
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \w#+10
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w#+20
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w.
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w#-10
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w#-20
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wprompt
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w//
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wkeep
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wview =
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wview
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wformat p5
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wformat
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wformat p-5
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wformat i5
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wformat i5p-5
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wformat i10
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wformat lp0
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w. format lp0
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w.,. format c
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w.,. format r
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wset t10 ...
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wset t*4
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wset t*5
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wset t*8
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w1
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w$;#-19
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wset x0
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wset x40-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wset x40+
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wfilename
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wforward half of screen height
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wforward by full screen height
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wforward to cursor line
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wbackward half of screen height
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wbackward by full screen height
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcommand prompt or pickup previous
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wscan forward using last pattern
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcheckpoint the file buffer
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wrefresh image
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wtoggle full-screen <--> context
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wletter-style paragraph
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wsquare paragraph
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wouter list item
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wouter list continued
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \winner list item
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \winner list continued
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wtidy paragraph (left-justified)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wtidy to paragraph end (left-j)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcentre cursor line
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wright justify cursor line
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wassembler tabs
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wtabs every 4th column
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wtabs every 5th column
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wtabs every 8th column
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wto first line
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wto last line, at screen bottom
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wleftmost window
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wwindow left 40 columns
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wwindow right 40 columns
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wcursor location, buffer name
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr TW \n(83
.if t .if \n(TW>\n(.li .tm Table at line 174 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F1\h'|\n(41u'ESC 1\h'|\n(42u'#+10\h'|\n(43u'forward half of screen height
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F2\h'|\n(41u'ESC 2\h'|\n(42u'#+20\h'|\n(43u'forward by full screen height
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F3\h'|\n(41u'ESC 3\h'|\n(42u'.\h'|\n(43u'forward to cursor line
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F4\h'|\n(41u'ESC 4\h'|\n(42u'#-10\h'|\n(43u'backward half of screen height
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F5\h'|\n(41u'ESC 5\h'|\n(42u'#-20\h'|\n(43u'backward by full screen height
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F6\h'|\n(41u'ESC 6\h'|\n(42u'prompt\h'|\n(43u'command prompt or pickup previous
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F7\h'|\n(41u'ESC 7\h'|\n(42u'//\h'|\n(43u'scan forward using last pattern
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F8\h'|\n(41u'ESC 8\h'|\n(42u'keep\h'|\n(43u'checkpoint the file buffer
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F9\h'|\n(41u'ESC 9\h'|\n(42u'view =\h'|\n(43u'refresh image
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F10\h'|\n(41u'ESC 0\h'|\n(42u'view\h'|\n(43u'toggle full-screen <--> context
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F11\h'|\n(41u'ESC Q\h'|\n(42u'format p5\h'|\n(43u'letter-style paragraph
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F12\h'|\n(41u'ESC W\h'|\n(42u'format\h'|\n(43u'square paragraph
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F13\h'|\n(41u'ESC E\h'|\n(42u'format p-5\h'|\n(43u'outer list item
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F14\h'|\n(41u'ESC R\h'|\n(42u'format i5\h'|\n(43u'outer list continued
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F15\h'|\n(41u'ESC T\h'|\n(42u'format i5p-5\h'|\n(43u'inner list item
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F16\h'|\n(41u'ESC Y\h'|\n(42u'format i10\h'|\n(43u'inner list continued
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F17\h'|\n(41u'ESC U\h'|\n(42u'format lp0\h'|\n(43u'tidy paragraph (left-justified)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F18\h'|\n(41u'ESC I\h'|\n(42u'. format lp0\h'|\n(43u'tidy to paragraph end (left-j)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F19\h'|\n(41u'ESC O\h'|\n(42u'.,. format c\h'|\n(43u'centre cursor line
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F20\h'|\n(41u'ESC P\h'|\n(42u'.,. format r\h'|\n(43u'right justify cursor line
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F21\h'|\n(41u'ESC A\h'|\n(42u'set t10 ...\h'|\n(43u'assembler tabs
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F22\h'|\n(41u'ESC S\h'|\n(42u'set t*4\h'|\n(43u'tabs every 4th column
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F23\h'|\n(41u'ESC D\h'|\n(42u'set t*5\h'|\n(43u'tabs every 5th column
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F24\h'|\n(41u'ESC F\h'|\n(42u'set t*8\h'|\n(43u'tabs every 8th column
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F25\h'|\n(41u'ESC G\h'|\n(42u'1\h'|\n(43u'to first line
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F26\h'|\n(41u'ESC H\h'|\n(42u'$;#-19\h'|\n(43u'to last line, at screen bottom
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F27\h'|\n(41u'ESC J\h'|\n(42u'set x0\h'|\n(43u'leftmost window
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F28\h'|\n(41u'ESC K\h'|\n(42u'set x40-\h'|\n(43u'window left 40 columns
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F29\h'|\n(41u'ESC L\h'|\n(42u'set x40+\h'|\n(43u'window right 40 columns
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'F30\h'|\n(41u'ESC ;\h'|\n(42u'filename\h'|\n(43u'cursor location, buffer name
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-34
.LP
F31..F37 are initially undefined.  The values of 10 and 20 in the
strings for F1/2/4/5 are for a 24 row screen; they are determined from
the screen height.  Key strings can be redefined using "set".
.SH Profile options in the "set" command
The second column gives the set parameter argument type; the third
column gives the default value.
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83
.nr 80 0
.nr 38 \wa
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wb
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wc
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wd
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \we
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wf
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wh
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wi
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wj
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wl
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wm
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wn
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wo
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wp
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wr
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \ws
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wt
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wu
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wv
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \ww
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wx
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wy
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wz
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnbr/defn/
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wtabdef
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumber
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wswitch
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wa+
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wb-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wc-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wd-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \we-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wh+
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wi+
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wj+
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wl+
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wm2
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wn-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wo0
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wp0
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wr7
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \ws1
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wt*8
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wu-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wv-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \ww70
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wx0
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wy0
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wz+
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wAppend line when RETURN pressed ("a-"=split-line)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wSimple block combine action ("b+"=format-lines)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wCharacters overstrike text ("c+"=insertion)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wDetab buffer text action ("d+"=expand-to-spaces)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wEntab written text action ("e+"=compress-to-tabs)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wFunction key definitions, already listed
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wHoriz tab keys ("h-"=tab-insert-&-previous-word)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAutomatic image updating ("i-"=await-request)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w"format" both margins aligned ("j-"=left-only)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wLoop search action over buffer ("l-"=do-not-loop)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wControls mask allows HT and DEL (0=none, 4=all)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAnycase toggle is off ("n+"=anycase-toggle-on)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w"format" overall text offset
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w"format" default paragraph first line indent
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAlternative rendition is reverse (0=def, 1=bold)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w"format" default sentence separation (0..4)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wSet tabulation stop columns every 8th column
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w"Software Tools" metacharacters ("u+"=UNIX)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wNo automatic view command on entry to editor
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w"format" default paragraph width
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wLeft margin offset in full-screen operation
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wVertical cursor movement wraps (>0=scroll)
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wWord wrap -- "power-typing" -- action ("z-"=off)
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr TW \n(83
.if t .if \n(TW>\n(.li .tm Table at line 208 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'a\h'|\n(41u'switch\h'|\n(42u'a+\h'|\n(43u'Append line when RETURN pressed ("a-"=split-line)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'b\h'|\n(41u'switch\h'|\n(42u'b-\h'|\n(43u'Simple block combine action ("b+"=format-lines)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'c\h'|\n(41u'switch\h'|\n(42u'c-\h'|\n(43u'Characters overstrike text ("c+"=insertion)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'd\h'|\n(41u'switch\h'|\n(42u'd-\h'|\n(43u'Detab buffer text action ("d+"=expand-to-spaces)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'e\h'|\n(41u'switch\h'|\n(42u'e-\h'|\n(43u'Entab written text action ("e+"=compress-to-tabs)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'f\h'|\n(41u'nbr/defn/\h'|\n(42u'\h'|\n(43u'Function key definitions, already listed
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'h\h'|\n(41u'switch\h'|\n(42u'h+\h'|\n(43u'Horiz tab keys ("h-"=tab-insert-&-previous-word)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'i\h'|\n(41u'switch\h'|\n(42u'i+\h'|\n(43u'Automatic image updating ("i-"=await-request)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'j\h'|\n(41u'switch\h'|\n(42u'j+\h'|\n(43u'"format" both margins aligned ("j-"=left-only)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'l\h'|\n(41u'switch\h'|\n(42u'l+\h'|\n(43u'Loop search action over buffer ("l-"=do-not-loop)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'm\h'|\n(41u'number\h'|\n(42u'm2\h'|\n(43u'Controls mask allows HT and DEL (0=none, 4=all)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'n\h'|\n(41u'switch\h'|\n(42u'n-\h'|\n(43u'Anycase toggle is off ("n+"=anycase-toggle-on)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'o\h'|\n(41u'number\h'|\n(42u'o0\h'|\n(43u'"format" overall text offset
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'p\h'|\n(41u'number\h'|\n(42u'p0\h'|\n(43u'"format" default paragraph first line indent
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'r\h'|\n(41u'number\h'|\n(42u'r7\h'|\n(43u'Alternative rendition is reverse (0=def, 1=bold)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u's\h'|\n(41u'number\h'|\n(42u's1\h'|\n(43u'"format" default sentence separation (0..4)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u't\h'|\n(41u'tabdef\h'|\n(42u't*8\h'|\n(43u'Set tabulation stop columns every 8th column
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'u\h'|\n(41u'switch\h'|\n(42u'u-\h'|\n(43u'"Software Tools" metacharacters ("u+"=UNIX)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'v\h'|\n(41u'switch\h'|\n(42u'v-\h'|\n(43u'No automatic view command on entry to editor
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'w\h'|\n(41u'number\h'|\n(42u'w70\h'|\n(43u'"format" default paragraph width
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'x\h'|\n(41u'number\h'|\n(42u'x0\h'|\n(43u'Left margin offset in full-screen operation
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'y\h'|\n(41u'number\h'|\n(42u'y0\h'|\n(43u'Vertical cursor movement wraps (>0=scroll)
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'z\h'|\n(41u'switch\h'|\n(42u'z+\h'|\n(43u'Word wrap -- "power-typing" -- action ("z-"=off)
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-25
.LP
Arguments for "set" are the single letter followed by one of:
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 80 0
.nr 38 \wswitch
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wnumber
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wnbr/defn/
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wtabdef
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w"+" or "-"
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA signed integer.  A "+" or "-" postfix increments
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wor decrements the current value. The result is
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \walways constrained to be legal.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wFor a function key definition; an unsigned integer,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w1..37, followed by delimiter, characters, delimiter.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA list of unsigned integers separated by spaces;
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wor an asterisk (*) followed by a single unsigned
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \winteger giving the regular tab stop interval.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 223 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'switch\h'|\n(41u'"+" or "-"
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'number\h'|\n(41u'A signed integer.  A "+" or "-" postfix increments
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'or decrements the current value. The result is
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'always constrained to be legal.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'nbr/defn/\h'|\n(41u'For a function key definition; an unsigned integer,
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'1..37, followed by delimiter, characters, delimiter.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'tabdef\h'|\n(41u'A list of unsigned integers separated by spaces;
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'or an asterisk (*) followed by a single unsigned
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'integer giving the regular tab stop interval.
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.LP
Examples:
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 80 0
.nr 38 \wenable both tab expansion and compression:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wmake format left-j, width 65, offset 1:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wwork with character insertion, RETURN splits:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wset tab stops at columns 5, 25 and 45:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wincrease the left margin offset by 64:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wset function key 33 (ESC C) to "quit":
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wset d+e+
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wset j-w65o1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wset c+a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wset t5 25 45
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wset x64+
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wset f33/quit/
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 235 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'enable both tab expansion and compression:\h'|\n(41u'set d+e+
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'make format left-j, width 65, offset 1:\h'|\n(41u'set j-w65o1
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'work with character insertion, RETURN splits:\h'|\n(41u'set c+a-
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'set tab stops at columns 5, 25 and 45:\h'|\n(41u'set t5 25 45
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'increase the left margin offset by 64:\h'|\n(41u'set x64+
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'set function key 33 (ESC C) to "quit":\h'|\n(41u'set f33/quit/
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-8
.LP
Use "show" to see the current editor profile.  Non-default profile
settings which will almost always be required can be stored in a
profile file; see below for details.
.SH Files
In the following descriptions,
<uuu> represents your numeric userid,
<ppp> represents the current process id, and
$HOME represents your home directory (taken from password file).
Note that the use of the userid prevents the use of curlew twice
from the same signon id,
however it makes recovery (nearly) automatic after machine or line failure.
.LP
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 80 0
.nr 38 \w$HOME/.curlewrc
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/tmp/cl<ppp>ebuf
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/tmp/cl<ppp>clon
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w/tmp/cl<ppp>safe
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\.cl<uuu>safe
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\.cl<uuu>jrnl
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wcurlew profile file
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wedit buffer
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wsaved block (clone) buffer
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wsafety filename during write command
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wsecond safety file during write command
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wjournal filename
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 257 file curlew.t is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'$HOME/.curlewrc\h'|\n(41u'curlew profile file
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/tmp/cl<ppp>ebuf\h'|\n(41u'edit buffer
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/tmp/cl<ppp>clon\h'|\n(41u'saved block (clone) buffer
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'/tmp/cl<ppp>safe\h'|\n(41u'safety filename during write command
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\.cl<uuu>safe\h'|\n(41u'second safety file during write command
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\.cl<uuu>jrnl\h'|\n(41u'journal filename
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-8
.SH RESTRICTIONS
.LP
Curlew  is  a text editor, suitable for editing natural language text,
program source and data, and similar tasks.  It is  not  suitable  for
files  containing  binary  data  such as executable object programs or
encoded data produced by some applications packages.
.LP
Curlew cannot handle files with more than 16,383 lines of information,
nor  can  it  handle  files  in  which any line contains more than 509
characters.  All versions of Curlew will refuse to "write" a file once
the 16,383 lines limit has been passed.
.LP
Users  of  files  whose  sharing  attributes  allow  concurrent  write
(modify) access by more than one user (or  process)  should  be  aware
that  Curlew  does not lock (reserve) the file for the duration of the
editing session.
.LP
Curlew can only be used once from any given login id.
If it is invoked a second time, no
journalling can take place, so no recovery will be possible,
and the u(undo) command will be useless.
