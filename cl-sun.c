/* Output from p2c, the Pascal-to-C translator */
/* From input file "cl-sun.p" */


#include <p2c/p2c.h>


/* %% Source generation dated :  Wed, 04 May 88  13:37:49 BST  %%%%%%% */


/* ****************************************************** curlew ***** */
/* *                                                                 * */
/* *                           C U R L E W                           * */
/* *                                                                 * */
/* ******************************************************************* */


/* %%%%%% update %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% cl-sun %%%%% */
/* %                                                                 % */
/* %     Updated for the UNIX time-sharing system, release 4.2bsd    % */
/* %     running on a SUN workstation.                               % */
/* %     UNIX is a trademark of A.T.&T. Bell Laboratories, U.S.A.    % */
/* %                                                                 % */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


/* ****************************************************** curlew ***** */
/* *                                                                 * */
/* *   A portable full-screen editor intended for use over a data    * */
/* *   communications network.  The full-screen user interface is    * */
/* *   derived from that of the old NUMAC Screen Editor, "sc".       * */
/* *   The network virtual terminal support is that of the "The      * */
/* *   Simple Screen Management Protocol" (United Kingdom Joint      * */
/* *   Network Team, July 1985).                                     * */
/* *                                                                 * */
/* *   The context editor and file buffer management facilities      * */
/* *   provided were inspired by and are modelled on "edit", by      * */
/* *   B.W.Kernighan and P.J.Plauger.  See the book "Software Tools  * */
/* *   in Pascal" (Addison-Wesley, 1981).                            * */
/* *                                                                 * */
/* *   Screen editor code, SSMP network virtual terminal support,    * */
/* *   and other extensions by Alan Hunter, Computing Laboratory,    * */
/* *   University of Newcastle upon Tyne.                            * */
/* *                                                                 * */
/* *   This is PUBLIC DOMAIN SOFTWARE, by which is meant that it     * */
/* *   may be copied and used by individuals or institutions but     * */
/* *   any rights of commercial exploitation are retained by the     * */
/* *   University of Newcastle upon Tyne.                            * */
/* *                                                                 * */
/* *   CURLEW screen editor and SSMP virtual terminal coding is      * */
/* *     copyright (1987) by the University of Newcastle upon Tyne.  * */
/* *   The original source of the Software Tools Pascal routines is  * */
/* *     copyright (1981) by Bell Telephone Laboratories,            * */
/* *       Incorporated, and Whitesmiths, Ltd.                       * */
/* *                                                                 * */
/* ******************************************************************* */




/* ****************************************************** iso646 ***** */
/* *                                                                 * */
/* *            International Standard 646 character set             * */
/* *                                                                 * */
/* ******************************************************************* */


#define blank           32

/* numerics */
#define dig0            48
#define dig1            49
#define dig2            50
#define dig3            51
#define dig4            52
#define dig5            53
#define dig6            54
#define dig7            55
#define dig8            56
#define dig9            57

/* lower case alphabetics */
#define leta            97
#define letb            98
#define letc            99
#define letd            100
#define lete            101
#define letf            102
#define letg            103
#define leth            104
#define leti            105
#define letj            106
#define letk            107
#define letl            108
#define letm            109
#define letn            110
#define leto            111
#define letp            112
#define letq            113
#define letr            114
#define lets            115
#define lett            116
#define letu            117
#define letv            118
#define letw            119
#define letx            120
#define lety            121
#define letz            122

/* upper case alphabetics */
#define capa            65
#define capb            66
#define capc            67
#define capd            68
#define cape            69
#define capf            70
#define capg            71
#define caph            72
#define capi            73
#define capj            74
#define capk            75
#define capl            76
#define capm            77
#define capn            78
#define capo            79
#define capp            80
#define capq            81
#define capr            82
#define caps            83
#define capt            84
#define capu            85
#define capv            86
#define capw            87
#define capx            88
#define capy            89
#define capz            90

/* other graphic symbols */
#define exclam          33
#define dquote          34
#define sharp           35
#define dollar          36
#define percent         37
#define amper           38
#define squote          39
#define lparen          40
#define rparen          41
#define star            42
#define plus            43
#define comma           44
#define minus           45
#define period          46
#define slash           47
#define colon           58
#define semicol         59
#define less            60
#define equals          61
#define greater         62
#define question        63
#define atsign          64
#define lbrack          91
#define backslash       92
#define rbrack          93
#define cflex           94
#define uscore          95
#define grave           96
#define lbrace          123
#define bar             124
#define rbrace          125
#define tilde           126

/* control characters */
#define nul             0
#define ctla            1
#define ctlb            2
#define ctlc            3
#define ctld            4
#define ctle            5
#define ctlf            6
#define ctlg            7
#define ctlh            8
#define ctli            9
#define ctlj            10
#define ctlk            11
#define ctll            12
#define ctlm            13
#define ctln            14
#define ctlo            15
#define ctlp            16
#define ctlq            17
#define ctlr            18
#define ctls            19
#define ctlt            20
#define ctlu            21
#define ctlv            22
#define ctlw            23
#define ctlx            24
#define ctly            25
#define ctlz            26
#define esc             27
#define fs              28
#define gs              29
#define rs              30
#define us              31
#define del             127


/* ****************************************************** gblcons **** */
/* *                                                                 * */
/* *              Software Tools :: Global constants                 * */
/* *                                                                 * */
/* ******************************************************************* */


#define isomax          255   /* ISO 4873 (646 is a subset) max value */

#define maxstr          512   /* maximum characters for chstring type */
#define endstr          nul   /* end of string marked with nul */
#define newline         ctlj   /* end of line marked with line feed */
#define tab             ctli   /* tab marked with horizontal tab */
#define backspace       ctlh   /* back space output device character */
#define endfile         ctly   /* end of file marked with em */

#define maxpat          maxstr   /* maximum characters in a pattern */
#define closize         1   /* size of a closure entry */

#define nccl            exclam   /* negated class is "!" (can't be negate) */
#define litchar         plus   /* literal character flagged by "+" */
#define mkditto         ctlz   /* ditto marker in string is sub */

#define ioerror_        (-1)   /* file descriptor not available indicator */
#define iomaxfd         19   /* maximum available stream number */

#define ioread          1   /* open mode for reading */
#define iowrite         2   /* open mode for writing */
#define ioappend        3   /* open mode for writing, append to end */

#define iomddeflt       0   /* normal file descriptor state */
#define iomdasis        1   /* "as is" : packets ready to write */
#define iomdmsg         2   /* "message" : no native mode networking */

#define maxarg          10   /* maximum number of command arguments */

#define idef            0   /* o/s default interrupt processing */
#define imask           1   /* mask interrupts -> just continue */
#define ikill           2   /* interrupt -> kill application */

/* following are for use with "getinf" */
#define sfnpfx          1   /* scratch filename  prefix */
#define cmdpars         2   /* command parameter string */
#define timedate        3   /* time and date, #822 format */
#define username        4   /* username */
#define maxframe        5   /* frames per packet in SSMP, nul -> 2 */
#define sortdate        6   /* time and date, sortable format */
#define gspare2         7   /* (not in use) */
#define litfchar        8   /* literal file initial character */
#define ffcrtn          9   /* "y" means "ffcopy" routine available */
#define ixcrtn          10   /* "y" means "ixcopy" routine available */


/* ****************************************************** ssmpcons *** */
/* *                                                                 * */
/* *              SSMP shared data structure constants               * */
/* *                                                                 * */
/* ******************************************************************* */


#define zmaxrow         59   /* allows a maximum of 60 rows */
#define zmaxcol         131   /* allows a maximum of 132 columns */
#define isospace        blank   /* to fit with The Book */
#define zfieldlimit     239   /* allows a maximum of 240 fields */

/* mode array */
#define tlevel          0
#define tmaxrow         1
#define tmaxcol         2
#define dsinvalid       3
#define notify          4
#define selectgr        5
#define reqshift        6
#define cursor          7
#define icharmode       8
#define ilinerow        9
#define intsignal       10
#define kinthost        11
#define ksuspend        12
#define krestart        13
#define kenter          14
#define kcsrup          15
#define kcsrdown        16
#define kcsrleft        17
#define kcsrright       18
#define knexttab        19
#define kprevtab        20
#define kleftupd        21
#define kfirstns        22
#define kalastns        23
#define kinsmode        24
#define keraright       25
#define kinsspac        26
#define kdelchar        27
#define keraprev        28
#define kinsline        29
#define kdelline        30
#define keraline        31
#define kappline        32
#define ksplline        33
#define knextfld        34
#define kprevfld        35
#define khomefld        36
#define knewline        37


/* ****************************************************** ssmpcons *** */
/* *                                                                 * */
/* *        SSMP network encoding and transmission constants         * */
/* *                                                                 * */
/* ******************************************************************* */


#define defrend         0   /* selectgr : default rendition */
#define bold            1   /* selectgr : bold, or increased intensity */
#define faint           2   /* selectgr : faint, or reduced intensity */
#define italicised      3   /* selectgr : italicised, or slanted */
#define underlined      4   /* selectgr : underlined */
#define slowblink       5
    /* selectgr : slowly blinking (less than 150/min) */
#define rapidblink      6
    /* selectgr : rapidly blinking (150/min or more) */
#define negative        7   /* selectgr : negative image (reverse video) */
#define concealed       8   /* selectgr : concealed (displayed as SPACEs) */
#define crossedout      9   /* selectgr : crossed out (marked for deletion) */

#define maxmode         knewline   /* maximum allocated mode index */
#define framesize       60   /* characters per SSMP frame output */




/* ****************************************************** gbltype **** */
/* *                                                                 * */
/* *          Software Tools :: Global type declarations             * */
/* *                                                                 * */
/* ******************************************************************* */


typedef uchar chstring[maxstr];
typedef boolean tablist[maxstr];
typedef uchar itable[256];
typedef Char chtable[256];
typedef Char textlit[24];


/* ****************************************************** lunixtyp *** */
/* *                                                                 * */
/* *     SSMP shared data structure types -- local UNIX version      * */
/* *                                                                 * */
/* ******************************************************************* */


typedef enum {
  withhost, withterm
} toktype;
/* in use, 0..maxrow */
/* in use, 0..maxcol */
typedef uchar imtype[zmaxrow + 1][zmaxcol + 1];

typedef struct savedfield {
  char fldtop, fldbottom;   /* stored update limit rows */
  uchar fldleft;
  /* stored update limit cols */
  uchar fldright;
} savedfield;   /* each stored field definition */

/* in use, 0..((maxrow+1)*4-1) */
/* 37 is highest defined index */
/* parameter range */
typedef uchar modearray[64];   /* terminal emulation status */
typedef enum {
  notab, tabset
} tabstop;   /* tabulation stop */


/* ****************************************************** lssmptyp *** */
/* *                                                                 * */
/* *          SSMP network encoding and transmission types           * */
/* *                                                                 * */
/* ******************************************************************* */


typedef uchar charstr[16];
typedef uchar netbuff[256];
typedef uchar primpar[4];
typedef uchar rowtext[zmaxcol + 1];
typedef enum {
  nocf, csrpend, fldpend
} ncftype;




/* ****************************************************** gblvar ***** */
/* *                                                                 * */
/* *         Software Tools :: Global variable declarations          * */
/* *                                                                 * */
/* ******************************************************************* */


Static long envlocn;   /* environment location */

Static itable littoiso;   /* native to iso code conversions */
Static chtable isotolit;   /* iso to native code conversions */

Static short stdin_;   /* standard input file descriptor */
Static short stdout_;   /* standard output file descriptor */
Static short stderror;   /* standard error file descriptor */

Static short initin, initout;   /* initial standard input & output */

Static chstring cmdline;   /* command argument text */
Static long nbrcmdargs;   /* number of command arguments */
Static long cmdargidx[maxarg + 1];   /* argument offsets */

Static uchar scan;   /* forward scan character */
Static uchar backscan;   /* backward scan character */

Static uchar escape;   /* escape special meanings */
Static uchar closure;   /* closure */
Static uchar bol;   /* begining of line */
Static uchar eol;   /* end of line */
Static uchar any;   /* wildcard */
Static uchar ccl;   /* character class start */
Static uchar cclend;   /* ... and end */
Static uchar negate;   /* class negation */
Static uchar anycase;   /* case sensitivity toggle */
Static uchar ditto;   /* ditto character */

Static boolean cssinit;   /* pattern start anycase state */


/* ****************************************************** lunixvar *** */
/* *                                                                 * */
/* *   SSMP shared data structure variables -- local UNIX version    * */
/* *                                                                 * */
/* ******************************************************************* */


Static toktype token;   /* access control token */
Static char row;   /* cursor row */
Static uchar col;   /* cursor column */

Static imtype image;   /* screen image */

Static uchar curfield, maxfield;
Static savedfield field[zfieldlimit + 1];   /* stored field definitions */

Static char boxtop, boxbottom;   /* update limits - rows */
Static uchar boxleft, boxright;   /* update limits - columns */
Static modearray mode;   /* terminal emulation status */
Static tabstop tabs[zmaxcol + 1];   /* tabulation stops array */


/* ****************************************************** lssmpvar *** */
/* *                                                                 * */
/* *           SSMP local action virtual terminal variables          * */
/* *                                                                 * */
/* ******************************************************************* */


Static long ssmplocn;   /* network environment pointer */
Static uchar masterlevel;   /* session level after negotiation */
Static boolean repainted;   /* true if screen refreshed */
Static ncftype cfstate;   /* setcursor/setfield optimisation */
Static boolean escaped;   /* true if previous keystroke was ESC */
Static boolean defer;   /* true to stop t-?s changing image */

Static long nqueued;   /* number of t-primitives on queue */
Static uchar qpcode[8];   /* primitive q : code */
Static uchar qpar1[8];   /* primitive q : par one */
Static uchar qpar2[8];   /* primitive q : par two */

Static itable filter;   /* strip parity table */


/* ****************************************************** stprim ***** */
/* *                                                                 * */
/* *         Software Tools :: Primitives external references        * */
/* *                                                                 * */
/* ******************************************************************* */


/* %% external references converted for 4.2bsd Pascal (SUN) */


/* SGETSTE -- initialise software tools external interface */
extern Void SGETSTE PP((long *envlocn, uchar *littoiso, Char *isotolit));


/* SRELSTE -- release resources acquired by external interface */
extern Void SRELSTE PP((long envlocn, long action));


/* SINTRPT -- set attention interrupt (break) processing */
extern Void SINTRPT PP((long envlocn, long reqcode));


/* SOPEN -- open a file */
extern Void SOPEN PP((long envlocn, uchar *name, long mode, short *fd));


/* SCREATE -- open a file, creating if necessary */
extern Void SCREATE PP((long envlocn, uchar *name, long mode, short *fd));


/* SIOCTRL -- set state of opened file descriptor */
extern Void SIOCTRL PP((long envlocn, long newstate, int fd));


/* SSECURE -- secure disk copy of opened file without closing it */
extern Void SSECURE PP((long envlocn, int fd));


/* SREWIND -- rewind (read) or reset (write, append) file desc */
extern Void SREWIND PP((long envlocn, int fd, long *rc));


/* SCLOSE -- close an open file */
extern Void SCLOSE PP((long envlocn, int fd));


/* SREMOVE -- remove (destroy) a file */
extern Void SREMOVE PP((long envlocn, uchar *name, long *rc));


/* SRENAME -- change the name of a file */
extern Void SRENAME PP((long envlocn, uchar *oldname, uchar *newname,
			long *rc));


/* SGETLIN -- read from fd until newline */
extern Void SGETLIN PP((long envlocn, uchar *dest, int fd, long maxlen,
			long *rc));


/* SGETCF -- read one character from fd */
extern Void SGETCF PP((long envlocn, uchar *c, int fd));


/* SPUTSTR -- write string to fd */
extern Void SPUTSTR PP((long envlocn, uchar *src, int fd));


/* SPUTCF -- write character to fd */
extern Void SPUTCF PP((long envlocn, int c, int fd));


/* SFFCOPY -- copy from one file descriptor to another */
extern Void SFFCOPY PP((long envlocn, int fdin, int fdout, long *nrecds,
			long *rc));


/* SIXCOPY -- copy specified record from one f/d to another */
extern Void SIXCOPY PP((long envlocn, long fdx, int fdin, int fdout, long *rc));


/* SGETIDX -- return next get/put i/o pointer for future get */
extern Void SGETIDX PP((long envlocn, long *fdx, int fd, long *rc));


/* SSEEK -- set next get pointer for fd */
extern Void SSEEK PP((long envlocn, long fdx, int fd));


/* SITOXPC -- internal to external file pointer conversion */
extern Void SITOXPC PP((long envlocn, uchar *str, long sidx, long fdx));


/* SXTOIPC -- external to internal file pointer conversion */
extern Void SXTOIPC PP((long envlocn, uchar *str, long sidx, long *fdx));


/* SGETFDS -- get file descriptor status */
extern Void SGETFDS PP((long envlocn, long *fdstat, int fd));


/* SPROMPT -- set standard input prompt string */
extern Void SPROMPT PP((long envlocn, uchar *pstr));


/* SGETINF -- return information according to index */
extern Void SGETINF PP((long envlocn, long index, uchar *info, long maxlen));


/* SSYSCMD -- execute command line */
extern Void SSYSCMD PP((long envlocn, uchar *cmd, long *rc));


/* SMAILER -- interface to e-mail spooler */
extern Void SMAILER PP((long envlocn, long index, uchar *info, long *rc));


/* STWAIT -- wait for a given elapsed time interval */
extern Void STWAIT PP((long envlocn, long waittime));


/* SLOGUSE -- log use of software tools application */
extern Void SLOGUSE PP((long envlocn, uchar *name));


/* ****************************************************** stprim ***** */
/* *                                                                 * */
/* *           Software Tools :: Initialisation routines             * */
/* *                                                                 * */
/* ******************************************************************* */


/* setctk  -- initialise pattern matching chicken track characters */
Static Void setctk(alternate)
boolean alternate;
{
  /* set up Software Tools values */
  scan = slash;   /* forward scan is "/" */
  backscan = backslash;   /* backward scan is "\" */
  escape = atsign;   /* escape special meanings with "@" */
  closure = star;   /* closure signalled by "*" */
  bol = percent;   /* begining of line by "%" */
  eol = dollar;   /* end of line by "$" */
  any = question;   /* wildcard by "?" */
  ccl = lbrack;   /* character class starts with "[" */
  cclend = rbrack;   /* ... and ends with "]" */
  negate = cflex;   /* class negation signalled by "^" */
  anycase = tilde;   /* case sensitivity toggle is "~" */
  ditto = amper;   /* ditto character is "&" */
  cssinit = true;   /* case sensitive scan default */

  /* check for UNIX (tm) values preferred */
  if (!alternate)
    return;
  backscan = question;   /* backward scan is "?" */
  escape = backslash;   /* escape special meanings with "\" */
  bol = cflex;   /* begining of line by "^" */
  any = period;   /* wildcard by "." */
  anycase = tilde;   /* case sensitivity toggle is "~" */
  ditto = amper;   /* ditto character is "&" */
}  /*setctk*/


/* setint  -- select interrupt (BREAK key) processing */
Static Void setint(intcode)
long intcode;
{
  SINTRPT(envlocn, intcode);   /* 0 = default, 1 = mask, 2 = kill exec */
}  /*setint*/


/* initsoft -- initialise software tools environment */
Static Void initsoft(intcode)
long intcode;
{
  /* initialise software tools environment, get translate tables */
  SGETSTE(&envlocn, littoiso, isotolit);
  if (intcode > 0)   /* 1 = mask, 2 = kill exec */
    setint(intcode);

  stdin_ = 0;   /* standard input */
  stdout_ = 1;   /* standard output */
  stderror = 2;   /* standard error */

  initin = stdin_;
  initout = stdout_;   /* save values */

  nbrcmdargs = 0;   /* no command arguments yet */
  cmdargidx[1] = 1;   /* ... keeps some compilers happy */
  /* set up chicken tracks */
  setctk(false);
}  /*initsoft*/


/* closesoft -- release resources for software tools environment */
Static Void closesoft()
{   /* return after clean up */
  SRELSTE(envlocn, 0L);
}  /*closesoft*/


/* errorexit -- emergency, stop program now */
Static Void errorexit()
{   /* don't return here ... */
  SRELSTE(envlocn, 1L);
}  /*errorexit*/


/* ****************************************************** stprim ***** */
/* *                                                                 * */
/* *                 Software Tools :: Primitives                    * */
/* *                                                                 * */
/* ******************************************************************* */


/* %% next routine name changed from "open" to avoid symbol clash */

/* openf -- open a file for reading or writing */
Static short openf(name, openmode)
uchar *name;
long openmode;
{
  short fd;

  SOPEN(envlocn, name, openmode, &fd);
  return fd;
}  /*openf*/


/* create -- open a file for reading or writing, create if necessary */
Static short create(name, openmode)
uchar *name;
long openmode;
{
  short fd;

  SCREATE(envlocn, name, openmode, &fd);
  return fd;
}  /*create*/


/* iocontrol -- set state of opened file descriptor */
Static Void iocontrol(newstate, fd)
long newstate;
short fd;
{
  SIOCTRL(envlocn, newstate, fd);
}  /*iocontrol*/


/* secure -- secure disk copy of opened file without closing it */
Static Void secure(fd)
short fd;
{
  SSECURE(envlocn, fd);
}  /*secure*/


/* %% next routine name changed from "rewind" to avoid symbol clash */

/* rewindf -- rewind (read) or reset (write, append) file desc */
Static boolean rewindf(fd)
short fd;
{
  long rc;

  SREWIND(envlocn, fd, &rc);
  return (rc == 0);
}  /*rewindf*/


/* %% next routine name changed from "close" to avoid symbol clash */

/* closef -- close a stream after input or output */
Static Void closef(fd)
short fd;
{
  SCLOSE(envlocn, fd);
}  /*closef*/


/* remove -- remove (destroy) file */
Static boolean remove(name)
uchar *name;
{
  long rc;

  SREMOVE(envlocn, name, &rc);
  return (rc == 0);
}  /*remove*/


/* %% next routine name changed from "rename" to avoid symbol clash */

/* renamf -- change name of file oldname to newname */
Static boolean renamf(oldname, newname)
uchar *oldname, *newname;
{
  long rc;

  SRENAME(envlocn, oldname, newname, &rc);
  return (rc == 0);
}  /*renamf*/


/* getline -- read one complete input record from stream fd */
Static boolean getline(dest, fd, maxlen)
uchar *dest;
short fd;
long maxlen;
{
  long rc;

  SGETLIN(envlocn, dest, fd, maxlen, &rc);
  return (rc == 0);
}  /*getline*/


/* getcf -- read one character from stream fd */
Static uchar getcf(c, fd)
uchar *c;
short fd;
{
  SGETCF(envlocn, c, fd);
  return (*c);
}  /*getcf*/


/* putstr -- write string src to file descriptor fd */
Static Void putstr(src, fd)
uchar *src;
short fd;
{
  SPUTSTR(envlocn, src, fd);
}  /*putstr*/


/* putcf -- put character c to file descriptor fd */
Static Void putcf(c, fd)
uchar c;
short fd;
{
  SPUTCF(envlocn, c, fd);
}  /*putcf*/


/* ffcopy -- copy from one file descriptor to another */
Static boolean ffcopy(fdin, fdout, nrecds)
short fdin, fdout;
long *nrecds;
{
  long rc;

  SFFCOPY(envlocn, fdin, fdout, nrecds, &rc);
  if (rc < 0)   /* truncation flag */
    *nrecds = -*nrecds;
  return (rc <= 0);
}  /*ffcopy*/


/* ixcopy -- copy specified record from one f/d to another */
Static boolean ixcopy(fdx, fdin, fdout)
long fdx;
short fdin, fdout;
{
  long rc;

  SIXCOPY(envlocn, fdx, fdin, fdout, &rc);
  return (rc == 0);
}  /*ixcopy*/


/* getindex -- returns next get/put index for future seek */
Static boolean getindex(fdidx, fd)
long *fdidx;
short fd;
{
  long rc;

  SGETIDX(envlocn, fdidx, fd, &rc);
  return (rc == 0);
}  /*getindex*/


/* seek -- set next read pointer for fd */
Static Void seek(fdidx, fd)
long fdidx;
short fd;
{
  SSEEK(envlocn, fdidx, fd);
}  /*seek*/


/* itoxpc -- internal to external file pointer conversion */
Static Void itoxpc(str, sidx, fdx)
uchar *str;
long sidx, fdx;
{
  SITOXPC(envlocn, str, sidx, fdx);
}  /*itoxpc*/


/* xtoipc -- external to internal file pointer conversion */
Static Void xtoipc(str, sidx, fdx)
uchar *str;
long sidx, *fdx;
{
  SXTOIPC(envlocn, str, sidx, fdx);
}  /*xtoipc*/


/* getfds -- get file descriptor status */
Static Void getfds(fdstat, fd)
long *fdstat;
short fd;
{
  SGETFDS(envlocn, fdstat, fd);
}  /*getfds*/


/* prompt -- set standard input prompt string */
Static Void prompt(pstr)
uchar *pstr;
{
  SPROMPT(envlocn, pstr);
}  /*prompt*/


/* getinf -- return information according to index */
Static Void getinf(index, info, maxlen)
long index;
uchar *info;
long maxlen;
{
  SGETINF(envlocn, index, info, maxlen);
}  /*getinf*/


/* syscmd -- execute command line */
Static Void syscmd(newcmd, rc)
uchar *newcmd;
long *rc;
{
  SSYSCMD(envlocn, newcmd, rc);
}  /*syscmd*/


/* mailer -- interface to e-mail spooler */
Static boolean mailer(index, info)
long index;
uchar *info;
{
  long rc;

  SMAILER(envlocn, index, info, &rc);
  return (rc == 0);
}  /*mailer*/


/* timewait -- wait for a given elapsed time interval */
Static Void timewait(waittime)
long waittime;
{
  STWAIT(envlocn, waittime);
}  /*timewait*/


/* loguse -- log use of software tools application */
Static Void loguse(name)
uchar *name;
{
  SLOGUSE(envlocn, name);
}  /*loguse*/


/* ****************************************************** unixprim *** */
/* *                                                                 * */
/* *   Terminal handling primitives for local SSMP -- UNIX version   * */
/* *                                                                 * */
/* ******************************************************************* */

/* %% external references converted for 4.2bsd Pascal (SUN) */



/* UINITSCRN -- initialise screen and any associated data structures */
extern Void UINITSCRN PP((uchar (*image)[zmaxcol + 1], uchar *mode));


/* UENDSCRN -- tidy up for end of session */
extern Void UENDSCRN PV();


/* USETCURSOR -- set cursor to specified row & col */
extern Void USETCURSOR PP((int row, int col));


/* UERASEROW -- erase specified row, over column range */
extern Void UERASEROW PP((int row, int cola, int colb));


/* UROWCOPY -- copy rowa to rowb over column range */
extern Void UROWCOPY PP((int rowa, int rowb, int cola, int colb));


/* URIGHTSHIFT -- shift row right num characters over column range */
extern Void URIGHTSHIFT PP((int row, int cola, int colb, long num));


/* ULEFTSHIFT -- shift row left num characters over column range */
extern Void ULEFTSHIFT PP((int row, int cola, int colb, long num));


/* UCHAR -- output character at current cursor position */
extern Void UCHAR PP((int ch));


/* UALARM -- sound the terminal's alarm bell */
extern Void UALARM PV();


/* UREAD -- read next character from keyboard */
extern Void UREAD PP((uchar *inchar));


/* USETMODE -- tell keyboard drivers about mode array changes */
extern Void USETMODE PP((int mode, long *rc));


/* UGETSIZE -- get screen sizes */
extern Void UGETSIZE PP((char *row, uchar *col));


/* UTOKEN -- show token state ("host" light) */
extern Void UTOKEN PP((long flag));


/* USCROLLUP -- scroll region up */
extern Void USCROLLUP PP((int rowa, int rowb, int cola, int colb, long count));


/* USCROLLDOWN -- scrollregion down */
extern Void USCROLLDOWN PP((int rowa, int rowb, int cola, int colb,
			    long count));


/* UINTHOST -- interrupt the m/c (this task) */
extern Void UINTHOST PV();


/* ****************************************************** lunixrtn *** */
/* *                                                                 * */
/* *   SSMP :: UNIX local action virtual terminal service routines   * */
/* *                                                                 * */
/* ******************************************************************* */


/* ****************************************************** lssmprtn *** */
/* *                                                                 * */
/* *   All SSMP service procedures are defined here.  If this        * */
/* *   section is split into a separate module, the procedures and   * */
/* *   functions listed in the following explanatory text will       * */
/* *   need to be exported.   Variables defined as "Shared data      * */
/* *   structure" or "Network encoding and transmission" are         * */
/* *   required by these SSMP routines only.                         * */
/* *                                                                 * */
/* ******************************************************************* */


/* ****************************************************** lssmprtn *** */
/* *                                                                 * */
/* *         General description of the SSMP host routines           * */
/* *         ---------------------------------------------           * */
/* *                                                                 * */
/* *   The following procedures service requests to generate the     * */
/* *   equivalent host primitives.  With the exception of            * */
/* *   "reqtoken", they should only be called whilst the token is    * */
/* *   with the host application.                                    * */
/* *                                                                 * */
/* *   Note that optimisation of H-SETCURSOR and H-SETFIELD is       * */
/* *   performed automatically.  Only essential instances will be    * */
/* *   generated.                                                    * */
/* *                                                                 * */
/* *      textchar     : H-CHARACTER to send text                    * */
/* *      sendtoken    : H-TOKEN to transfer token                   * */
/* *      setcursor    : H-SETCURSOR to set the cursor position      * */
/* *      setmode      : H-SETMODE to set a mode array element value * */
/* *      erasetoright : H-ERASETORIGHT to erase to end of row       * */
/* *      insertspace  : H-INSERTSPACE to insert SPACE characters    * */
/* *      deletechar   : H-DELETECHAR to delete characters           * */
/* *      soundalarm   : H-SOUNDALARM to sound the audible alarm     * */
/* *      scrollup     : H-SCROLLUP to scroll rows up                * */
/* *      scrolldown   : H-SCROLLDOWN to scroll rows down            * */
/* *      erasedisplay : H-ERASEDISPLAY to erase screen image        * */
/* *      erasefields  : H-ERASEFIELDS to erase all field defns      * */
/* *      setfield     : H-SETFIELD to select new field definition   * */
/* *      setupdate    : H-SETUPDATE to set update limits for field  * */
/* *      erasetabs    : H-ERASETABS to erase all tabulation stops   * */
/* *      settab       : H-SETTAB to set one tabulation stop         * */
/* *      session      : H-SESSION to enter or exit session          * */
/* *      reqtoken     : H-REQTOKEN to request early return of token * */
/* *                                                                 * */
/* *   The following procedures allow interrogation of the shared    * */
/* *   data structure variables.  Use of functions and procedures    * */
/* *   for this purpose hides the data from the application so that  * */
/* *   all assignments are under the control of procedures in the    * */
/* *   SSMP module and can be vetted.  The data returned for each    * */
/* *   procedure is:                                                 * */
/* *                                                                 * */
/* *      maxrow       : session maximum row value                   * */
/* *      maxcol       : session maximum column value                * */
/* *      fieldlimit   : session maximum field index                 * */
/* *      getloc       : current cursor position row and column      * */
/* *      getfield     : current field index                         * */
/* *      getmode      : a copy of the mode array                    * */
/* *      getrow       : the text of the requested row               * */
/* *                                                                 * */
/* *   The next two procedures are to allow convenient replacement   * */
/* *   of shared data structure values.                              * */
/* *                                                                 * */
/* *      putmode      : makes mode array conform to that supplied   * */
/* *      putrow       : replaces the text for the specified row     * */
/* *                                                                 * */
/* *   Note that "putmode" will issue only those H-SETMODEs required * */
/* *   to correct the mode array.  A convenient way of quickly       * */
/* *   switching terminal operating modes is to keep a suitable copy * */
/* *   of the mode array for each set required and use "putmode" to  * */
/* *   establish that state from any other.                          * */
/* *                                                                 * */
/* *   The second procedure, "putrow", will optimise generation of   * */
/* *   the required host primitives.  It will use H-SETCURSOR to     * */
/* *   skip ten or more consecutive blanks where this is possible    * */
/* *   (that is, either default rendition set or mode[reqshift]=1).  * */
/* *                                                                 * */
/* *   If the application requires to refresh the terminal copy of   * */
/* *   the shared data structure, the following procedure will do    * */
/* *   this.  Note that since the graphic rendition of each          * */
/* *   character is not held in the data structure, any highlighting * */
/* *   will be lost and must be separately re-established if         * */
/* *   required.                                                     * */
/* *                                                                 * */
/* *      refresh      : refreshes the whole shared data structure   * */
/* *                                                                 * */
/* *   The following procedures wrap up the session entry            * */
/* *   negotiation and the recommended generation of primitives at   * */
/* *   session close.                                                * */
/* *                                                                 * */
/* *      startssmp    : negotiate session start with terminal       * */
/* *      stopssmp     : tidy up and close down session              * */
/* *                                                                 * */
/* *   The key to use of the whole SSMP module is the last           * */
/* *   primitive:                                                    * */
/* *                                                                 * */
/* *      getprimitive : process input until primitive decoded       * */
/* *                                                                 * */
/* *   This function should be repeatedly called to fetch each       * */
/* *   primitive from the terminal.  Input frames will be fetched    * */
/* *   automatically as required.  Each call returns exactly one     * */
/* *   terminal-generated primitive decoded into the primitive       * */
/* *   identifier and parameters.  The primitive may be processed as * */
/* *   appropriate.  Only T-TOKEN must be processed, and only in     * */
/* *   response to this primitive may host primitives other than     * */
/* *   H-REQTOKEN be generated.                                      * */
/* *                                                                 * */
/* *   When each primitive is presented by "getprimitive" the host   * */
/* *   copy of the shared data structure will already have been      * */
/* *   updated.  The parser calls the relevant T-primitive semantic  * */
/* *   routine to perform this action before returning control.      * */
/* *   Calls to "getloc", "getfield", "getmode" and "getrow" at this * */
/* *   point will therefore return correct values.                   * */
/* *                                                                 * */
/* *   The semantic routine called on recognition of T-TOKEN checks  * */
/* *   mode[TLEVEL] and mode[DSINVALID] for values consistent with   * */
/* *   the current SSMP session.  If they are not consistent, the    * */
/* *   values are corrected and "refresh" is called to correct the   * */
/* *   terminal.  On being presented with T-TOKEN, the application   * */
/* *   is freed from the necessity of checking session integrity and * */
/* *   may act on the request code as appropriate.                   * */
/* *                                                                 * */
/* *   The master session level will be taken from the first         * */
/* *   T-SETMODE setting it.  Note that it is the responsibility of  * */
/* *   the host application calling "getprimitive" to check the      * */
/* *   screen dimensions offered.  This should be done at the first  * */
/* *   T-TOKEN; if the values are unsuitable a suitable text message * */
/* *   should be output and "stopssmp" called.                       * */
/* *                                                                 * */
/* ******************************************************************* */


/* initcoding -- initialise tables for SSMP primitive processing */
Static Void initcoding()
{
  uchar j;

  /* i/o system and session initialisation */
  masterlevel = 0;
  cfstate = nocf;
  repainted = false;
  escaped = false;
  defer = false;
  nqueued = 0;

  /* input filter table to strip parity */
  for (j = 0; j <= 127; j++) {
    filter[j] = j;
    filter[j + 128] = j;
  }
}  /*initcoding*/


/* initshared -- initialise our copy of the shared data structure */
Static Void initshared()
{
  uchar fidx;
  char trow;
  uchar tcol;
  char midx;
  savedfield *WITH;

  token = withhost;
  row = 0;
  col = 0;

  /* stored field definitions */
  for (fidx = 0; fidx <= zfieldlimit; fidx++) {
    WITH = &field[fidx];
    WITH->fldtop = 0;
    WITH->fldbottom = 0;
    WITH->fldleft = 0;
    WITH->fldright = 0;
  }
  curfield = 0;
  maxfield = 0;

  /* update limits */
  boxtop = 0;
  boxbottom = 0;
  boxleft = 0;
  boxright = 0;

  /* mode array */
  for (midx = 0; midx <= 63; midx++)
    mode[midx] = 0;

  /* tabulation stops array*/
  for (tcol = 0; tcol <= zmaxcol; tcol++)
    tabs[tcol] = notab;

  /* not true ssmp, but assign all elements of the screen array
     to avoid upsetting Pascal run time checking routines */
  for (trow = 0; trow <= zmaxrow; trow++) {
    for (tcol = 0; tcol <= zmaxcol; tcol++)
      image[trow][tcol] = grave;
  }
}  /*initshared*/


/* cftest -- issues pending H-SETCURSOR or H-SETFIELD as necessary */
Static Void cftest(newcfstate)
ncftype newcfstate;
{
  if (newcfstate == cfstate)
    return;
  switch (cfstate) {

  case nocf:
    break;

  case csrpend:
    USETCURSOR(row, col);
    break;

  case fldpend:
    USETCURSOR(row, col);
    break;

  }/*case*/
  cfstate = newcfstate;
}  /*cftest*/


/* maxrow -- returns current session maximum row value */
Static char maxrow()
{
  return (mode[tmaxrow]);
}  /*maxrow*/


/* maxcol -- returns current session maximum column value */
Static uchar maxcol()
{
  return (mode[tmaxcol]);
}  /*maxcol*/


/* fieldlimit -- returns current session maximum field index */
Static uchar fieldlimit()
{
  return ((maxrow() + 1) * 4 - 1);
}  /*fieldlimit*/


/* eraserow -- erase part of one row to SPACE characters */
Static Void eraserow(rowa, cola, colb)
char rowa;
uchar cola, colb;
{
  uchar ctemp;

  for (ctemp = cola; ctemp <= colb; ctemp++)
    image[rowa][ctemp] = isospace;
  UERASEROW(rowa, cola, colb);
  USETCURSOR(row, col);
}  /*eraserow*/


/* erasebox -- erase a rectangle of positions to SPACE characters */
Static Void erasebox(rowa, rowb, cola, colb)
char rowa, rowb;
uchar cola, colb;
{
  char rtemp;

  for (rtemp = rowa; rtemp <= rowb; rtemp++)
    eraserow(rtemp, cola, colb);
}  /*erasebox*/


/* rowcopy -- copy part of row to same column range of another row */
Static Void rowcopy(rowa, rowb, cola, colb)
char rowa, rowb;
uchar cola, colb;
{
  uchar ctemp;

  for (ctemp = cola; ctemp <= colb; ctemp++)
    image[rowb][ctemp] = image[rowa][ctemp];
  UROWCOPY(rowa, rowb, cola, colb);
  USETCURSOR(row, col);
}  /*rowcopy*/


/* rightshift -- shift row characters to right within column range */
Static Void rightshift(rowa, cola, colb, ncols)
char rowa;
uchar cola, colb;
uchar ncols;
{
  uchar ctemp;

  if (colb - cola < ncols)
    eraserow(rowa, cola, colb);
  else {
    URIGHTSHIFT(rowa, cola, colb, (long)ncols);
    for (ctemp = colb - ncols; ctemp >= cola; ctemp--)
      image[rowa][ctemp + ncols] = image[rowa][ctemp];
    for (ctemp = cola; ctemp < cola + ncols; ctemp++)
      image[rowa][ctemp] = isospace;
  }
  USETCURSOR(row, col);
}  /*rightshift*/


/* leftshift -- shift row characters to left within column range */
Static Void leftshift(rowa, cola, colb, ncols)
char rowa;
uchar cola, colb;
uchar ncols;
{
  uchar ctemp;

  if (colb - cola < ncols)
    eraserow(rowa, cola, colb);
  else {
    ULEFTSHIFT(rowa, cola, colb, (long)ncols);
    for (ctemp = cola + ncols; ctemp <= colb; ctemp++)
      image[rowa][ctemp - ncols] = image[rowa][ctemp];
    for (ctemp = colb - ncols + 1; ctemp <= colb; ctemp++)
      image[rowa][ctemp] = isospace;
  }
  USETCURSOR(row, col);
}  /*leftshift*/


/* queueprim -- queue t-primitive for later evaluation */
Static Void queueprim(pcode, par1, par2)
uchar pcode;
uchar par1, par2;
{
  nqueued++;
  if (nqueued > 1 && pcode == capb) {
    if (qpcode[nqueued - 2] == capb)
      nqueued--;
  }
  qpcode[nqueued - 1] = pcode;
  qpar1[nqueued - 1] = par1;
  qpar2[nqueued - 1] = par2;
}  /*queueprim*/


/* textchar -- action of H-CHARACTER to send text */
Static Void textchar(ch)
uchar ch;
{
  cftest(nocf);   /* flush any H-SETFIELD or H-SETCURSOR */
  UCHAR(ch);
  image[row][col] = ch;
  if (col < maxcol())
    col++;
  else {
    /* set the cursor back - */
    /* - the cursor move done was wrong anyway */
    USETCURSOR(row, col);
  }
}  /*textchar*/


/* sendtoken -- action of H-TOKEN to transfer token */
Static Void sendtoken(reqcode)
uchar reqcode;
{
  cftest(nocf);   /* flush any H-SETFIELD or H-SETCURSOR */
  if (mode[cursor] == 0) {  /* bound cursor */
    if (row < boxtop || row > boxbottom || col < boxleft || col > boxright) {
      row = boxtop;
      col = boxleft;
    }
  }
  mode[tlevel] = 0;   /* check this on token return */
  queueprim(capc, tlevel, masterlevel);   /* queue T-SETMODE */
  if (reqcode == 1)   /* queue T-TOKEN */
    queueprim(capa, 0, nul);
  UTOKEN(0L);
  token = withterm;   /* control with terminal emulation */
  USETCURSOR(row, col);
}  /*sendtoken*/


/* setcursor -- action of H-SETCURSOR to set the cursor position */
Static Void setcursor(newrow, newcol)
char newrow;
uchar newcol;
{
  cftest(csrpend);   /* flush any H-SETFIELD, defer H-SETCURSOR */
  row = newrow;
  col = newcol;
}  /*setcursor*/


/* setmode -- action of H-SETMODE to set a mode array element value */
Static Void setmode(index, ivalue)
char index;
uchar ivalue;
{
  long rc;

  if (index > intsignal) {
    mode[index] = ivalue;
    return;
  }
  switch (index) {

  case tlevel:
    if (ivalue > 0)
      mode[tlevel] = ivalue;
    else
      queueprim(capc, tlevel, 1);
    break;

  case tmaxrow:
    if (ivalue != mode[tmaxrow])
      queueprim(capc, tmaxrow, mode[tmaxrow]);
    break;

  case tmaxcol:
    if (ivalue != mode[tmaxcol])
      queueprim(capc, tmaxcol, mode[tmaxcol]);
    break;

  case dsinvalid:
    mode[dsinvalid] = ivalue;
    break;

  case notify:
    mode[notify] = ivalue;
    break;

  case selectgr:
    rc = 0;
    USETMODE(ivalue, &rc);
    if (rc >= 0)
      mode[selectgr] = ivalue;
    else
      queueprim(capc, selectgr, defrend);
    break;

  case reqshift:
    if (ivalue < 2)
      mode[reqshift] = ivalue;
    else
      queueprim(capc, reqshift, 0);
    break;

  case cursor:
    if (ivalue < 2)
      mode[cursor] = ivalue;
    else
      queueprim(capc, cursor, 0);
    break;

  case icharmode:
    if (ivalue < 2)
      mode[icharmode] = ivalue;
    else
      queueprim(capc, icharmode, 0);
    break;

  case ilinerow:
    if (ivalue <= mode[tmaxrow])
      mode[ilinerow] = ivalue;
    else
      queueprim(capc, ilinerow, mode[ilinerow]);
    break;

  case intsignal:
    queueprim(capc, intsignal, 0);
    break;
  }
}  /*setmode*/


/* erasetoright -- action of H-ERASETORIGHT to erase to end of row */
Static Void erasetoright()
{
  cftest(nocf);   /* flush any H-SETFIELD or H-SETCURSOR */
  eraserow(row, col, maxcol());
}  /*erasetoright*/


/* insertspace -- action of H-INSERTSPACE to insert SPACE characters */
Static Void insertspace(nsp)
uchar nsp;
{
  uchar count;

  cftest(nocf);   /* flush any H-SETFIELD or H-SETCURSOR */
  if (mode[reqshift] != 1)
    return;
  rightshift(row, col, maxcol(), nsp);
  for (count = 1; count <= nsp; count++)
    UCHAR(isospace);
  USETCURSOR(row, col);
}  /*insertspace*/


/* deletechar -- action of H-DELETECHAR to delete characters */
Static Void deletechar(nch)
uchar nch;
{
  cftest(nocf);   /* flush any H-SETFIELD or H-SETCURSOR */
  if (mode[reqshift] == 1)
    leftshift(row, col, maxcol(), nch);
}  /*deletechar*/


/* soundalarm -- action of H-SOUNDALARM to sound the audible alarm */
Static Void soundalarm()
{
  UALARM();
}  /*soundalarm*/


/* scrollup -- action of H-SCROLLUP to scroll rows up */
Static Void scrollup(rowa, rowb, nrows)
char rowa, rowb;
uchar nrows;
{
  char rtemp;
  uchar ctemp;
  char FORLIM;
  uchar FORLIM1;

  USCROLLUP(rowa, rowb, 0, maxcol(), (long)nrows);
  FORLIM = rowb - nrows;
  for (rtemp = rowa; rtemp <= FORLIM; rtemp++) {
    FORLIM1 = maxcol();
    for (ctemp = 0; ctemp <= FORLIM1; ctemp++)
      image[rtemp][ctemp] = image[rtemp + nrows][ctemp];
  }
  for (rtemp = rowb - nrows + 1; rtemp <= rowb; rtemp++) {
    FORLIM1 = maxcol();
    for (ctemp = 0; ctemp <= FORLIM1; ctemp++)
      image[rtemp][ctemp] = isospace;
  }
}  /*scrollup*/


/* scrolldown -- action of H-SCROLLDOWN to scroll rows down */
Static Void scrolldown(rowa, rowb, nrows)
char rowa, rowb;
uchar nrows;
{
  char rtemp;
  uchar ctemp, FORLIM1;

  USCROLLDOWN(rowa, rowb, 0, maxcol(), (long)nrows);
  for (rtemp = rowb; rtemp >= rowa + nrows; rtemp--) {
    FORLIM1 = maxcol();
    for (ctemp = 0; ctemp <= FORLIM1; ctemp++)
      image[rtemp][ctemp] = image[rtemp - nrows][ctemp];
  }
  for (rtemp = rowa + nrows - 1; rtemp >= rowa; rtemp--) {
    FORLIM1 = maxcol();
    for (ctemp = 0; ctemp <= FORLIM1; ctemp++)
      image[rtemp][ctemp] = isospace;
  }
}  /*scrolldown*/


/* erasedisplay -- action of H-ERASEDISPLAY to erase screen image */
Static Void erasedisplay()
{
  if (cfstate == csrpend)   /* flush any H-SETFIELD */
    cfstate = nocf;   /* implied cursor movement to [0,0] */
  else
    cftest(nocf);
  erasebox(0, maxrow(), 0, maxcol());
  row = 0;
  col = 0;
  USETCURSOR(row, col);
}  /*erasedisplay*/


/* erasefields -- action of H-ERASEFIELDS to erase all field defns */
Static Void erasefields()
{
  uchar ftemp, FORLIM;
  savedfield *WITH;

  if (cfstate == fldpend)   /* flush any H-SETCURSOR */
    cfstate = nocf;   /* implicit selection of field zero */
  else
    cftest(nocf);
  curfield = 0;
  maxfield = 0;
  FORLIM = fieldlimit();
  for (ftemp = 0; ftemp <= FORLIM; ftemp++) {
    WITH = &field[ftemp];
    WITH->fldtop = 0;
    WITH->fldbottom = 0;
    WITH->fldleft = 0;
    WITH->fldright = 0;
  }
  boxtop = 0;
  boxbottom = 0;
  boxleft = 0;
  boxright = 0;
}  /*erasefields*/


/* setfield -- action of H-SETFIELD to select new field definition */
Static Void setfield(fidx)
uchar fidx;
{
  savedfield *WITH;

  cfstate = fldpend;   /* suppress any H-SETCURSOR */
  if (fidx > maxfield)
    maxfield = fidx;
  curfield = fidx;
  WITH = &field[curfield];
  boxtop = WITH->fldtop;
  boxbottom = WITH->fldbottom;
  boxleft = WITH->fldleft;
  boxright = WITH->fldright;
  row = boxtop;
  col = boxleft;
}  /*setfield*/


/* setupdate -- action of H-SETUPDATE to set update limits for field */
Static Void setupdate(rowa, rowb, cola, colb)
char rowa, rowb;
uchar cola, colb;
{
  savedfield *WITH;

  cftest(nocf);   /* flush any H-SETFIELD or H-SETCURSOR */
  WITH = &field[curfield];
  boxtop = rowa;
  WITH->fldtop = boxtop;
  boxbottom = rowb;
  WITH->fldbottom = boxbottom;
  boxleft = cola;
  WITH->fldleft = boxleft;
  boxright = colb;
  WITH->fldright = boxright;
}  /*setupdate*/


/* erasetabs -- action of H-ERASETABS to erase all tabulation stops */
Static Void erasetabs()
{
  uchar ctemp, FORLIM;

  FORLIM = maxcol();
  for (ctemp = 0; ctemp <= FORLIM; ctemp++)
    tabs[ctemp] = notab;
}  /*erasetabs*/


/* settab -- action of H-SETTAB to set one tabulation stop */
Static Void settab(tcol)
uchar tcol;
{
  tabs[tcol] = tabset;
}  /*settab*/


/* session -- action of H-SESSION to enter or exit session */
Static Void session(reqcode)
uchar reqcode;
{
  char tcmaxrow;
  uchar tcmaxcol;

  cftest(nocf);   /* flush any [H-SETFIELD or] H-SETCURSOR */
  if (reqcode != 0)
    return;
  initshared();   /* new session, init shared data structure */
  masterlevel = 1;
  UGETSIZE(&tcmaxrow, &tcmaxcol);
  queueprim(capc, tlevel, masterlevel);   /* queue T-SETMODE */
  queueprim(capc, tmaxrow, tcmaxrow - 1);   /* queue T-SETMODE */
  queueprim(capc, tmaxcol, tcmaxcol - 1);   /* queue T-SETMODE */
}  /*session*/


/* reqtoken -- action of H-REQTOKEN to request early return of token */
Static Void reqtoken()
{
  cftest(nocf);   /* flush any H-SETFIELD or H-SETCURSOR */
  /* queue T-TOKEN(128) */
  queueprim(capa, 128, nul);
}  /*reqtoken*/


/* getloc -- returns current cursor position row and column */
Static Void getloc(currow, curcol)
char *currow;
uchar *curcol;
{
  *currow = row;
  *curcol = col;
}  /*getloc*/


/* getfield -- returns current field index */
Static Void getfield(curfdx)
uchar *curfdx;
{
  *curfdx = curfield;
}  /*getfield*/


/* getmode -- returns a copy of the mode array */
Static Void getmode(mcopy)
uchar *mcopy;
{
  memcpy(mcopy, mode, sizeof(modearray));
}  /*getmode*/


/* putmode -- makes the real mode array conform to that supplied */
Static Void putmode(mcopy)
uchar *mcopy;
{
  char midx;

  for (midx = notify; midx <= knewline; midx++) {
    if (mcopy[midx] != mode[midx])
      setmode(midx, mcopy[midx]);
  }
}  /*putmode*/


/* getrow -- returns the text of the requested row */
Static Void getrow(r, line)
char r;
uchar *line;
{
  uchar tcol, mcol;

  mcol = maxcol();
  for (tcol = 0; tcol <= mcol; tcol++)
    line[tcol] = image[r][tcol];
  if (mcol < zmaxcol) {
    for (tcol = mcol + 1; tcol <= zmaxcol; tcol++)
      line[tcol] = blank;
  }
}  /*getrow*/


/* putrow -- replaces the text for the specified row */
Static Void putrow(r, line)
char r;
uchar *line;
{
  uchar tcol, mcol;
  boolean done;
  uchar ch;
  uchar bcount, bidx;

  done = false;
  mcol = maxcol();
  setcursor(r, 0);

  if (mode[selectgr] != defrend) {   /* clear to default rendition spaces */
    if (mode[reqshift] == 1)   /* clear to selected rendition */
      insertspace(mcol + 1);
    else {  /* must output each character */
      for (tcol = 0; tcol <= mcol; tcol++)
	textchar(line[tcol]);
      done = true;
    }
  } else
    erasetoright();

  /* if the text has not yet been output, use the following
     algorithm which will skip large areas of white space */
  tcol = 0;
  bcount = 0;
  while (!done) {
    ch = line[tcol];
    if (ch == blank)
      bcount++;
    else {
      if (bcount > 0) {
	if (bcount >= 10)
	  setcursor(r, tcol);
	else {
	  for (bidx = 1; bidx <= bcount; bidx++)
	    textchar(blank);
	}
	bcount = 0;
      }
      textchar(ch);
    }
    if (tcol == mcol)
      done = true;
    else
      tcol++;
  }
}  /*putrow*/


/* refresh -- refreshes the whole shared data structure */
Static Void refresh()
{
  char svrow, trow;
  uchar svcol;
  rowtext text;
  char FORLIM;

  svrow = row;
  svcol = col;   /* note cursor position */

  /* mode array */
  if (masterlevel == 0)   /* safety check */
    masterlevel = 1;
  setmode(tlevel, masterlevel);
  setmode(dsinvalid, 0);
  setmode(selectgr, defrend);

  FORLIM = maxrow();
  /* screen image and cursor position */
  for (trow = 0; trow <= FORLIM; trow++) {
    getrow(trow, text);
    putrow(trow, text);
  }
  setcursor(svrow, svcol);
}  /*refresh*/


/* tcharacter -- interpret T-CHARACTER character replaced report */
Static Void tcharacter(ch)
uchar ch;
{
  if (defer)   /* queue T-CHARACTER */
    queueprim(star, ch, nul);
  else {
    image[row][col] = ch;
    UCHAR(ch);
  }
  if (col < boxright)
    col++;
}  /*tcharacter*/


/* ttoken -- interpret T-TOKEN return of token */
Static Void ttoken(reqcode)
uchar reqcode;
{
  if (defer) {   /* queue T-TOKEN */
    queueprim(capa, reqcode, nul);
    return;
  }
  UTOKEN(1L);
  token = withhost;
  if (reqcode == 128)   /* possibly a duplicate token */
    mode[tlevel] = masterlevel;
  if (mode[dsinvalid] != 0 || mode[tlevel] != masterlevel)
  {   /* data structure corrupt */
    refresh();
    repainted = true;
  }
  /* return from nested session */
  /* application will interpret the request code parameter */
}  /*ttoken*/


/* tsetcursor -- interpret T-SETCURSOR cursor positioned report */
Static Void tsetcursor(newrow, newcol)
char newrow;
uchar newcol;
{
  if (defer)   /* queue T-SETCURSOR */
    queueprim(capb, newrow, newcol);
  else
    USETCURSOR(newrow, newcol);
  row = newrow;
  col = newcol;
}  /*tsetcursor*/


/* tsetmode -- interpret T-SETMODE mode array correction report */
Static Void tsetmode(index, ivalue)
char index;
uchar ivalue;
{
  uchar newval;

  if (defer) {   /* queue T-SETMODE */
    queueprim(capc, index, ivalue);
    return;
  }
  newval = ivalue;
  if (index <= dsinvalid) {
    switch (index) {

    case tlevel:
      if (newval == 0)
	mode[dsinvalid] = 1;
      else if (masterlevel == 0)
	masterlevel = newval;
      break;

    case tmaxrow:
      if (newval > zmaxrow)
	newval = zmaxrow;
      break;

    case tmaxcol:
      if (newval > zmaxcol)
	newval = zmaxcol;
      break;

    case dsinvalid:
      newval = 1;
      break;

    }/*case*/
  } else {
    if (index == icharmode && newval > 1)
      newval = 0;
    else
      ivalue = 0;
  }
  mode[index] = newval;
}  /*tsetmode*/


/* terasetoright -- interpret T-ERASETORIGHT right erasure report */
Static Void terasetoright()
{
  if (defer)   /* queue T-ERASETORIGHT */
    queueprim(capd, nul, nul);
  else {
    if (mode[keraright] > 0 || mode[keraline] > 0)
      eraserow(row, col, boxright);
  }
}  /*terasetoright*/


/* tinsertspace -- interpret T-INSERTSPACE SPACEs inserted report */
Static Void tinsertspace(nsp)
uchar nsp;
{
  if (defer)   /* queue T-INSERTSPACE */
    queueprim(cape, nsp, nul);
  else {
    if (mode[kinsspac] > 0 || mode[icharmode] == 1)
      rightshift(row, col, boxright, nsp);
  }
}  /*tinsertspace*/


/* tdeletechar -- interpret T-DELETECHAR characters deleted report */
Static Void tdeletechar(nch)
uchar nch;
{
  if (defer)   /* queue T-DELETECHAR */
    queueprim(capf, nch, nul);
  else {
    if (mode[kdelchar] > 0)
      leftshift(row, col, boxright, nch);
  }
}  /*tdeletechar*/


/* teraseprev -- interpret T-ERASEPREV previous char erasure report */
Static Void teraseprev(nch)
uchar nch;
{
  uchar temp;

  if (defer)   /* queue T-ERASEPREV */
    queueprim(capg, nch, nul);
  if (mode[keraprev] <= 0)
    return;
  /* Note:  nch <= col - boxleft */
  for (temp = 1; temp <= nch; temp++) {
    col--;
    if (!defer) {
      if (mode[icharmode] == 1)
	leftshift(row, col, boxright, 1);
      else {
	USETCURSOR(row, col);
	UCHAR(isospace);
	USETCURSOR(row, col);
	image[row][col] = isospace;
      }
    }
  }
}  /*teraseprev*/


/* tinsertline -- interpret T-INSERTLINE line(s) inserted report */
Static Void tinsertline(nln)
uchar nln;
{
  char rtemp, FORLIM;

  if (defer) {   /* queue T-INSERTLINE */
    queueprim(caph, nln, nul);
    return;
  }
  if (mode[kinsline] <= 0)
    return;
  if (boxbottom - row < nln) {
    erasebox(row, boxbottom, boxleft, boxright);
    return;
  }
  if ((boxleft == 0) & (boxright == maxcol())) {
    scrolldown(row, boxbottom, nln);
    return;
  }
  FORLIM = row;
  for (rtemp = boxbottom - nln; rtemp >= FORLIM; rtemp--)
    rowcopy(rtemp, rtemp + nln, boxleft, boxright);
  erasebox(row, row + nln - 1, boxleft, boxright);
}  /*tinsertline*/


/* tdeleteline -- interpret T-DELETELINE line(s) deleted report */
Static Void tdeleteline(nln)
uchar nln;
{
  char rtemp, FORLIM;

  if (defer) {   /* queue T-DELETELINE */
    queueprim(capi, nln, nul);
    return;
  }
  if (mode[kdelline] <= 0)
    return;
  if (boxbottom - row < nln) {
    erasebox(row, boxbottom, boxleft, boxright);
    return;
  }
  if ((boxleft == 0) & (boxright == maxcol())) {
    scrollup(row, boxbottom, nln);
    return;
  }
  FORLIM = boxbottom;
  for (rtemp = row + nln; rtemp <= FORLIM; rtemp++)
    rowcopy(rtemp, rtemp - nln, boxleft, boxright);
  erasebox(boxbottom - nln + 1, boxbottom, boxleft, boxright);
}  /*tdeleteline*/


/* tappendline -- interpret T-APPENDLINE line appended report */
Static Void tappendline()
{
  char rtemp, FORLIM;

  if (defer)   /* queue T-APPENDLINE */
    queueprim(capj, nul, nul);
  if (mode[kappline] <= 0)
    return;
  col = boxleft;
  if (row >= mode[ilinerow] || row == boxbottom) {
    if (!defer && boxtop < boxbottom) {
      if ((boxleft == 0) & (boxright == maxcol()))
	scrollup(boxtop, row, 1);
      else {
	FORLIM = row;
	for (rtemp = boxtop + 1; rtemp <= FORLIM; rtemp++)
	  rowcopy(rtemp, rtemp - 1, boxleft, boxright);
      }
    }
  } else {
    row++;
    if (!defer) {
      if ((boxleft == 0) & (boxright == maxcol()))
	scrolldown(row, boxbottom, 1);
      else {
	FORLIM = row;
	for (rtemp = boxbottom - 1; rtemp >= FORLIM; rtemp--)
	  rowcopy(rtemp, rtemp + 1, boxleft, boxright);
      }
    }
  }
  if (!defer) {
    eraserow(row, boxleft, boxright);
    tsetcursor(row, col);
  }
}  /*tappendline*/


/* tsplitline -- interpret T-SPLITLINE line split report */
Static Void tsplitline()
{
  char rtemp;
  uchar cinit;
  char FORLIM;

  if (defer)   /* queue T-SPLITLINE */
    queueprim(capk, nul, nul);
  if (mode[ksplline] <= 0)
    return;
  cinit = col;
  col = boxleft;
  if (row >= mode[ilinerow] || row == boxbottom) {
    if (!defer && boxtop < boxbottom) {
      if ((boxleft == 0) & (boxright == maxcol())) {
	scrollup(boxtop, row - 1, 1);
	rowcopy(row, row - 1, boxleft, boxright);
      } else {
	FORLIM = row;
	for (rtemp = boxtop + 1; rtemp <= FORLIM; rtemp++)
	  rowcopy(rtemp, rtemp - 1, boxleft, boxright);
      }
    }
  } else {
    if (!defer) {
      if ((boxleft == 0) & (boxright == maxcol())) {
	scrolldown(row + 1, boxbottom, 1);
	rowcopy(row, row + 1, boxleft, boxright);
      } else {
	FORLIM = row;
	for (rtemp = boxbottom - 1; rtemp >= FORLIM; rtemp--)
	  rowcopy(rtemp, rtemp + 1, boxleft, boxright);
      }
    }
    row++;
  }
  if (defer)
    return;
  if (row > boxtop)
    eraserow(row - 1, cinit, boxright);
  leftshift(row, boxleft, boxright, cinit - boxleft);
  USETCURSOR(row, col);
}  /*tsplitline*/


/* tsetfield -- interpret T-SETFIELD field selection report */
Static Void tsetfield(fidx)
uchar fidx;
{
  savedfield *WITH;

  if (defer)   /* queue T-SETFIELD */
    queueprim(capl, fidx, nul);
  /* Note: fidx <= maxfield */
  curfield = fidx;
  WITH = &field[curfield];
  boxtop = WITH->fldtop;
  boxbottom = WITH->fldbottom;
  boxleft = WITH->fldleft;
  boxright = WITH->fldright;
  row = boxtop;
  col = boxleft;
  if (!defer)
    USETCURSOR(row, col);
}  /*tsetfield*/


/* checkfield -- identify and check field which cursor occupies */
Static boolean checkfield(warnuser)
boolean warnuser;
{
  uchar tfield;
  boolean found, seeking;
  char trow;
  uchar tcol;
  savedfield *WITH;

  found = (row >= boxtop && row <= boxbottom && col >= boxleft && col <= boxright);
  /* within current field? */
  if (found)  /* attempt identification of new current field */
    return found;
  seeking = true;
  tfield = 0;
  while (seeking) {
    WITH = &field[tfield];
    if (row >= WITH->fldtop && row <= WITH->fldbottom &&
	col >= WITH->fldleft && col <= WITH->fldright) {
      found = true;
      seeking = false;
    } else {
      if (tfield < maxfield)
	tfield++;
      else
	seeking = false;
    }
  }

  if (!found) {
    if (warnuser)  /* edit -- sound alarm to warn user */
      UALARM();
    return found;
  }
  trow = row;
  tcol = col;
  tsetfield(tfield);   /* select new field */
  /* report cursor posn */
  tsetcursor(trow, tcol);
  return found;
}  /*checkfield*/


/* upkey -- process keystroke associated with mode[kcsrup] */
Static Void upkey()
{
  if (mode[cursor] == 0) {  /* cursor bound to update limits box */
    if (row > boxtop)
      tsetcursor(row - 1, col);
    else
      ttoken(mode[kcsrup]);
    return;
  }
  if (row > 0)
    tsetcursor(row - 1, col);
  else
    tsetcursor(maxrow(), col);

  /* free cursor */
}  /*upkey*/


/* downkey -- process keystroke associated with mode[kcsrdown] */
Static Void downkey()
{
  if (mode[cursor] == 0) {  /* Cursor bound to update limits box */
    if (row < boxbottom)
      tsetcursor(row + 1, col);
    else
      ttoken(mode[kcsrdown]);
    return;
  }
  if (row < maxrow())
    tsetcursor(row + 1, col);
  else
    tsetcursor(0, col);

  /* Free cursor */
}  /*downkey*/


/* leftkey -- process keystroke associated with mode[kcsrleft] */
Static Void leftkey()
{
  if (mode[cursor] == 0) {  /* Cursor bound to update limits box */
    if (col > boxleft)
      tsetcursor(row, col - 1);
    else
      ttoken(mode[kcsrleft]);
    return;
  }
  if (col > 0) {
    tsetcursor(row, col - 1);
    return;
  }
  if (row > 0)
    tsetcursor(row - 1, maxcol());
  else
    tsetcursor(maxrow(), maxcol());

  /* Free cursor */
}  /*leftkey*/


/* rightkey -- process keystroke associated with mode[kcrsright] */
Static Void rightkey()
{
  if (mode[cursor] == 0) {  /* Cursor bound to update limits box */
    if (col < boxright)
      tsetcursor(row, col + 1);
    else
      ttoken(mode[kcsrright]);
    return;
  }
  if (col < maxcol()) {
    tsetcursor(row, col + 1);
    return;
  }
  if (row < maxrow())
    tsetcursor(row + 1, 0);
  else
    tsetcursor(0, 0);

  /* Free cursor */
}  /*rightkey*/


/* enterkey -- process keystroke associated with mode[kenter] */
Static Void enterkey()
{
  while (!checkfield(false))
    rightkey();
  tsetcursor(row, boxleft);
  ttoken(mode[kenter]);
}  /*enterkey*/


/* nexttabkey -- process keystroke associated with mode[ktabnext] */
Static Void nexttabkey()
{
  boolean atlimit, seeking;
  uchar tcol, rlimit;

  if (mode[cursor] == 0)
    rlimit = boxright;
  else
    rlimit = maxcol();
  tcol = col;
  atlimit = false;
  seeking = true;
  while (seeking) {
    if (tcol < rlimit) {
      tcol++;
      if (tabs[tcol] == tabset)
	seeking = false;
    } else {
      seeking = false;
      atlimit = true;
    }
  }
  tsetcursor(row, tcol);
  if (!atlimit)
    return;
  if (mode[cursor] == 0)
    ttoken(mode[knexttab]);
  else
    rightkey();
}  /*nexttabkey*/


/* prevtabkey -- process keystroke associated with mode[ktabprev] */
Static Void prevtabkey()
{
  boolean atlimit, seeking;
  uchar tcol, llimit;

  if (mode[cursor] == 0)
    llimit = boxleft;
  else
    llimit = 0;
  tcol = col;
  atlimit = false;
  seeking = true;
  while (seeking) {
    if (tcol > llimit) {
      tcol--;
      if (tabs[tcol] == tabset)
	seeking = false;
    } else {
      seeking = false;
      atlimit = true;
    }
  }
  tsetcursor(row, tcol);
  if (!atlimit)
    return;
  if (mode[cursor] == 0)
    ttoken(mode[kprevtab]);
  else
    leftkey();
}  /*prevtabkey*/


/* leftukey -- process keystroke associated with mode[kleftupd] */
Static Void leftukey()
{
  while (!checkfield(false))
    rightkey();
  tsetcursor(row, boxleft);
}  /*leftukey*/


/* firstnskey -- process keystroke associated with mode[kfirstns] */
Static Void firstnskey()
{
  uchar tcol;
  boolean seeking;

  while (!checkfield(false))
    rightkey();
  tcol = boxleft;
  seeking = true;
  while (seeking) {
    if (image[row][tcol] != isospace) {
      seeking = false;
      break;
    }
    if (tcol < boxright)
      tcol++;
    else {
      seeking = false;
      tcol = boxleft;
    }
  }
  tsetcursor(row, tcol);
}  /*firstnskey*/


/* alastnskey -- process keystroke associated with mode[kalastns] */
Static Void alastnskey()
{
  uchar tcol;
  boolean seeking;

  while (!checkfield(false))
    rightkey();
  tcol = boxright;
  seeking = true;
  while (seeking) {
    if (image[row][tcol] != isospace) {
      seeking = false;
      break;
    }
    if (tcol > boxleft)
      tcol--;
    else
      seeking = false;
  }
  if (image[row][tcol] != isospace && tcol < boxright)
    tcol++;
  tsetcursor(row, tcol);
}  /*alastnskey*/


/* ctogglekey -- process keystroke associated with mode[kinsmode] */
Static Void ctogglekey()
{
  tsetmode(icharmode, 1 - mode[icharmode]);
}  /*ctogglekey*/


/* erightkey -- process keystroke associated with mode[keraright] */
Static Void erightkey()
{
  if (checkfield(true))
    terasetoright();
}  /*erightkey*/


/* ispackey -- process keystroke associated with mode[kinsspac] */
Static Void ispackey()
{
  if (checkfield(true))
    tinsertspace(1);
}  /*ispackey*/


/* dcharkey -- process keystroke associated with mode[kdelchar] */
Static Void dcharkey()
{
  if (checkfield(true))
    tdeletechar(1);
}  /*dcharkey*/


Local Void tryprevious()
{
  char trow;
  uchar tcol;

  leftkey();
  if (!checkfield(true))
    return;
  trow = row;
  tcol = col;
  tcharacter(isospace);
  tsetcursor(trow, tcol);
}  /*tryprevious*/


/* eprevkey -- process keystroke associated with mode[keraprev] */
Static Void eprevkey()
{
  if (!checkfield(false)) {
    if (mode[cursor] == 1)
      tryprevious();
    return;
  }
  if (col != boxleft) {
    teraseprev(1);
    return;
  }
  if (mode[cursor] == 0)
    ttoken(mode[keraprev]);
  else
    tryprevious();
}  /*eprevkey*/


/* ilinekey -- process keystroke associated with mode[kinsline] */
Static Void ilinekey()
{
  if (checkfield(true)) {
    tinsertline(1);
    tsetcursor(row, col);
  }
}  /*ilinekey*/


/* dlinekey -- process keystroke associated with mode[kdelline] */
Static Void dlinekey()
{
  if (checkfield(true)) {
    tdeleteline(1);
    tsetcursor(row, col);
  }
}  /*dlinekey*/


/* elinekey -- process keystroke associated with mode[keraline] */
Static Void elinekey()
{
  if (checkfield(true)) {
    tsetcursor(row, boxleft);
    terasetoright();
  }
}  /*elinekey*/


/* alinekey -- process keystroke associated with mode[kappline] */
Static Void alinekey()
{
  if (checkfield(true))
    tappendline();
}  /*alinekey*/


/* slinekey -- process keystroke associated with mode[ksplline] */
Static Void slinekey()
{
  if (checkfield(true))
    tsplitline();
}  /*slinekey*/


/* nextfkey -- process keystroke associated with mode[knextfld] */
Static Void nextfkey()
{
  while (!checkfield(false))
    leftkey();
  if (curfield < maxfield)
    tsetfield(curfield + 1);
  else
    tsetfield(0);
}  /*nextfkey*/


/* prevfkey -- process keystroke associated with mode[kprevfld] */
Static Void prevfkey()
{
  while (!checkfield(false))
    rightkey();
  if (curfield > 0)
    tsetfield(curfield - 1);
  else
    tsetfield(maxfield);
}  /*prevfkey*/


/* homefkey -- process keystroke associated with mode[khomefld] */
Static Void homefkey()
{
  tsetfield(0);
}  /*homefkey*/


/* newlnkey -- process keystroke associated with mode[knewline] */
Static Void newlnkey()
{
  while (!checkfield(false))
    leftkey();
  if (row < boxbottom)
    tsetcursor(row + 1, boxleft);
  else
    nextfkey();
}  /*newlnkey*/


/* restartkey -- process keystroke associated with mode[krestart] */
Static Void restartkey()
{
  mode[dsinvalid] = 1;
  queueprim(capa, 129, 0);
}  /*restartkey*/


/* datakey -- process data keystrokes */
Static Void datakey(chkey)
uchar chkey;
{
  boolean atboxright;

  if (!checkfield(true))
    return;
  if (mode[icharmode] == 1)   /* Insert a SPACE */
    tinsertspace(1);
  atboxright = (col == boxright);
  tcharacter(chkey);
  /* Replaces character and
                           may move cursor to right */
  if (!atboxright)
    return;
  if (mode[cursor] == 0)
    ttoken(0);
  else
    rightkey();
}  /*datakey*/


/* qcmdsignal -- process queue command signal keystroke */
Static Void qcmdsignal(lkcode)
uchar lkcode;
{
  char mindex;
  boolean seeking, found;

  mindex = kinthost;
  seeking = true;
  while (seeking) {
    found = (lkcode == mode[mindex]);
    if (found || mindex == knewline)
      seeking = false;
    else
      mindex++;
  }

  if (!found) {  /* a "K"-prefix function */
    ttoken(lkcode);
    return;
  }
  switch (mindex) {

  case kinthost:
    UINTHOST();
    ttoken(lkcode);
    break;

  case ksuspend:
    ttoken(lkcode);
    break;

  case krestart:
    restartkey();
    break;

  case kenter:
    enterkey();
    break;

  case kcsrup:
    upkey();
    break;

  case kcsrdown:
    downkey();
    break;

  case kcsrleft:
    leftkey();
    break;

  case kcsrright:
    rightkey();
    break;

  case knexttab:
    nexttabkey();
    break;

  case kprevtab:
    prevtabkey();
    break;

  case kleftupd:
    leftukey();
    break;

  case kfirstns:
    firstnskey();
    break;

  case kalastns:
    alastnskey();
    break;

  case kinsmode:
    ctogglekey();
    break;

  case keraright:
    erightkey();
    break;

  case kinsspac:
    ispackey();
    break;

  case kdelchar:
    dcharkey();
    break;

  case keraprev:
    eprevkey();
    break;

  case kinsline:
    ilinekey();
    break;

  case kdelline:
    dlinekey();
    break;

  case keraline:
    elinekey();
    break;

  case kappline:
    alinekey();
    break;

  case ksplline:
    slinekey();
    break;

  case knextfld:
    nextfkey();
    break;

  case kprevfld:
    prevfkey();
    break;

  case khomefld:
    homefkey();
    break;

  case knewline:
    newlnkey();
    break;
  }/*case*/

  /* signal host application */
}  /*qcmdsignal*/


/* getlogkey -- get logical keystroke from one or two actual keystrokes */
Static boolean getlogkey(keych, retch, isdatach)
uchar keych, *retch;
boolean *isdatach;
{
  boolean gotchar;

  gotchar = false;
  *isdatach = true;
  if (escaped) {
    *retch = keych;
    if (keych >= isospace && keych <= tilde)
      *isdatach = false;
    escaped = false;
    gotchar = true;
    return gotchar;
  }
  if (keych == esc) {
    escaped = true;
    return gotchar;
  }
  *retch = keych;
  if (keych < isospace || keych > tilde)
    *isdatach = false;
  gotchar = true;
  return gotchar;
}  /*getlogkey*/


/* getprimitive -- process input until a primitive has been decoded */
Static Void getprimitive(pcode, ppars)
uchar *pcode;
uchar *ppars;
{
  uchar nextch, lch;
  char svrow;
  uchar svcol;
  uchar svfld;
  long i;
  boolean isdata;
  savedfield *WITH;
  long FORLIM;

  while (nqueued <= 0) {
    do {
      UREAD(&nextch);   /* read from keyboard */
    } while (!getlogkey(nextch, &lch, &isdata));

    /* save state and prevent image access from keystroke routines */
    defer = true;   /* queue primitives, don't change screen */
    svrow = row;
    svcol = col;
    svfld = curfield;

    if (isdata)   /* data character */
      datakey(lch);
    else
      qcmdsignal(lch);
    /* queued command signal */

    /* restore the state ready for processing of the queued primitives */
    row = svrow;
    col = svcol;
    curfield = svfld;
    WITH = &field[curfield];
    boxtop = WITH->fldtop;
    boxbottom = WITH->fldbottom;
    boxleft = WITH->fldleft;
    boxright = WITH->fldright;
    defer = false;   /* primitives change screen again */
  }

  *pcode = qpcode[0];
  ppars[0] = qpar1[0];
  ppars[1] = qpar2[0];
  nqueued--;
  FORLIM = nqueued;
  for (i = 1; i <= FORLIM; i++) {
    qpcode[i - 1] = qpcode[i];
    qpar1[i - 1] = qpar1[i];
    qpar2[i - 1] = qpar2[i];
  }

  /* now it's safe to call the semantic routines properly */
  switch (*pcode) {

  case star:
    tcharacter(ppars[0]);
    break;

  case capa:
    ttoken(ppars[0]);
    break;

  case capb:
    tsetcursor(ppars[0], ppars[1]);
    break;

  case capc:
    tsetmode(ppars[0], ppars[1]);
    break;

  case capd:
    terasetoright();
    break;

  case cape:
    tinsertspace(ppars[0]);
    break;

  case capf:
    tdeletechar(ppars[0]);
    break;

  case capg:
    teraseprev(ppars[0]);
    break;

  case caph:
    tinsertline(ppars[0]);
    break;

  case capi:
    tdeleteline(ppars[0]);
    break;

  case capj:
    tappendline();
    break;

  case capk:
    tsplitline();
    break;

  case capl:
    tsetfield(ppars[0]);
    break;

  }/*case*/
}  /*getprimitive*/


/* startssmp -- negotiate screen management session with terminal */
Static Void startssmp(success, usertext)
boolean *success;
uchar *usertext;
{
  initcoding();
  UINITSCRN(image, mode);

  session(0);   /* initialises shared data structure */
  /* note that in this version we don't put out the user text */
  /* since the the negotiation is not going to fail! */
  sendtoken(1);

  *success = true;   /* it always works here! */
}  /*startssmp */


/* stopssmp -- tidy up and close down screen management session */
Static Void stopssmp()
{
  scrollup(0, maxrow(), 1);
  setcursor(maxrow(), 0);
  session(1);
  UENDSCRN();
}  /*stopssmp*/


/*#### End of local SSMP module ### */


/* ****************************************************** stdenv ***** */
/* *                                                                 * */
/* *        Software Tools :: Standard environment routines          * */
/* *                                                                 * */
/* ******************************************************************* */


/* skipbl -- skip blanks and tabs at s[i]... */
Static Void skipbl(s, i)
uchar *s;
long *i;
{
  while (s[*i - 1] == blank || s[*i - 1] == tab)
    (*i)++;
}  /*skipbl*/


/* addstr -- put c in outset[j], if it fits, increment j */
Static boolean addstr(c, outset, j, maxset)
uchar c;
uchar *outset;
long *j, maxset;
{
  if (*j > maxset)
    return false;
  else {
    outset[*j - 1] = c;
    (*j)++;
    return true;
  }
}  /*addstr*/


/* equal -- test two strings for equality */
Static boolean equal(str1, str2)
uchar *str1, *str2;
{
  long i;

  i = 1;
  while (str1[i - 1] == str2[i - 1] && str1[i - 1] != endstr)
    i++;
  return (str1[i - 1] == str2[i - 1]);
}  /*equal*/


/* mapesc -- map s[i] into escaped character, increment i */
Static uchar mapesc(s, i)
uchar *s;
long *i;
{
  if (s[*i - 1] != escape)
    return (s[*i - 1]);
  else if (s[*i] == endstr)
    return escape;
  else {
    (*i)++;
    if (s[*i - 1] == letn)
      return newline;
    else if (s[*i - 1] == lett)
      return tab;
    else
      return (s[*i - 1]);
  }
}  /*mapesc*/


/* chindex -- find position of character c in string s */
Static long chindex(s, c)
uchar *s;
uchar c;
{
  long i;

  i = 1;
  while (s[i - 1] != c && s[i - 1] != endstr)
    i++;
  if (s[i - 1] == endstr)
    return 0;
  else
    return i;
}  /*chindex*/


/* isdigit -- true if c is a digit */
Static boolean isdigit_(c)
uchar c;
{
  return (c >= dig0 && c <= dig9);
}  /*isdigit*/


/* islower -- true if c is a lower case letter */
Static boolean islower_(c)
uchar c;
{
  return (c >= leta && c <= letz);
}  /*islower*/


/* isupper -- true if c is an upper case letter */
Static boolean isupper_(c)
uchar c;
{
  return (c >= capa && c <= capz);
}  /*isupper*/


/* isletter -- true if c is a letter */
Static boolean isletter(c)
uchar c;
{
  return (islower_(c) | isupper_(c));
}  /*isletter*/


/* isalphanum -- true if c is a letter or a digit */
Static boolean isalphanum(c)
uchar c;
{
  return (islower_(c) | isupper_(c) | isdigit_(c));
}  /*isalphanum*/


/* lowercase -- take character, return as lower case if upper case */
Static uchar lowercase(c)
uchar c;
{
  if (isupper_(c))
    return (c - capa + leta);
  else
    return c;
}  /*lowercase*/


/* itoc -- converts integer n to string in s[i], returns end of s */
Static long itoc(n, s, i)
long n;
uchar *s;
long i;
{
  if (n < 0) {
    s[i - 1] = minus;
    return (itoc(-n, s, i + 1));
  } else {
    if (n >= 10)
      i = itoc(n / 10, s, i);
    s[i - 1] = n % 10 + dig0;
/* p2c: cl-sun.p, line 2581:
 * Note: Using % for possibly-negative arguments [317] */
    s[i] = endstr;
    return (i + 1);
  }
}  /*itoc*/


/* max -- computes maximum of two integers */
Static long max(x, y)
long x, y;
{
  if (x > y)
    return x;
  else
    return y;
}  /*max*/


/* min -- computes minimum of two integers */
Static long min(x, y)
long x, y;
{
  if (x < y)
    return x;
  else
    return y;
}  /*min*/


/* scopy -- copies string starting at src[i] to dest starting at j */
Static Void scopy(src, i, dest, j)
uchar *src;
long i;
uchar *dest;
long j;
{
  while (src[i - 1] != endstr && j < maxstr) {
    dest[j - 1] = src[i - 1];
    i++;
    j++;
  }
  dest[j - 1] = endstr;
}  /*scopy*/


/* getword -- get word from s[i] into out */
Static long getword(s, i, out)
uchar *s;
long i;
uchar *out;
{
  long j;
  uchar c;

  c = s[i - 1];
  while (c == blank || c == tab || c == newline) {
    i++;
    c = s[i - 1];
  }
  j = 1;
  while (c != endstr && c != blank && c != tab && c != newline) {
    out[j - 1] = s[i - 1];
    i++;
    c = s[i - 1];
    j++;
  }
  out[j - 1] = endstr;
  if (s[i - 1] == endstr)
    return 0;
  else
    return i;
}  /*getword*/


/* ctoi -- convert string at s[i] to integer, increment i */
Static long ctoi(s, i)
uchar *s;
long *i;
{
  long n, sign;

  while (s[*i - 1] == blank || s[*i - 1] == tab)
    (*i)++;
  if (s[*i - 1] == minus)
    sign = -1;
  else
    sign = 1;
  if (s[*i - 1] == minus || s[*i - 1] == plus)
    (*i)++;
  n = 0;
  while (isdigit_(s[*i - 1])) {
    n = n * 10 + s[*i - 1] - dig0;
    (*i)++;
  }
  return (sign * n);
}  /*ctoi*/


/* length -- computes length of string s */
Static long length_(s)
uchar *s;
{
  long n;

  n = 1;
  while (s[n - 1] != endstr)
    n++;
  return (n - 1);
}  /*length*/


/* append -- append contents of str1 to contents of str2 */
Static Void append(str1, str2)
uchar *str1, *str2;
{
  scopy(str1, 1L, str2, length_(str2) + 1);
}  /*append*/


#define endlit          "&"
#define litlit          "@"


/* setstring -- make string from literal */
Static Void setstring(target, txt)
uchar *target;
Char *txt;
{
  boolean copying;
  long sidx, tidx;
  uchar isoch;
  Char lch;
  Char STR1[256];

  tidx = 1;
  sidx = 1;
  sprintf(STR1, "%c", txt[0]);
  copying = (strcmp(STR1, endlit) != 0);

  while (copying) {
    lch = txt[sidx - 1];
    isoch = nul;
    sprintf(STR1, "%c", lch);
    if (!strcmp(STR1, litlit)) {
      if (sidx < 24) {
	sidx++;
	lch = txt[sidx - 1];
	if (lch == 'n')   /* @n = newline */
	  isoch = newline;
	else if (lch == 't')
	  isoch = tab;
      }
    }
    if (isoch == nul)
      target[tidx - 1] = littoiso[lch];
    else
      target[tidx - 1] = isoch;
    tidx++;
    sidx++;
    if (sidx > 24)
      copying = false;
    else {
      sprintf(STR1, "%c", txt[sidx - 1]);
      copying = (strcmp(STR1, endlit) != 0);
    }
  }
  target[tidx - 1] = endstr;

  /* @t = tab */
}  /*setstring*/

#undef endlit
#undef litlit


/* appstring -- append literal to existing string */
Static Void appstring(target, txt)
uchar *target;
Char *txt;
{
  chstring temp;

  setstring(temp, txt);
  append(temp, target);
}  /*appstring*/


/* getarg -- gets up to maxsize characters of the n'th argument */
Static boolean getarg(n, str, maxsize)
long n;
uchar *str;
long maxsize;
{
  if ((unsigned long)n > nbrcmdargs)
    return false;
  else {
    scopy(cmdline, cmdargidx[n], str, 1L);
    return true;
  }
}  /*getarg*/


/* mustopen -- open file or die */
Static short mustopen(name, iomode)
uchar *name;
long iomode;
{
  short fd;
  chstring s;

  fd = openf(name, iomode);
  if (fd != ioerror_)
    return fd;
  scopy(name, 1L, s, 1L);
  appstring(s, ":  cannot open file@n&  ");
  putstr(s, stderror);
  errorexit();
  return fd;
}  /*mustopen*/


/* mustcreate -- create file or die */
Static short mustcreate(name, iomode)
uchar *name;
long iomode;
{
  short fd;
  chstring s;

  fd = create(name, iomode);
  if (fd != ioerror_)
    return fd;
  scopy(name, 1L, s, 1L);
  appstring(s, ":  cannot create file@n&");
  putstr(s, stderror);
  errorexit();
  return fd;
}  /*mustcreate*/


/* fcopy -- copy file fin to file fout */
Static Void fcopy(fin, fout)
short fin, fout;
{
  chstring lin;

  while (getline(lin, fin, (long)maxstr))
    putstr(lin, fout);
}  /*fcopy*/


/* ****************************************************** ssmpex ***** */
/* *                                                                 * */
/* *                         SSMP Extras                             * */
/* *                                                                 * */
/* ******************************************************************* */


/* literal -- convert literal string into 1 to 24 character calls */
Static Void literal(txt)
Char *txt;
{
  chstring s;
  long i;

  setstring(s, txt);
  i = 1;
  while (s[i - 1] != endstr) {
    textchar(s[i - 1]);
    i++;
  }
}  /*literal*/


/* outdec -- send integer to screen image */
Static Void outdec(n)
long n;
{
  long nd, k;
  chstring s;

  nd = itoc(n, s, 1L);
  for (k = 0; k <= nd - 2; k++)
    textchar(s[k]);
}  /*outdec*/


/* slcopy -- copy string s[i].. to rowtext ln[j].., max n characters */
Static Void slcopy(s, i, ln, j, n)
uchar *s;
long i;
uchar *ln;
uchar j;
long n;
{
  long jmax;

  jmax = j + n - 1;
  while (s[i - 1] != endstr && s[i - 1] != newline && j <= jmax) {
    ln[j] = s[i - 1];
    i++;
    j++;
  }
}  /*slcopy*/


/* ****************************************************** pattern **** */
/* *                                                                 * */
/* *          Software Tools :: Pattern matching routines            * */
/* *                                                                 * */
/* ******************************************************************* */


/* dodash -- expand set at src[i] into dest[j], stop at delim */
Static Void dodash(delim, src, i, dest, j, maxset)
uchar delim;
uchar *src;
long *i;
uchar *dest;
long *j, maxset;
{
  uchar k, ks, ke;
  boolean junk;

  while (src[*i - 1] != delim && src[*i - 1] != endstr) {
    if (src[*i - 1] == escape)
      junk = addstr(mapesc(src, i), dest, j, maxset);
    else {
      if (src[*i - 1] != minus)
	junk = addstr(src[*i - 1], dest, j, maxset);
      else {
	if (*j <= 1 || src[*i] == endstr)
	  junk = addstr(minus, dest, j, maxset);   /* literal "-" */
	else {
	  if ((isalphanum(src[*i - 2]) & isalphanum(src[*i])) &&
	      src[*i - 2] <= src[*i]) {
	    /* limits calculated first because some compilers get upset */
	    ks = src[*i - 2] + 1;
	    ke = src[*i];
	    for (k = ks; k <= ke; k++)
	      junk = addstr(k, dest, j, maxset);
	    (*i)++;
	  } else
	    junk = addstr(minus, dest, j, maxset);
	}
      }
    }
    (*i)++;
  }
}  /*dodash*/


/* getccl -- expand character class at arg[i] into pat[j] */
Local boolean getccl(arg, i, pat, j)
uchar *arg;
long *i;
uchar *pat;
long *j;
{
  long jstart;
  boolean junk;

  (*i)++;   /* skip over '[' */
  if (arg[*i - 1] == negate) {
    junk = addstr(nccl, pat, j, (long)maxpat);
    (*i)++;
  } else
    junk = addstr(ccl, pat, j, (long)maxpat);
  jstart = *j;
  junk = addstr(nul, pat, j, (long)maxpat);   /* room for count */
  dodash(cclend, arg, i, pat, j, (long)maxpat);
  pat[jstart - 1] = *j - jstart - 1;
  return (arg[*i - 1] == cclend);
}  /*getccl*/


/* stclose -- insert closure entry at pat[j] */
Local Void stclose(pat, j, lastj)
uchar *pat;
long *j, lastj;
{
  long jp, jt;
  boolean junk;

  for (jp = *j - 1; jp >= lastj; jp--) {
    jt = jp + closize;
    junk = addstr(pat[jp - 1], pat, &jt, (long)maxpat);
  }
  *j += closize;
  pat[lastj - 1] = closure;   /* where original pattern began */
}  /*stclose*/


/* makepat -- make pattern from arg[i], terminate at delim */
Static long makepat(arg, start, delim, pat)
uchar *arg;
long start;
uchar delim;
uchar *pat;
{  /* makepat main routine */
  long i, j, lastj, lj;
  boolean done, css, junk;
  uchar tc, plj;


  j = 1;   /* pat index */
  i = start;   /* arg index */
  css = cssinit;   /* case sensitive scan default */
  lastj = 1;
  done = false;
  while (!done && arg[i - 1] != delim && arg[i - 1] != endstr) {
    lj = j;
    if (arg[i - 1] == any)
      junk = addstr(any, pat, &j, (long)maxpat);
    else {
      if (arg[i - 1] == bol && i == start)
	junk = addstr(bol, pat, &j, (long)maxpat);
      else {
	if (arg[i - 1] == eol && arg[i] == delim)
	  junk = addstr(eol, pat, &j, (long)maxpat);
	else {
	  if (arg[i - 1] == ccl)
	    done = (getccl(arg, &i, pat, &j) == false);
	  else {
	    if (arg[i - 1] == closure && i > start) {
	      lj = lastj;
	      plj = pat[lj - 1];
	      if (plj == bol || plj == eol || plj == closure)
		done = true;   /* force loop termination */
	      else
		stclose(pat, &j, lastj);
	    } else {
	      if (arg[i - 1] == anycase)
		css = !css;   /* invert case sensitivity switch */
	      else {  /* literal character */
		if ((!css) & isletter(arg[i - 1]))
		{  /* anycase match for letter */
		  junk = addstr(ccl, pat, &j, (long)maxpat);
		  junk = addstr(2, pat, &j, (long)maxpat);
		  tc = arg[i - 1];
		  junk = addstr(tc, pat, &j, (long)maxpat);
		  if (tc < leta)
		    tc += leta - capa;
		  else
		    tc += capa - leta;
		  junk = addstr(tc, pat, &j, (long)maxpat);
		} else {  /* true literal */
		  junk = addstr(litchar, pat, &j, (long)maxpat);
		  junk = addstr(mapesc(arg, &i), pat, &j, (long)maxpat);
		}
	      }
	    }
	  }
	}
      }
    }
    lastj = lj;
    if (!done)
      i++;
  }
  if (done || arg[i - 1] != delim)
    return 0;   /* finished early */
  else {
    if (!addstr(endstr, pat, &j, (long)maxpat))
      return 0;   /* no room */
    else
      return i;   /* all is well */
  }
}  /*makepat*/


/* getpat -- convert whole of argument into pattern */
Static boolean getpat(arg, pat)
uchar *arg, *pat;
{
  return (makepat(arg, 1L, endstr, pat) > 0);
}  /*getpat*/


/* locate -- look for c in character class at pat[offset] */
Local boolean locate(c, pat, offset)
uchar c;
uchar *pat;
long offset;
{
  boolean Result;
  long i;

  /* size of class is at pat[offset], characters follow */
  Result = false;
  i = offset + pat[offset - 1];   /* last position */
  while (i > offset) {
    if (c == pat[i - 1]) {
      Result = true;
      i = offset;   /* force loop termination */
    } else
      i--;
  }
  return Result;
}  /*locate*/


/* omatch -- match one pattern element at pat[j] */
Local boolean omatch(lin, i, pat, j)
uchar *lin;
long *i;
uchar *pat;
long j;
{
  boolean Result;
  short advance;

  advance = -1;
  if (lin[*i - 1] == endstr)
    Result = false;
  else if (pat[j - 1] == litchar) {
    if (lin[*i - 1] == pat[j])
      advance = 1;
  } else if (pat[j - 1] == bol) {
    if (*i == 1)
      advance = 0;
  } else if (pat[j - 1] == any) {
    if (lin[*i - 1] != newline)
      advance = 1;
  } else if (pat[j - 1] == eol) {
    if (lin[*i - 1] == newline)
      advance = 0;
  } else if (pat[j - 1] == ccl) {
    if (locate(lin[*i - 1], pat, j + 1))
      advance = 1;
  } else if (pat[j - 1] == nccl) {
    if ((lin[*i - 1] != newline) & (!locate(lin[*i - 1], pat, j + 1)))
      advance = 1;
  } else
    errorexit();

  if (advance >= 0) {
    *i += advance;
    return true;
  } else
    return false;
}  /*omatch*/


/* patsize -- returns size of pattern entry at pat[n] */
Local long patsize(pat, n)
uchar *pat;
long n;
{
  uchar ptn;

  ptn = pat[n - 1];
  if (ptn == litchar)
    return 2;
  if (ptn == bol || ptn == eol || ptn == any)
    return 1;
  if (ptn == ccl || ptn == nccl)
    return (pat[n] + 2);
  if (ptn == closure)
    return closize;
  errorexit();
}  /*patsize*/


/* amatch -- look for match of pat[j]... at lin[offset]... */
Static long amatch(lin, offset, pat, j)
uchar *lin;
long offset;
uchar *pat;
long j;
{  /* amatch main routine */
  long i, k;
  boolean done;


  done = false;
  while (!done && pat[j - 1] != endstr) {
    if (pat[j - 1] != closure) {
      if (!omatch(lin, &offset, pat, j)) {
	offset = 0;   /* non-closure */
	done = true;
      } else  /* omatch succeeded on this pattern element */
	j += patsize(pat, j);
      continue;
    }
    j += patsize(pat, j);   /* step over closure */
    i = offset;

    /* match as many as possible */
    while (!done && lin[i - 1] != endstr) {
      if (!omatch(lin, &i, pat, j))
	done = true;
    }

    /* i points to input character that made us fail */
    /* match rest of pattern against rest of input */
    /* shrink closure by 1 after each failure */
    done = false;
    while (!done && i >= offset) {
      k = amatch(lin, i, pat, j + patsize(pat, j));
      if (k > 0)   /* matched rest of pattern */
	done = true;
      else
	i--;
    }
    offset = k;   /* if k=0 failure else success */
    done = true;
  }
  return offset;
}  /*amatch*/
/* p2c: cl-sun.p, line 3209: 
 * Warning: Symbol 'Result' was already defined [220] */


/* match -- find a match anywhere on a line */
Static boolean match(lin, pat)
uchar *lin, *pat;
{
  long i, pos;

  pos = 0;
  i = 1;
  while (lin[i - 1] != endstr && pos == 0) {
    pos = amatch(lin, i, pat, 1L);
    i++;
  }
  return (pos > 0);
}  /*match*/


/* makesub -- make substitution string from arg in sub */
Static long makesub(arg, fromm, delim, sub)
uchar *arg;
long fromm;
uchar delim;
uchar *sub;
{
  long i, j;
  boolean junk;

  j = 1;
  i = fromm;
  while (arg[i - 1] != delim && arg[i - 1] != endstr) {
    if (arg[i - 1] == ditto)
      junk = addstr(mkditto, sub, &j, (long)maxpat);
    else
      junk = addstr(mapesc(arg, &i), sub, &j, (long)maxpat);
    i++;
  }
  if (arg[i - 1] != delim)   /* missing delimiter */
    return 0;
  else {
    if (!addstr(endstr, sub, &j, (long)maxpat))
      return 0;
    else
      return i;
  }
}  /*makesub*/


/* ****************************************************** clcons ***** */
/* *                                                                 * */
/* *                   CURLEW :: Editor Constants                    * */
/* *                                                                 * */
/* ******************************************************************* */


#define maxlines        16384   /* maximum number of lines in file buffer */
#define curline         period   /* dot signals current line */
#define lastline        dollar   /* dollar signals last line */
#define topline         sharp   /* sharp signals top line of f/s display */

#define cmdprompt       greater   /* command prompt character */
#define txtprompt       period   /* a/c/i prompt character */
#define cbmax           59   /* number of rows in conversation buffer */

#define minkey          1   /* minimum function key number */
#define maxkey          37   /* maximum function key number */
#define vkdlim          2048   /* characters available for definitions */
#define keyleadin       atsign   /* function key lead-in on command line */

/* following are for use with "getinf" and CURLEW-specific */
#define clbuffer        11   /* CURLEW :: edit buffer filename */
#define clclone         12   /* CURLEW :: clone buffer filename */
#define clprofile       13   /* CURLEW :: profile filename */
#define clsaveone       14   /* CURLEW :: first safety filename */
#define clsavetwo       15   /* CURLEW :: second safety filename */
#define cljournal       16   /* CURLEW :: journal filename */
#define clhelpcmd       17   /* CURLEW :: system help command */
#define clhelpfn        18   /* CURLEW :: simple help file */
#define clsitedef       19   /* CURLEW :: site default overrides */
#define clrensave       20   /* CURLEW :: rename save file switch */
#define clwidchk        21   /* CURLEW :: check line width overflow */




/* ****************************************************** cltype ***** */
/* *                                                                 * */
/* *                     CURLEW :: Editor Types                      * */
/* *                                                                 * */
/* ******************************************************************* */


/* negotiation reply */
/* context editor */
/* full-screen editor request */
typedef enum {
  entry_, coned, fsereq, fsecmd
} astype;   /* context command from full-screen editor */
/* no more text to scan */
/* failure */
typedef enum {
  enddata, err, ok
} stcode;   /* success (so far!) */
/* no special note */
/* pattern search failed */
typedef enum {
  basic, search, lrange
} errtype;   /* line parameter out of range */
/* context editor command */
/* new line for a/c/i command */
/* y/n reply to e command */
/* y/n reply to q command */
typedef enum {
  concmd, conapp, conecmd, conqcmd, paused
} cstype;   /* replying to page-full prompt (querymore) */
typedef enum {
  badcmd, gcmd, xcmd, acmd, ccmd, cocmd, cucmd, clcmd, dcmd, ecmd, excmd,
  fcmd, focmd, hcmd, icmd, kcmd, lcmd, mcmd, pcmd, prcmd, qcmd, rcmd, scmd,
  secmd, shcmd, stcmd, ucmd, vcmd, wcmd, yesrep, norep, okrep, canrep
} cmdtype;
/* normal full-screen operation */
typedef enum {
  normal, blkmode
} fstype;   /* a block has been marked */
/* no mousetrap set */
/* block ^Q or ^W */
typedef enum {
  safe, armed, sprung
} traptype;   /* still no action, must reset block mode */
typedef uchar masktype[del - nul + 1];   /* for control character mask */


#define mkenable        false


/* fill; left justify (ragged right margin) */
/* fill; justify both margins */
/* centre text within margins */
typedef enum {
  lj, jb, cen, rj
} fmttype;   /* quad right (ragged left margin) */


/* Local variables for editor: */
struct LOC_editor {


  /* ****************************************************** clvar ****** */
  /* *                                                                 * */
  /* *                   CURLEW :: Editor Variables                    * */
  /* *                                                                 * */
  /* ******************************************************************* */


  boolean screenmode;   /* true if frames may be fetched */
  uchar tokencode;   /* set to 1 for restricted transfer */
  astype appstate;   /* application processing state */
  cstype cstate;   /* context editor processing state */
  fstype fstate;   /* full-screen editor processing state */
  long filenbr;   /* number of files edited this session */

  /* mode arrays */
  modearray ctxtmode;   /* mode array for editor commands */
  modearray nfsomode;   /* mode array for normal f/screen operation */
  modearray blokmode;   /* mode array for block operation */
  modearray bblkmode;   /* modified version for basic block */

  /* screen positions */
  char toprow;   /* top row of conversation or file display */
  char botrow;   /* bottom row of file display */
  char pinrow;   /* preferred insertion row */
  char cmdrow;   /* row from which commands are read */
  uchar cmdcol;   /* ... and the command start column */

  /* file buffer */
  long buftext[maxlines + 1];   /* line locations */
  uchar bufmark[maxlines + 1];   /* line markers */

  /* checkpoint buffer */
  long cptext[maxlines + 1];   /* checkpoint text */
  long cplstln;   /* checkpoint last line */
  long cpcurln;   /* checkpoint current line */
  long cptopln;   /* checkpoint top of screen line */
  char cpfsrow;   /* checkpoint f/s row */
  uchar cpfscol;   /* checpoint f/s column */
  boolean cpchanged;   /* checkpoint changed flag */
  boolean cpneeded;   /* checkpoint and buffer differ flag */

  /* buffer scratch file */
  short scrout;   /* scratch output fd */
  short scrin;   /* scratch input fd */
  long recout;   /* next record to write to scrout */
  boolean bufferok;   /* true if buffer passes integrity checks */
  boolean widcheck;   /* true record truncation is to be checked */
  boolean crecdlen;   /* true if record truncation problem */
  boolean cfilelen;   /* true is file length truncation problem */

  /* editor buffer line parameters */
  long line1;   /* first line number */
  long line2;   /* second line number */
  long nlines;   /* number of line numbers specified */
  long curln;   /* current line; value of dot */
  long lastln;   /* last line; value of dollar */
  long topln;   /* full-screen; top of display value */
  long itopln;   /* intermediate top line used for '#' */
  boolean topused;   /* true if topline used in computation */

  chstring pat;   /* current search pattern */
  chstring prevlin;   /* previous input line */
  chstring savefile;   /* remembered file name */
  chstring newfile;   /* new file name from edit */

  long cursave, i;
  stcode status;   /* overall command completion status */
  boolean msgdone;   /* true if error message printed already */
  errtype errcode;   /* fine control for error messages */
  boolean changed;   /* true if buffer and file differ */

  /* conversation buffer */
  rowtext blankrow;   /* a completely blank row */
  rowtext cbuf[cbmax + 1];   /* conversation buffer */
  long cbrows;   /* number of rows in conversation buffer */
  long cfirst;   /* first row in conversation buffer */
  long cnext;   /* conversation buffer row for next char */
  long cbidx;   /* array index derived from cnext */
  long cindent;   /* indent for subsequent lines */
  uchar ccol;   /* column in row for next character */
  boolean cblfull;   /* true if current row is full to maxcol */
  boolean pausing;   /* true if pausing when screen full */
  long cpause;   /* pause after this many rows */

  /* full-screen current position and line cache */
  char fsrow;   /* full-screen cursor row tracking */
  uchar fscol;   /* full-screen cursor column tracking */
  long fsline;   /* full-screen active line, if not zero */
  chstring fstext;   /* text associated with fsline */
  boolean fschanged;   /* true if fstext differs from buffer */
  chstring vtitle;   /* full-screen title if set */
  boolean tokreq;   /* true if the token has been requested */
  boolean fscmask;   /* true if c/chars masked in f/s operation */
  masktype mask;   /* control character mask */

  /* block mode */
  long blktopln;   /* top line as block mode entered */
  char blkrow;   /* block mark cursor row */
  uchar blkcol;   /* block mark cursor column */
  long blkoset;   /* left margin column at block mode entry */
  traptype mousetrap;   /* block ^Q or ^W mousetrap */
  boolean vbmark;   /* visual block mark indication */

  /* clone details */
  boolean cloneexists;   /* true if scratch file has been created */
  boolean fileclone;   /* true if current clone in scratch file */
  boolean charclone;   /* true if clone is a character rectangle */
  chstring sclone;   /* used for internal clone storage */

  /* journal file details */
  boolean rebuild;   /* true if session rebuild in progress */
  short jfout;   /* journal file writes (logging) */
  short jfin;   /* journal file reads (recovery) */
  uchar jfprevc;   /* previous command character */
  long jfprevln;   /* previous command line number */

  /* profile parameters */
  tabstop tabcols[zmaxcol + 1];   /* tab stop columns */
  long tabrpt;   /* if non-zero, tab repeat interval */
  boolean detab;   /* true if "gettxt" is to expand tabs */
  boolean entab;   /* true if "write" is to compress with tabs */
  boolean htkeys;   /* true if horizontal tab keys available */
  boolean gblfmt;   /* true for "fo b", false for "fo l" */
  long gblwidth;   /* default width for "fo" command */
  long gblpara;   /* default paragraph indent for "fo" */
  long gbloset;   /* default offset for "fo" command */
  long gbleos;   /* default extra sentence spaces for "fo" */
  boolean blkpara;   /* true if ^B..^P is to paragraph */
  long rrend;   /* secondary SELECTGR code */
  long mlevel;   /* control character entry mask */
  boolean rappend;   /* true for RETURN appends (else splits) */
  boolean ciview;   /* true for char insertion after "view" */
  boolean loopsearch;   /* true for search loop over whole buffer */
  boolean alttracks;   /* true for UNIX(tm) chicken tracks */
  boolean acsearch;   /* true for anycase search */
  boolean autoimage;   /* true for host-initiated image correction */
  boolean autoview;   /* automatic "v" command switch */
  boolean inswrap;   /* insert word wrap switch */
  long cscroll;   /* vertical cursor movement action */
  long xoset;   /* horizontal scroll offset */

  long kptrs[maxkey - minkey + 1];   /* f/key pointers */
  uchar kdefs[vkdlim];   /* f/key text */
  long kdmax;   /* characters used in vkdefs */
  boolean kpick[maxkey - minkey + 1];   /* cmd pickup flags */
} ;


/* ****************************************************** cljournl *** */
/* *                                                                 * */
/* *     CURLEW :: Routines to access the editor journal file        * */
/* *                                                                 * */
/* ******************************************************************* */


/* jfcontrol -- control open status of journal file */
Local Void jfcontrol(iomode, LINK)
long iomode;
struct LOC_editor *LINK;
{
  chstring jfilename;
  boolean junk;

  getinf((long)cljournal, jfilename, (long)maxstr);
  if (jfilename[0] == endstr) {  /* no journal file available */
    LINK->jfin = ioerror_;
    LINK->jfout = ioerror_;
  } else if (iomode == ioread) {
    LINK->jfin = create(jfilename, iomode);
    LINK->jfout = ioerror_;
  } else if (iomode == iowrite) {
    if (LINK->jfin != ioerror_) {
      closef(LINK->jfin);
      LINK->jfin = ioerror_;
    }
    if (LINK->jfout != ioerror_)
      closef(LINK->jfout);
    LINK->jfout = create(jfilename, (long)iowrite);
  } else if (iomode == ioappend) {
    if (LINK->jfin != ioerror_) {
      closef(LINK->jfin);
      LINK->jfin = ioerror_;
    }
    if (LINK->jfout != ioerror_)
      closef(LINK->jfout);
    LINK->jfout = openf(jfilename, (long)ioappend);
  } else {
    closef(LINK->jfout);
    LINK->jfout = openf(jfilename, (long)iowrite);
	/* empty, remove may fail */
    closef(LINK->jfout);
    junk = remove(jfilename);   /* if you can */
  }
  LINK->jfprevc = nul;   /* no cached command */

  /* recovery check open */
  /* nil recovery open */
  /* post-recovery open, "k" save */
  /* end of session clean up */
}  /*jfcontrol*/


/* jwrite -- write command to journal file */
Local Void jwrite(ln1, ln2, cmd, s, LINK)
long ln1, ln2;
uchar cmd;
uchar *s;
struct LOC_editor *LINK;
{
  chstring js, pdel;
  boolean oneshot;
  uchar delim;
  long nd, j, k, l;

  switch (cmd) {

  case leta:
  case letc:   /* append, change */
    j = itoc(ln1, js, 1L);
    js[j - 1] = cmd;
    oneshot = (length_(s) < maxstr - 48);

    if (oneshot) {   /* room, find a delimiter */
      setstring(pdel, "/:#$&                   ");
      k = 1;
      while (s[k - 1] != endstr) {
	l = 1;
	do {
	  if (pdel[l - 1] == s[k - 1])
	    pdel[l - 1] = blank;
	  l++;
	} while (pdel[l - 1] != endstr);
	k++;
      }
      l = 1;
      while (pdel[l - 1] != endstr && pdel[l - 1] == blank)
	l++;
      delim = pdel[l - 1];
      oneshot = (delim != endstr);
    }

    if (oneshot) {   /* found a delimiter */
      js[j] = delim;
      scopy(s, 1L, js, j + 2);
    } else {  /* preferred delimiters in s */
      js[j] = newline;
      js[j + 1] = endstr;
      putstr(js, LINK->jfout);
      putstr(s, LINK->jfout);
      js[0] = period;
      js[1] = newline;
      js[2] = endstr;
    }
    break;

  case letd:   /* delete */
    nd = itoc(ln1, js, 1L);
    j = nd;
    if (ln2 > ln1) {
      js[nd - 1] = comma;
      j = itoc(ln2, js, nd + 1);
    }
    js[j - 1] = letd;
    js[j] = newline;
    js[j + 1] = endstr;
    break;

  case lete:
  case letf:   /* edit, file */
    js[0] = cmd;
    js[1] = blank;
    scopy(s, 1L, js, 3L);
    j = length_(js);
    js[j] = newline;
    js[j + 1] = endstr;
    break;

  case letk:
  case letu:   /* keep, undo */
    js[0] = cmd;
    js[1] = newline;
    js[2] = endstr;
    break;

  }/*case*/
  putstr(js, LINK->jfout);
}  /*jwrite*/


/* journal -- accept journal log request and optimise */
Local Void journal(ln1, ln2, cmd, s, LINK)
long ln1, ln2;
uchar cmd;
uchar *s;
struct LOC_editor *LINK;
{
  uchar tc;
  chstring nl;

  if (LINK->jfout == ioerror_)
    return;
  tc = cmd;
  nl[0] = newline;
  nl[1] = endstr;
  if (LINK->jfprevc == letd) {
    if (tc == leta && LINK->jfprevln == ln1 + 1) {
      jwrite(LINK->jfprevln, 0L, letc, s, LINK);
      tc = nul;
    } else
      jwrite(LINK->jfprevln, LINK->jfprevln, LINK->jfprevc, nl, LINK);
    LINK->jfprevc = nul;
  }

  if (tc == nul)
    return;
  if (tc == letd) {
    if (ln1 == ln2) {
      LINK->jfprevc = tc;
      LINK->jfprevln = ln1;
    }
  }
  if (LINK->jfprevc == nul)
    jwrite(ln1, ln2, tc, s, LINK);
}  /*journal*/


/* ****************************************************** clwhite **** */
/* *                                                                 * */
/* *             CURLEW :: White space (tab) conversion              * */
/* *                                                                 * */
/* ******************************************************************* */


/* expand -- expansion of tabulation stops in buffer text */
Local Void expand(s, LINK)
uchar *s;
struct LOC_editor *LINK;
{
  uchar c;
  long i, j, k;
  uchar tc;
  chstring t;
  boolean fini;

  fini = false;
  i = 1;
  j = 1;
  do {
    c = s[i - 1];
    i++;
    if (c != tab) {
      t[j - 1] = c;
      j++;
    } else {
      k = 1;
      if (LINK->tabrpt > 0) {
	k = LINK->tabrpt - (j - 1) % LINK->tabrpt;
/* p2c: cl-sun.p, line 3722:
 * Note: Using % for possibly-negative arguments [317] */
      } else {
	tc = j;
	while (tc < maxcol() && LINK->tabcols[tc] != tabset)
	  tc++;
	if (LINK->tabcols[tc] == tabset)
	  k = tc - j + 1;
      }
      while (k > 0 && j < maxstr) {
	t[j - 1] = blank;
	j++;
	k--;
      }
    }
    fini = (c == endstr || j >= maxstr);
  } while (!fini);
  if (t[j - 2] != endstr) {
    t[j - 3] = newline;
    t[j - 2] = endstr;
  }
  scopy(t, 1L, s, 1L);
}  /*expand*/


/* compress -- compression of white space into tab characters */
Local Void compress(s, LINK)
uchar *s;
struct LOC_editor *LINK;
{
  long i, j, k, nch;
  uchar tc;
  chstring t;

  i = 1;
  j = 1;
  k = 0;
  do {
    k = i;
    while (s[i - 1] == blank)
      i++;
    if (i > k) {
      nch = 0;
      if (i < k + 3)
	nch = i - k;
      else if (LINK->tabrpt > 0) {
	while (i > k) {
	  nch++;
	  if (k % LINK->tabrpt == 0) {
/* p2c: cl-sun.p, line 3774:
 * Note: Using % for possibly-negative arguments [317] */
	    t[j - 1] = tab;
	    j++;
	    nch = 0;
	  }
	  k++;
	}
      } else {
	while (i > k) {
	  nch++;
	  if (k <= maxcol()) {
	    tc = k;
	    if (LINK->tabcols[tc] == tabset) {
	      t[j - 1] = tab;
	      j++;
	      nch = 0;
	    }
	  }
	  k++;
	}
      }
      while (nch > 0) {
	t[j - 1] = blank;
	j++;
	nch--;
      }
    }
    while (s[i - 1] != blank && s[i - 1] != endstr) {
      t[j - 1] = s[i - 1];
      i++;
      j++;
    }
  } while (s[i - 1] != endstr);
  t[j - 1] = endstr;
  scopy(t, 1L, s, 1L);
}  /*compress*/


/* ****************************************************** cledbuf **** */
/* *                                                                 * */
/* *             CURLEW :: Edit Buffer Management Routines           * */
/* *                                                                 * */
/* ******************************************************************* */


/* checkpoint -- checkpoint buffer for subsequent undo command */
Local Void checkpoint(LINK)
struct LOC_editor *LINK;
{
  long j;
  chstring ns;
  long FORLIM;

  if (LINK->jfout != ioerror_)
    secure(LINK->jfout);
  if (LINK->cpneeded) {   /* only copy array if changed */
    FORLIM = LINK->lastln;
    for (j = 0; j <= FORLIM; j++)
      LINK->cptext[j] = LINK->buftext[j];
    LINK->cpneeded = false;
    journal(0L, 0L, letk, ns, LINK);
  }
  LINK->cplstln = LINK->lastln;
  LINK->cpcurln = LINK->curln;
  if (LINK->appstate == coned) {
    LINK->cptopln = LINK->curln;
    LINK->cpfsrow = LINK->toprow;
    LINK->cpfscol = 0;
  } else {
    LINK->cptopln = LINK->topln;
    LINK->cpfsrow = LINK->fsrow;
    LINK->cpfscol = LINK->fscol;
  }
  LINK->cpchanged = LINK->changed;
  ns[0] = endstr;
}  /*checkpoint*/


/* setbuf -- create scratch file and set up line zero */
Local Void setbuf(LINK)
struct LOC_editor *LINK;
{
  chstring edittemp;
  boolean junk;

  getinf((long)clbuffer, edittemp, (long)maxstr);
  LINK->scrout = create(edittemp, (long)iowrite);
  LINK->scrin = openf(edittemp, (long)ioread);
  if (LINK->scrout == ioerror_ || LINK->scrin == ioerror_) {
    erasedisplay();
    soundalarm();
    setcursor(maxrow() - 4, 2);
    literal("*** Edit buffer initiali");
    literal("sation failed :: abandon");
    literal("ing session !!&         ");
    stopssmp();
    errorexit();
  }

  LINK->bufferok = true;
  junk = getindex(&LINK->recout, LINK->scrout);
  LINK->curln = 0;
  LINK->lastln = 0;
  LINK->changed = false;
  LINK->cpneeded = true;

  LINK->crecdlen = false;
  LINK->cfilelen = false;
  getinf((long)clwidchk, edittemp, (long)maxstr);
  LINK->widcheck = (edittemp[0] == lety);

  LINK->buftext[0] = 0;
  checkpoint(LINK);
}  /*setbuf*/


/* getmark -- get mark from nth line */
Local boolean getmark(n, LINK)
long n;
struct LOC_editor *LINK;
{
  return (LINK->bufmark[n] == plus);
}  /*getmark*/


/* putmark -- put mark m on nth line */
Local Void putmark(n, m, LINK)
long n;
boolean m;
struct LOC_editor *LINK;
{
  if (m)
    LINK->bufmark[n] = plus;
  else
    LINK->bufmark[n] = minus;
}  /*putmark*/


/* reverse -- reverse buffer lines n1..n2 */
Local Void reverse(n1, n2, LINK)
long n1, n2;
struct LOC_editor *LINK;
{
  long temptext;
  uchar tempmark;

  while (n1 < n2) {
    temptext = LINK->buftext[n1];
    tempmark = LINK->bufmark[n1];
    LINK->buftext[n1] = LINK->buftext[n2];
    LINK->bufmark[n1] = LINK->bufmark[n2];
    LINK->buftext[n2] = temptext;
    LINK->bufmark[n2] = tempmark;
    n1++;
    n2--;
  }
}  /*reverse*/


/* blkmove -- move block of lines n1..n2 to after n3 */
Local Void blkmove(n1, n2, n3, LINK)
long n1, n2, n3;
struct LOC_editor *LINK;
{
  if (n3 < n1 - 1) {
    reverse(n3 + 1, n1 - 1, LINK);
    reverse(n1, n2, LINK);
    reverse(n3 + 1, n2, LINK);
  } else if (n3 > n2) {
    reverse(n1, n2, LINK);
    reverse(n2 + 1, n3, LINK);
    reverse(n1, n3, LINK);
  }
  LINK->changed = true;   /* all changes call this routine */
  LINK->cpneeded = true;   /* ... and checkpoint is out of date */
}  /*blkmove*/


/* puttxt -- put text from lin after curln */
Local stcode puttxt(lin, LINK)
uchar *lin;
struct LOC_editor *LINK;
{
  stcode Result;
  boolean junk;

  Result = err;
  if (LINK->lastln >= maxlines) {
    LINK->cfilelen = true;
    return Result;
  } else {
    if (LINK->widcheck) {
      if (length_(lin) > maxstr - 2) {
	LINK->crecdlen = true;
	lin[maxstr - 3] = newline;
	lin[maxstr - 2] = endstr;
      }
    }
    LINK->lastln++;
    putstr(lin, LINK->scrout);
    putmark(LINK->lastln, false, LINK);
    LINK->buftext[LINK->lastln] = LINK->recout;
    junk = getindex(&LINK->recout, LINK->scrout);
    blkmove(LINK->lastln, LINK->lastln, LINK->curln, LINK);
    journal(LINK->curln, 0L, leta, lin, LINK);
    LINK->curln++;
    return ok;
  }
  return Result;
}  /*puttxt*/


/* putfiletxt -- put file from fd after curln */
Local stcode putfiletxt(fd, count, LINK)
short fd;
long *count;
struct LOC_editor *LINK;
{
  stcode Result;
  boolean done, junk;
  long start, j, FORLIM;

  Result = err;
  *count = maxlines - LINK->lastln;
  if (*count <= 0)
    return Result;
  done = ffcopy(fd, LINK->scrout, count);
  if (done) {
    if (*count < 0) {   /* check for truncated records */
      *count = -*count;
      if (LINK->widcheck)
	LINK->crecdlen = true;
    }
    start = LINK->lastln + 1;
    FORLIM = *count;
    for (j = 1; j <= FORLIM; j++) {
      LINK->lastln++;
      junk = getindex(&LINK->recout, LINK->scrout);
      LINK->buftext[LINK->lastln] = LINK->recout;
      putmark(LINK->lastln, false, LINK);
    }
    blkmove(start, LINK->lastln, LINK->curln, LINK);
    LINK->curln += *count;
    if (LINK->lastln >= maxlines)
      LINK->cfilelen = true;
    junk = getindex(&LINK->recout, LINK->scrout);   /* for next "puttxt" */
    return ok;
  } else {
    LINK->bufferok = false;
    soundalarm();
    return Result;
  }
  return Result;
}  /*putfiletxt*/


/* gettxt -- get text from line n into s */
Local Void gettxt(n, s, LINK)
long n;
uchar *s;
struct LOC_editor *LINK;
{
  boolean junk;

  if (n < 1 || n > LINK->lastln) {
    s[0] = endstr;
    return;
  }
  seek(LINK->buftext[n], LINK->scrin);
  junk = getline(s, LINK->scrin, (long)maxstr);
  if (LINK->detab)   /* tabs -> space conversion */
    expand(s, LINK);
}  /*gettxt*/


/* copytxt -- copy text line from buffer to file descriptor */
Local stcode copytxt(n, fd, LINK)
long n;
short fd;
struct LOC_editor *LINK;
{
  stcode Result;

  Result = err;
  if (n >= 1 && n <= LINK->lastln) {
    if (ixcopy(LINK->buftext[n], LINK->scrin, fd))
      return ok;
  }
  return Result;
}  /*copytxt*/


/* reptxt -- replace text from lin at lnum */
Local stcode reptxt(lin, lnum, LINK)
uchar *lin;
long lnum;
struct LOC_editor *LINK;
{
  short jsave;
  stcode status;

  LINK->curln = LINK->lastln;
  jsave = LINK->jfout;
  LINK->jfout = ioerror_;   /* inhibit journal write */
  status = puttxt(lin, LINK);
  LINK->jfout = jsave;   /* re-enable journal */
  LINK->curln = min(lnum, LINK->lastln);
  if (status != ok)
    return status;
  LINK->buftext[LINK->curln] = LINK->buftext[LINK->lastln];
  putmark(LINK->curln, false, LINK);
  LINK->lastln--;
  journal(LINK->curln, 0L, letc, lin, LINK);
  return status;
}  /*reptxt*/


/* restore -- restore file from checkpoint */
Local Void restore(LINK)
struct LOC_editor *LINK;
{
  long j, k, ti, tp;
  char tr;
  uchar tc;
  long FORLIM;

  k = min(LINK->lastln, LINK->cplstln);
  for (j = 0; j <= k; j++) {
    tp = LINK->cptext[j];
    LINK->cptext[j] = LINK->buftext[j];
    LINK->buftext[j] = tp;
  }

  if (k < LINK->lastln) {
    FORLIM = LINK->lastln;
    for (j = k + 1; j <= FORLIM; j++)
      LINK->cptext[j] = LINK->buftext[j];
  } else if (k < LINK->cplstln) {
    FORLIM = LINK->cplstln;
    for (j = k + 1; j <= FORLIM; j++)
      LINK->buftext[j] = LINK->cptext[j];
  }

  ti = LINK->cplstln;
  LINK->cplstln = LINK->lastln;
  LINK->lastln = ti;
  ti = LINK->cpcurln;
  LINK->cpcurln = LINK->curln;
  LINK->curln = ti;
  if (LINK->appstate == coned) {
    LINK->topln = LINK->curln;
    LINK->fsrow = LINK->toprow;
    LINK->fscol = 0;
  }
  ti = LINK->cptopln;
  LINK->cptopln = LINK->topln;
  LINK->topln = ti;
  tr = LINK->cpfsrow;
  LINK->cpfsrow = LINK->fsrow;
  LINK->fsrow = tr;
  tc = LINK->cpfscol;
  LINK->cpfscol = LINK->fscol;
  LINK->fscol = tc;

  if (LINK->cpchanged)   /* may not have, but make the user think */
    LINK->changed = true;
  LINK->cpneeded = true;   /* safety */
}  /*restore*/


/* edbufok -- returns an integrity verdict on the edit buffer */
Local boolean edbufok(LINK)
struct LOC_editor *LINK;
{
  long rc;

  if (LINK->bufferok) {
    getfds(&rc, LINK->scrout);
    LINK->bufferok = (rc == 0);
    if (LINK->bufferok) {
      getfds(&rc, LINK->scrin);
      LINK->bufferok = (rc == 0);
    }
  }
  if (!LINK->bufferok)   /* make them look */
    soundalarm();
  return LINK->bufferok;
}  /*edbufok*/


/* clrbuf -- dispose of scratch file */
Local Void clrbuf(LINK)
struct LOC_editor *LINK;
{
  chstring edittemp;
  boolean junk;

  closef(LINK->scrin);
  closef(LINK->scrout);
  getinf((long)clbuffer, edittemp, (long)maxstr);
  junk = remove(edittemp);
}  /*clrbuf*/


/* ****************************************************** cllpar ***** */
/* *                                                                 * */
/* *            CURLEW :: Line number parameter routines             * */
/* *                                                                 * */
/* ******************************************************************* */


/* nextln -- get line after n */
Local long nextln(n, LINK)
long n;
struct LOC_editor *LINK;
{
  if (n >= LINK->lastln)
    return 0;
  else
    return (n + 1);
}  /*nextln*/


/* prevln -- get line before n */
Local long prevln(n, LINK)
long n;
struct LOC_editor *LINK;
{
  if (n <= 0)
    return LINK->lastln;
  else
    return (n - 1);
}  /*prevln*/


/* optpat -- get optional pattern from lin[i] and increment i */
Local stcode optpat(lin, i, LINK)
uchar *lin;
long *i;
struct LOC_editor *LINK;
{
  if (lin[*i - 1] == endstr)
    *i = 0;
  else if (lin[*i] == endstr)
    *i = 0;
  else if (lin[*i] == lin[*i - 1])
    (*i)++;   /* leave existing pattern alone */
  else
    *i = makepat(lin, *i + 1, lin[*i - 1], LINK->pat);
  if (LINK->pat[0] == endstr)
    *i = 0;
  if (*i == 0) {
    LINK->pat[0] = endstr;
    return err;
  } else
    return ok;

  /* repeated delimiter */
}  /*optpat*/


/* patscan -- find next occurence of pattern after line n */
Local stcode patscan(way, n, LINK)
uchar way;
long *n;
struct LOC_editor *LINK;
{
  stcode Result;
  boolean done;
  long pn;
  chstring line;

  *n = LINK->curln;
  Result = err;
  done = false;
  do {
    pn = *n;
    if (way == scan) {  /* forwards */
      *n = nextln(*n, LINK);
      done = (!LINK->loopsearch && pn > *n);
    } else {  /* backwards */
      *n = prevln(*n, LINK);
      done = (!LINK->loopsearch && pn < *n);
    }

    if (done)
      *n = LINK->curln;   /* search loop failed */
    else {  /* check new line's text */
      gettxt(*n, line, LINK);
      if (match(line, LINK->pat)) {
	Result = ok;
	done = true;
      }
    }
  } while (!(*n == LINK->curln || done));   /*patscan*/
  return Result;
}


/* getnum -- get a single line number component */
Local stcode getnum(lin, i, num, status, LINK)
uchar *lin;
long *i, *num;
stcode *status;
struct LOC_editor *LINK;
{
  *status = ok;
  skipbl(lin, i);
  if (isdigit_(lin[*i - 1])) {
    *num = ctoi(lin, i);
    (*i)--;   /* move back, to be advanced at end */
  } else if (lin[*i - 1] == curline)
    *num = LINK->curln;
  else if (lin[*i - 1] == lastline)
    *num = LINK->lastln;
  else if (lin[*i - 1] == topline) {
    LINK->topused = true;
    *num = LINK->itopln;
    LINK->topln = LINK->itopln;
  } else if (lin[*i - 1] == scan || lin[*i - 1] == backscan) {
    if (optpat(lin, i, LINK) == err)
      *status = err;
    else {
      *status = patscan(lin[*i - 1], num, LINK);
      if (*status == err)
	LINK->errcode = search;
    }
  } else
    *status = enddata;
  if (*status == ok)   /* next character to be examined */
    (*i)++;
  return (*status);
}  /*getnum*/


/* getone -- get one line number expression */
Local stcode getone(lin, i, num, status, LINK)
uchar *lin;
long *i, *num;
stcode *status;
struct LOC_editor *LINK;
{
  long istart, mul, pnum;
  stcode junk;

  istart = *i;
  *num = 0;
  LINK->topused = false;
  skipbl(lin, i);
  if (lin[*i - 1] == plus || lin[*i - 1] == minus) {   /* 1st term */
    *num = LINK->curln;
    *status = ok;
  } else
    junk = getnum(lin, i, num, status, LINK);
  while (*status == ok) {
    skipbl(lin, i);
    if (lin[*i - 1] != plus && lin[*i - 1] != minus) {
      *status = enddata;
      continue;
    }
    if (lin[*i - 1] == plus)
      mul = 1;
    else
      mul = -1;
    (*i)++;
    if (getnum(lin, i, &pnum, status, LINK) == enddata) {
      pnum = 1;
      *status = ok;
    }
    if (*status == ok)
      *num += mul * pnum;
  }
  if (LINK->topused) {  /* constrain line number */
    *num = max(*num, 1L);
    *num = min(*num, LINK->lastln);
  }
  if ((unsigned long)(*num) > LINK->lastln) {
    *status = err;
    LINK->errcode = lrange;
  }
  if (*status == err)
    return (*status);
  if (*i <= istart)
    *status = enddata;
  else
    *status = ok;
  return (*status);
}  /*getone*/


/* getlist -- get list of line nums at lin[i] and increment i */
Local stcode getlist(lin, i, status, LINK)
uchar *lin;
long *i;
stcode *status;
struct LOC_editor *LINK;
{
  long num;
  boolean done;

  LINK->line2 = 0;
  LINK->nlines = 0;
  if (LINK->appstate == coned)
    LINK->itopln = LINK->curln;
  else
    LINK->itopln = LINK->topln;
  done = (getone(lin, i, &num, status, LINK) != ok);
  while (!done) {
    LINK->line1 = LINK->line2;
    LINK->line2 = num;
    LINK->nlines++;
    if (lin[*i - 1] == semicol) {
      LINK->curln = num;
      LINK->itopln = LINK->curln;
      LINK->fsrow = LINK->toprow;
    }
    if (lin[*i - 1] == comma || lin[*i - 1] == semicol) {
      (*i)++;
      done = (getone(lin, i, &num, status, LINK) != ok);
    } else
      done = true;
  }
  LINK->nlines = min(LINK->nlines, 2L);
  if (LINK->nlines == 0)
    LINK->line2 = LINK->curln;
  if (LINK->nlines <= 1)
    LINK->line1 = LINK->line2;
  if (*status != err)
    *status = ok;
  return (*status);
}  /*getlist*/


/* default -- set defaulted line numbers */
Local stcode default_(def1, def2, status, LINK)
long def1, def2;
stcode *status;
struct LOC_editor *LINK;
{
  if (LINK->nlines == 0) {
    LINK->line1 = def1;
    LINK->line2 = def2;
  }
  if (LINK->line1 > LINK->line2 || LINK->line1 <= 0)
    *status = err;
  else
    *status = ok;
  return (*status);
}  /*default*/


/* ****************************************************** cldelete *** */
/* *                                                                 * */
/* *    CURLEW :: line deletion, called also for line replacement    * */
/* *                                                                 * */
/* ******************************************************************* */


/* lndelete -- delete lines n1 through n2 */
Local stcode lndelete(n1, n2, status, LINK)
long n1, n2;
stcode *status;
struct LOC_editor *LINK;
{
  chstring ns;

  if (n1 <= 0) {
    *status = err;
    return (*status);
  }
  blkmove(n1, n2, LINK->lastln, LINK);
  LINK->lastln += n1 - n2 - 1;
  LINK->curln = prevln(n1, LINK);
  ns[0] = endstr;
  journal(n1, n2, letd, ns, LINK);
  *status = ok;
  return (*status);
}  /*lndelete*/


/* ****************************************************** clceirtn *** */
/* *                                                                 * */
/* *         CURLEW :: Context editor screen image routines          * */
/* *                                                                 * */
/* ******************************************************************* */


/* edname -- put up the editor name */
Local Void edname(LINK)
struct LOC_editor *LINK;
{
  setcursor(0, 0);
  if (LINK->rrend != defrend)
    setmode(selectgr, (int)LINK->rrend);
  literal(" Curlew &               ");
  if (LINK->rrend != defrend)
    setmode(selectgr, defrend);
  textchar(colon);
}  /*edname*/


/* edprompt -- put up the editor command prompt */
Local Void edprompt(LINK)
struct LOC_editor *LINK;
{
  uchar ckcol;

  setcursor(LINK->cmdrow, 0);
  switch (LINK->cstate) {

  case concmd:
  case conapp:
    ckcol = 2;
    if (LINK->cstate == concmd) {
      if (LINK->appstate == fsecmd) {
	literal("Enter command &         ");
	ckcol = 16;
      }
      textchar(cmdprompt);
    } else
      textchar(txtprompt);
    textchar(blank);
    break;

  case conecmd:
  case conqcmd:
    literal("Discard changes?  (y/n):");
    textchar(blank);
    ckcol = 25;
    break;

  case paused:
    setmode(selectgr, (int)LINK->rrend);
    literal(" More output?  (y/n): & ");
    setmode(selectgr, defrend);
    textchar(blank);
    ckcol = 23;
    break;

  }/*case*/

  erasetoright();
  if (LINK->cmdcol != ckcol) {
    LINK->cmdcol = ckcol;
    setupdate(LINK->cmdrow, LINK->cmdrow, LINK->cmdcol, maxcol());
  }
  if (LINK->appstate != fsereq)   /* may set/reset char insertion */
    putmode(LINK->ctxtmode);
}  /*edprompt*/


/* ctxtimage -- set up screen image for context operation */
Local Void ctxtimage(LINK)
struct LOC_editor *LINK;
{
  uchar c, FORLIM;

  LINK->appstate = coned;
  putmode(LINK->ctxtmode);
  setcursor(0, 0);
  erasetoright();
  edname(LINK);
  literal(" Full-screen text editor");
  setcursor(1, 0);
  FORLIM = maxcol();
  for (c = 0; c <= FORLIM; c++)
    textchar(minus);
  edprompt(LINK);
}  /*ctxtimage*/


/* ****************************************************** clfsirtn *** */
/* *                                                                 * */
/* *       CURLEW :: Full-screen image and line cache routines       * */
/* *                                                                 * */
/* ******************************************************************* */


/* header -- put header line on screen */
Local Void header(LINK)
struct LOC_editor *LINK;
{
  chstring s;
  rowtext ln;

  if (LINK->vtitle[0] != newline)
    scopy(LINK->vtitle, 1L, s, 1L);
  else if (LINK->savefile[0] == endstr)
    setstring(s, "-- no filename --&      ");
  else
    scopy(LINK->savefile, 1L, s, 1L);
  memcpy(ln, LINK->blankrow, sizeof(rowtext));
  slcopy(s, 1L, ln, 0, maxcol() - 25L);
  putrow(0, ln);
}  /*header*/


/* vbmreset -- reset visual block mark if on-screen */
Local Void vbmreset(LINK)
struct LOC_editor *LINK;
{
  rowtext ln;
  long bline;
  char rr;

  if (!LINK->vbmark)
    return;
  bline = LINK->blktopln + LINK->blkrow - LINK->topln;
  if (bline >= LINK->toprow && bline <= LINK->botrow) {
    rr = bline;
    getrow(rr, ln);
    putrow(rr, ln);
    setcursor(LINK->fsrow, LINK->fscol);
  }
  LINK->vbmark = false;
}  /*vbmreset*/


/* markblock -- show position of block mark */
Local Void markblock(LINK)
struct LOC_editor *LINK;
{
  boolean tb;
  long by, bx, bxx;
  rowtext ln;
  char rr;
  uchar cc;
  modearray cmode;

  tb = mkenable;
  if (LINK->rrend != negative)   /* not worth it otherwise */
    return;
  if (!tb)   /* locked-out by mkenable */
    return;
  by = LINK->blktopln + LINK->blkrow - LINK->topln;
  bx = LINK->blkoset - LINK->xoset + LINK->blkcol;
  if (by < LINK->toprow || by > LINK->botrow)
    return;
  getrow((int)by, ln);
  getloc(&rr, &cc);
  getmode(cmode);
  if (cmode[selectgr] != negative)
    setmode(selectgr, (int)LINK->rrend);
  if (bx > 0) {  /* leading mark */
    bxx = min(bx - 1, (long)maxcol());
    setcursor((int)by, (int)bxx);
    textchar(ln[bxx]);
  }
  if (bx < maxcol()) {  /* trailing mark */
    bxx = max(bx + 1, 0L);
    setcursor((int)by, (int)bxx);
    textchar(ln[bxx]);
  }
  if (cmode[selectgr] != negative)
    setmode(selectgr, defrend);
  setcursor(rr, cc);
  LINK->vbmark = true;
}  /*markblock*/

#undef mkenable


/* viewstate -- set f/s operating state and reflect in header */
Local Void viewstate(nstate, LINK)
fstype nstate;
struct LOC_editor *LINK;
{
  LINK->fstate = nstate;
  setcursor(0, maxcol() - 20);
  erasetoright();
  if (LINK->fstate == normal) {
    putmode(LINK->nfsomode);
    return;
  }
  setmode(selectgr, (int)LINK->rrend);
  markblock(LINK);
  literal(" block: &               ");
  setmode(selectgr, defrend);
  textchar(blank);
  outdec(LINK->blktopln + LINK->blkrow - LINK->toprow);
  literal(" at &                   ");
  outdec(LINK->blkcol + LINK->blkoset + 1);
  putmode(LINK->blokmode);
}  /*viewstate*/


/* ruler -- put ruler line on screen */
Local Void ruler(rr, LINK)
char rr;
struct LOC_editor *LINK;
{
  chstring s;
  rowtext ln;
  uchar cc;
  long j, k, lclxoset, roset, nd;

  setstring(s, "====.====|====.====|&   ");
  lclxoset = LINK->xoset;
  if (rr > maxrow() / 2)
    lclxoset = 0;
  roset = lclxoset % 10 + 1;
/* p2c: cl-sun.p, line 4658:
 * Note: Using % for possibly-negative arguments [317] */
  s[roset + 9] = endstr;
  j = 0;
  k = maxcol() + 1;
  memcpy(ln, LINK->blankrow, sizeof(rowtext));
  while (k > 0) {  /* build ruler pattern */
    slcopy(s, roset, ln, (int)j, k);
    j += 10;
    k -= 10;
  }
  if (lclxoset > 0) {  /* calibrate first ruler bar */
    cc = 0;
    while (ln[cc] != bar)
      cc++;
    j = lclxoset + cc + 1;
    s[0] = blank;
    nd = itoc(j, s, 2L);
    s[nd - 1] = blank;
    s[nd] = endstr;
    slcopy(s, 1L, ln, cc + 1, 10L);
  }
  setmode(selectgr, (int)LINK->rrend);
  putrow(rr, ln);
  setmode(selectgr, defrend);
}  /*ruler*/


/* filetext -- put file text on screen for row rr */
Local Void filetext(rr, LINK)
char rr;
struct LOC_editor *LINK;
{
  chstring s;
  rowtext ln, oldln;
  long j, k, mc, c;
  boolean sdiffers, remblank;

  memcpy(ln, LINK->blankrow, sizeof(rowtext));
  j = LINK->topln + rr - 2;
  if (j <= LINK->lastln) {
    gettxt(j, s, LINK);
    LINK->i = 1;
    while (s[LINK->i - 1] != newline && LINK->i <= LINK->xoset)
      LINK->i++;
    slcopy(s, LINK->i, ln, 0, maxcol() + 1L);
  }
  getrow(rr, oldln);
  sdiffers = false;
  j = 0;
  mc = maxcol();
  while (j <= mc && !sdiffers) {
    if (ln[j] != oldln[j])
      sdiffers = true;
    else
      j++;
  }
  if (!sdiffers)
    return;
  k = j;
  remblank = true;
  while (k <= mc && remblank) {
    if (ln[k] != blank)
      remblank = false;
    k++;
  }
  if (remblank) {
    setcursor(rr, (int)j);
    erasetoright();
    return;
  }
  if (j < mc / 4) {
    putrow(rr, ln);
    return;
  }
  k = mc;
  while (k > j && ln[k] == blank)
    k--;
  setcursor(rr, (int)j);
  erasetoright();
  for (c = j; c <= k; c++)
    textchar(ln[c]);
}  /*filetext*/


/* replace -- if changed, replace active line in file buffer */
Local Void replace(LINK)
struct LOC_editor *LINK;
{
  stcode stat;

  if (LINK->fschanged) {
    stat = reptxt(LINK->fstext, LINK->fsline, LINK);
    LINK->fschanged = false;
  }
  LINK->fsline = 0;   /* mark cache line invalid */
}  /*replace*/


/* fileimage -- conditionally correct the file image */
Local Void fileimage(LINK)
struct LOC_editor *LINK;
{
  char r, FORLIM;

  replace(LINK);   /* safety */
  FORLIM = LINK->botrow;
  for (r = LINK->toprow; r <= FORLIM; r++)
    filetext(r, LINK);
  if (LINK->fstate == blkmode)   /* in case it's been overwritten */
    markblock(LINK);
}  /*fileimage*/


/* fetch -- get line and text for row at r */
Local Void fetch(r, LINK)
char r;
struct LOC_editor *LINK;
{
  long line, j;
  stcode stat;
  chstring bs;

  line = LINK->topln + r - LINK->toprow;
  if (line == LINK->fsline)
    return;
  replace(LINK);   /* if changed, replace text */
  if (line > LINK->lastln) {
    bs[0] = newline;
    bs[1] = endstr;
    LINK->curln = LINK->lastln;
    for (j = LINK->lastln + 1; j <= line; j++)
      stat = puttxt(bs, LINK);
  }
  gettxt(line, LINK->fstext, LINK);   /* text of active line */
  LINK->fsline = line;   /* ... and buffer line number */
}  /*fetch*/


/* ****************************************************** clinform *** */
/* *                                                                 * */
/* *             CURLEW :: I/O Status Information Routines           * */
/* *                                                                 * */
/* ******************************************************************* */


/* inform -- display literal, conditionally transfer token */
Local Void inform(tag, LINK)
Char *tag;
struct LOC_editor *LINK;
{
  setcursor(maxrow(), maxcol() - 15);
  setmode(selectgr, (int)LINK->rrend);
  textchar(blank);
  literal(tag);
  textchar(blank);
  setcursor(maxrow(), 0);   /* restricted token transfer */
  sendtoken(1);
}  /*inform*/


/* reclaim -- reclaim token after information message */
Local long reclaim(LINK)
struct LOC_editor *LINK;
{
  uchar pc;
  primpar pars;

  do {   /* fetch next primitive */
    getprimitive(&pc, pars);
  } while (pc != capa);   /* got the token back */
  if (pars[0] > 128) {  /* restart; fix-up required */
    if (LINK->appstate == coned)
      edname(LINK);
    else {
      if (LINK->rrend != defrend) {
	ruler(LINK->toprow - 1, LINK);
	ruler(LINK->botrow + 1, LINK);
      }
    }
  }
  setcursor(maxrow(), maxcol() - 15);
  setmode(selectgr, defrend);
  erasetoright();
  return (pars[0]);
}  /*reclaim*/


/* ****************************************************** clconbuf *** */
/* *                                                                 * */
/* *             CURLEW :: Conversation buffer routines              * */
/* *                                                                 * */
/* ******************************************************************* */


/* flushcb -- flush conversation buffer to screen */
Local Void flushcb(LINK)
struct LOC_editor *LINK;
{
  long cl, nd, md;
  uchar lcol;
  char lrow;
  uchar FORLIM;

  md = LINK->cmdrow - LINK->toprow;   /* maximum rows available */
  nd = LINK->cnext - LINK->cfirst;   /* potential number to display */
  if (!LINK->pausing && nd > md / 2) {
    LINK->cfirst = LINK->cnext - 2;
    LINK->cbidx = LINK->cfirst % LINK->cbrows;
    FORLIM = maxcol();
/* p2c: cl-sun.p, line 4864:
 * Note: Using % for possibly-negative arguments [317] */
    for (lcol = 0; lcol <= FORLIM; lcol++)
      LINK->cbuf[LINK->cbidx][lcol] = period;
    nd = 2;
  }
  if (nd > md) {   /* too many */
    nd = md;
    LINK->cfirst = LINK->cnext - md;
  }
  if (nd < md)   /* less than display, keep overlap */
    scrollup(LINK->toprow, LINK->cmdrow - 1, (int)nd);
  lrow = LINK->cmdrow - nd;   /* first new row */
  cl = LINK->cfirst;   /* first buffer text row index */
  while (cl != LINK->cnext) {
    LINK->cbidx = cl % LINK->cbrows;
/* p2c: cl-sun.p, line 4880:
 * Note: Using % for possibly-negative arguments [317] */
    putrow(lrow, LINK->cbuf[LINK->cbidx]);
    cl++;
    lrow++;
  }
}  /*flushcb*/


/* lastcb -- flush last line of conversation buffer to screen */
Local Void lastcb(LINK)
struct LOC_editor *LINK;
{
  rowtext ln;
  long j;

  LINK->cbidx = (LINK->cnext - 1) % LINK->cbrows;
/* p2c: cl-sun.p, line 4894:
 * Note: Using % for possibly-negative arguments [317] */
  memcpy(ln, LINK->cbuf[LINK->cbidx], sizeof(rowtext));
  if (ln[0] == blank && ln[1] == blank) {
    ln[0] = lbrack;
    j = maxcol() - 2;
    while (ln[j] == blank)
      j--;
    ln[j + 2] = rbrack;
  }
  putrow(maxrow(), ln);
}  /*lastcb*/


/* showposn -- returns column of next character to be written */
Local uchar showposn(LINK)
struct LOC_editor *LINK;
{
  return LINK->ccol;
}  /*showposn*/


/* showcmd -- log command line to conversation buffer */
Local Void showcmd(ln, LINK)
uchar *ln;
struct LOC_editor *LINK;
{
  memcpy(LINK->cbuf[0], ln, sizeof(rowtext));
  LINK->cfirst = 0;
  LINK->cnext = 1;
  LINK->cbidx = LINK->cnext;
  LINK->ccol = 0;
  LINK->cindent = 0;
  LINK->cblfull = false;
  LINK->pausing = true;
  LINK->cpause = maxrow() - 4;
}  /*showcmd*/


/* querymore -- ask user whether further verification is required */
Local Void querymore(LINK)
struct LOC_editor *LINK;
{
  rowtext ln;
  uchar c;
  long tcode;

  LINK->cstate = paused;
  do {
    edprompt(LINK);
    sendtoken(0);
    tcode = reclaim(LINK);
    getrow(LINK->cmdrow, ln);
    c = lowercase(ln[LINK->cmdcol]);
  } while (tcode != ctlm || c != blank && c != lety && c != letn);
  LINK->cstate = concmd;

  if (c == letn)
    LINK->pausing = false;
  setcursor(LINK->cmdrow, 0);
  erasetoright();
}  /*querymore*/


/* shownl -- handle newline action in conversation buffer */
Local Void shownl(LINK)
struct LOC_editor *LINK;
{
  uchar cc;
  rowtext ln;
  uchar FORLIM;

  if (LINK->ccol < maxcol()) {
    FORLIM = maxcol();
    for (cc = LINK->ccol; cc <= FORLIM; cc++)
      LINK->cbuf[LINK->cbidx][cc] = blank;
/* p2c: cl-sun.p, line 4962:
 * Note: Evaluating FOR loop limit before initial value [315] */
  }
  if (LINK->pausing && LINK->cnext >= LINK->cpause) {
    memcpy(ln, LINK->cbuf[LINK->cbidx], sizeof(rowtext));
    flushcb(LINK);
    querymore(LINK);
    memcpy(LINK->cbuf[0], ln, sizeof(rowtext));
    LINK->cnext = 0;
  }
  LINK->cnext++;
  LINK->cbidx = LINK->cnext % LINK->cbrows;
/* p2c: cl-sun.p, line 4972:
 * Note: Using % for possibly-negative arguments [317] */
  if (LINK->cindent > 0) {
    FORLIM = LINK->cindent;
    for (cc = 0; cc < FORLIM; cc++)
      LINK->cbuf[LINK->cbidx][cc] = blank;
  }
  LINK->ccol = LINK->cindent;
}  /*shownl*/


/* showc -- display character via conversation buffer */
Local Void showc(c, LINK)
uchar c;
struct LOC_editor *LINK;
{
  if (c == newline) {
    if (LINK->cblfull)
      LINK->cblfull = false;
    else
      LINK->cbuf[LINK->cbidx][LINK->ccol] = blank;
    LINK->cindent = 0;
    shownl(LINK);
    return;
  }
  if (LINK->cblfull) {
    shownl(LINK);
    LINK->cblfull = false;
  }
  LINK->cbuf[LINK->cbidx][LINK->ccol] = c;
  if (LINK->ccol == maxcol())
    LINK->cblfull = true;
  else
    LINK->ccol++;
}  /*showc*/


/* showstr -- display string via conversation buffer */
Local Void showstr(s, LINK)
uchar *s;
struct LOC_editor *LINK;
{
  long j;

  j = 1;
  while (s[j - 1] != endstr) {
    showc(s[j - 1], LINK);
    j++;
  }
}  /*showstr*/


/* showlit -- display literal text via conversation buffer */
Local Void showlit(l, LINK)
Char *l;
struct LOC_editor *LINK;
{
  chstring s;

  setstring(s, l);
  showstr(s, LINK);
}  /*showlit*/


/* showdec -- show integer n r-j in field width w, ts trailing spaces */
Local Void showdec(n, w, ts, LINK)
long n, w, ts;
struct LOC_editor *LINK;
{
  long i, nd;
  chstring s;

  nd = itoc(n, s, 1L);
  for (i = nd; i <= w; i++)
    showc(blank, LINK);
  for (i = 0; i <= nd - 2; i++)
    showc(s[i], LINK);
  if (ts < 0)
    showc(newline, LINK);
  else {
    for (i = 1; i <= ts; i++)
      showc(blank, LINK);
  }
}  /*showdec*/


/* ****************************************************** clgfnrtn *** */
/* *                                                                 * */
/* *       CURLEW :: Get filename and file status verification       * */
/* *                                                                 * */
/* ******************************************************************* */


/* getfn -- get filename from lin[i] */
Local stcode getfn(lin, i, fil, LINK)
uchar *lin;
long *i;
uchar *fil;
struct LOC_editor *LINK;
{
  long k;
  stcode stat;

  stat = err;
  fil[0] = endstr;
  if (lin[*i] == blank) {
    k = getword(lin, *i + 2, fil);   /* get new filename */
    if (k > 0) {
      if (lin[k - 1] == newline)
	stat = ok;
    }
  } else if (lin[*i] == newline && LINK->savefile[0] != endstr) {
    scopy(LINK->savefile, 1L, fil, 1L);
    stat = ok;
  }
  if (stat != ok || LINK->savefile[0] != endstr)
    return stat;
  scopy(fil, 1L, LINK->savefile, 1L);   /* save if no old one */
  if (LINK->appstate == fsereq) {
    header(LINK);
    viewstate(LINK->fstate, LINK);
  }
  return stat;
}  /*getfn*/


/* linestat -- show buffer current and last line numbers */
Local Void linestat(LINK)
struct LOC_editor *LINK;
{
  showlit("  Line &                ", LINK);
  showdec(LINK->topln + LINK->fsrow - LINK->toprow, 1L, 0L, LINK);
  showlit(" at &                   ", LINK);
  showdec(LINK->fscol + LINK->xoset + 1, 1L, 0L, LINK);
  if (LINK->xoset > 0) {
    showc(blank, LINK);
    showc(lparen, LINK);
    showdec(LINK->fscol + 1L, 1L, 0L, LINK);
    showc(rparen, LINK);
  }
  if (LINK->lastln < 1)
    showlit(", buffer empty&         ", LINK);
  else {
    showlit(", last line is &        ", LINK);
    showdec(LINK->lastln, 1L, 0L, LINK);
    if (!edbufok(LINK))
      showlit(", edit buffer damaged!& ", LINK);
    else if (LINK->cfilelen || LINK->crecdlen)
      showlit(", \"write\" disabled...&  ", LINK);
    else if (LINK->changed)
      showlit("  (buffer changed)&     ", LINK);
  }
  showc(newline, LINK);
}  /*linestat*/


/* filestat -- verification of file and buffer status */
Local Void filestat(LINK)
struct LOC_editor *LINK;
{
  if (LINK->appstate == fsereq) {
    header(LINK);
    viewstate(LINK->fstate, LINK);
    linestat(LINK);
    return;
  }
  if (LINK->savefile[0] == endstr)
    showlit("  No saved filename&    ", LINK);
  else
    showlit("  Saved filename     :  ", LINK);
  showstr(LINK->savefile, LINK);
  showc(newline, LINK);
  showlit("  Lines in buffer    :  ", LINK);
  showdec(LINK->lastln, 1L, 4L, LINK);
  if (LINK->changed)
    showlit("(buffer changed)  &     ", LINK);
  showc(newline, LINK);
  if (LINK->cfilelen || LINK->crecdlen) {
    showlit("  *** \"write\" disabled t", LINK);
    showlit("o avoid loss of data fro", LINK);
    showlit("m oversize file@n&      ", LINK);
  }
  if (edbufok(LINK))
    return;
  showlit("  *** \"write\" disabled b", LINK);
  showlit("ecause edit buffer integ", LINK);
  showlit("rity checks failed@n&   ", LINK);
}  /*filestat*/


/* ****************************************************** clacicmd *** */
/* *                                                                 * */
/* *      CURLEW :: "a" (append), "c" (change) and "i" (insert)      * */
/* *                                                                 * */
/* ******************************************************************* */


/* appcpar -- append command parameter text after line */
Local stcode appcpar(line, cmd_, i, glob, LINK)
long line;
uchar *cmd_;
long i;
boolean glob;
struct LOC_editor *LINK;
{
  chstring cmd;
  uchar delim;
  stcode stat;
  chstring inline_;
  long j, lct;
  uchar tc;
  char irow;

  memcpy(cmd, cmd_, sizeof(chstring));
  stat = err;
  lct = 0;
  LINK->curln = line;
  j = i + 1;
  skipbl(cmd, &i);
  delim = cmd[i - 1];
  if (isalphanum(delim)) {  /* no delimiter, add remainder */
    scopy(cmd, j, inline_, 1L);
    stat = puttxt(inline_, LINK);
    if (stat != err)
      lct = 1;
  } else {  /* delimiter, process lines(s) */
    do {
      i++;
      j = 1;
      tc = cmd[i - 1];
      while (tc != delim && tc != newline && tc != endstr) {
	inline_[j - 1] = tc;
	j++;
	i++;
	tc = cmd[i - 1];
      }
      inline_[j - 1] = newline;
      inline_[j] = endstr;
      stat = puttxt(inline_, LINK);
      if (stat != err)
	lct++;
    } while (stat != err && cmd[i] != newline && cmd[i] != endstr);
  }

  if (!glob) {
    showlit("  Text lines added :  & ", LINK);
    showdec(lct, 1L, 0L, LINK);
    showc(newline, LINK);
  }

  if (LINK->appstate != fsereq)
    return stat;
  if (line < LINK->topln || line >= LINK->topln + LINK->botrow - LINK->toprow)
    return stat;
  irow = LINK->toprow + line - LINK->topln + 1;
  if (lct < LINK->botrow - irow)
    scrolldown(irow, LINK->botrow, (int)lct);
  return stat;
}  /*appcpar*/


/* doappend -- append lines after line */
Local stcode doappend(line, cmd, i, glob, LINK)
long line;
uchar *cmd;
long i;
boolean glob;
struct LOC_editor *LINK;
{
  uchar tcol, FORLIM;

  if (!glob && !LINK->rebuild)
    checkpoint(LINK);
  if (cmd[i - 1] != newline)   /* one-shot command */
    return (appcpar(line, cmd, i, glob, LINK));
  else if (glob)
    return err;
  else {
    LINK->curln = line;
    LINK->cstate = conapp;
    if (LINK->rebuild)  /* talking to the user */
      return ok;
    setcursor(0, maxcol() - 35);
    setmode(selectgr, (int)LINK->rrend);
    literal(" Enter text; \".\" to end ");
    literal("insertion. &            ");
    setmode(selectgr, defrend);
    erasetabs();
    FORLIM = maxcol() - 2;
    for (tcol = 0; tcol <= FORLIM; tcol++) {
      if (LINK->tabcols[tcol] == tabset)
	settab(tcol + 2);
    }
    return ok;
  }

  /* too late for global */
  /* prompt for text */
}  /*doappend*/


/* newtext -- insert line of input into buffer (from a/c/i) */
Local stcode newtext(inline__, LINK)
uchar *inline__;
struct LOC_editor *LINK;
{
  chstring inline_;
  stcode stat;
  uchar tc, FORLIM;

  memcpy(inline_, inline__, sizeof(chstring));
  if (inline_[0] == period && inline_[1] == newline)
    stat = enddata;
  else
    stat = puttxt(inline_, LINK);
  if (stat == ok)
    return stat;
  LINK->cstate = concmd;
  setcursor(0, maxcol() - 35);
  erasetoright();

  erasetabs();
  FORLIM = maxcol();
  for (tc = 0; tc <= FORLIM; tc++) {
    if (LINK->tabcols[tc] == tabset)
      settab(tc);
  }

  if (stat != err)
    stat = ok;
  return stat;
}  /*newtext*/


/* ****************************************************** cldcmd ***** */
/* *                                                                 * */
/* *                     CURLEW :: "d" (delete)                      * */
/* *                                                                 * */
/* ******************************************************************* */


/* dodelete -- delete lines n1..n2 */
Local stcode dodelete(n1, n2, glob, LINK)
long n1, n2;
boolean glob;
struct LOC_editor *LINK;
{
  stcode stat;
  long oldlastln, lct;
  char drow;

  if (!glob && !LINK->rebuild)
    checkpoint(LINK);
  oldlastln = LINK->lastln;
  if (lndelete(n1, n2, &stat, LINK) == ok) {
    if (nextln(LINK->curln, LINK) != 0)
      LINK->curln = nextln(LINK->curln, LINK);
  }
  lct = oldlastln - LINK->lastln;

  if (!glob) {
    showlit("  Text lines deleted :  ", LINK);
    showdec(lct, 1L, -1L, LINK);
  }

  if (LINK->appstate != fsereq)
    return stat;
  if (LINK->curln < LINK->topln) {
    LINK->topln = max(LINK->curln, 1L);
    LINK->fsrow = LINK->toprow;
    return stat;
  }
  if (n1 < LINK->topln || n1 >= LINK->topln + LINK->botrow - LINK->toprow)
    return stat;
  drow = LINK->toprow + n1 - LINK->topln;
  if (lct < LINK->botrow - drow)
    scrollup(drow, LINK->botrow, (int)lct);
  return stat;
}  /*dodelete*/


/* ****************************************************** clplcmd **** */
/* *                                                                 * */
/* *              CURLEW :: "p" (print) and "l" (list)               * */
/* *                                                                 * */
/* ******************************************************************* */


/* doprint -- print lines n1..n2 */
Local stcode doprint(n1, n2, lflag, LINK)
long n1, n2;
boolean lflag;
struct LOC_editor *LINK;
{
  long i;
  chstring line;

  if (n1 <= 0)
    return err;
  else {
    for (i = n1; i <= n2; i++) {
      if (lflag) {
	showdec(i, 6L, 2L, LINK);
	LINK->cindent = 8;
      }
      gettxt(i, line, LINK);
      showstr(line, LINK);
    }
    LINK->curln = n2;
    return ok;
  }
}  /*doprint*/


/* ****************************************************** clrcmd ***** */
/* *                                                                 * */
/* *                      CURLEW :: "r" (read)                       * */
/* *                                                                 * */
/* ******************************************************************* */


/* doread -- read "fil" after line n */
Local stcode doread(n, fil, glob, rjfd, LINK)
long n;
uchar *fil;
boolean glob;
short rjfd;
struct LOC_editor *LINK;
{
  long count;
  boolean t;
  stcode stat;
  short fd, jfd;
  chstring inline_;
  long rc, junk;
  boolean fastread;

  if (fil[0] == endstr) {
    showlit("  *** No saved filename&", LINK);
    showc(newline, LINK);
    LINK->msgdone = true;
    stat = err;
    return stat;
  }
  fd = openf(fil, (long)ioread);
  if (fd == ioerror_) {
    showlit("  *** File does not exis", LINK);
    showlit("t or no read access :  &", LINK);
    showstr(fil, LINK);
    showc(newline, LINK);
    LINK->msgdone = true;
    stat = err;
    return stat;
  }
  inform("Reading file&           ", LINK);
  if (!glob)
    checkpoint(LINK);
  LINK->curln = n;
  stat = ok;
  jfd = LINK->jfout;
  LINK->jfout = rjfd;
  fastread = (LINK->jfout == ioerror_);
  if (fastread) {  /* check that "ffcopy" available */
    getinf((long)ffcrtn, inline_, (long)maxstr);
    fastread = (inline_[0] == lety);
  }
  if (fastread)   /* fast file copy route */
    stat = putfiletxt(fd, &count, LINK);
  else {  /* record by record copy */
    count = 0;
    do {
      t = getline(inline_, fd, (long)maxstr);
      if (t) {
	stat = puttxt(inline_, LINK);
	if (stat != err)
	  count++;
      }
    } while (stat == ok && t);
  }
  LINK->jfout = jfd;
  getfds(&rc, fd);
  closef(fd);
  junk = reclaim(LINK);
  if (rc <= 0 && stat == ok)
    showlit("  Text lines read    :  ", LINK);
  else {
    soundalarm();
    showlit("  *** Problems reading f", LINK);
    showlit("ile; stopped at record :", LINK);
    showc(blank, LINK);
    LINK->msgdone = true;
    LINK->autoview = false;   /* if entry, no auto "v" */
    nbrcmdargs = 0;   /* ...and no second parameter */
    stat = err;
  }
  showdec(count, 1L, -1L, LINK);
  return stat;
}  /*doread*/


/* ****************************************************** clwexcmd *** */
/* *                                                                 * */
/* *              CURLEW :: "w" (write) and "ex" (exit)              * */
/* *                                                                 * */
/* ******************************************************************* */


/* filebuild -- copy lines from buffer to target file */
Local boolean filebuild(n1, n2, fd, entab, LINK)
long n1, n2;
short fd;
boolean entab;
struct LOC_editor *LINK;
{
  boolean Result, svdetab, direct;
  long i, j, rc;
  chstring line;
  stcode stat;

  svdetab = LINK->detab;
  if (entab)   /* must expand any tabs present */
    LINK->detab = true;
  if (LINK->detab)
    direct = false;   /* can't use ixcopy if tabs around */
  else {
    getinf((long)ixcrtn, line, (long)maxstr);
    direct = (line[0] == lety);
  }

  if (direct) {
    j = n1;
    stat = ok;
    while (j <= n2 && stat == ok) {
      stat = copytxt(j, fd, LINK);
      j++;
    }
    Result = (stat == ok);
  } else {
    for (i = n1; i <= n2; i++) {
      gettxt(i, line, LINK);
      if (entab)   /* spaces -> tab characters */
	compress(line, LINK);
      putstr(line, fd);
    }
    getfds(&rc, fd);
    Result = (rc == 0);
  }
  LINK->detab = svdetab;
  closef(fd);
  return Result;
}  /*filebuild*/


/* safewrite -- save write information before emptying target file */
Local boolean safewrite(n1, n2, sf, LINK)
long n1, n2;
uchar *sf;
struct LOC_editor *LINK;
{
  chstring sfile1, sfile2;
  short fd;
  boolean safew;

  safew = false;
  sf[0] = endstr;
  getinf((long)clsaveone, sfile1, (long)maxstr);
  if (sfile1[0] == endstr)   /* safety write needed? */
    return safew;
  fd = create(sfile1, (long)iowrite);
  if (fd == ioerror_)
    return safew;
  scopy(sfile1, 1L, sf, 1L);   /* save the name */
  safew = filebuild(n1, n2, fd, false, LINK);   /* write save file */
  if (!safew)
    return safew;
  /* may need to rename this as a different filename */
  getinf((long)clsavetwo, sfile2, (long)maxstr);
  if (sfile2[0] == endstr)
    return safew;
  safew = renamf(sfile1, sfile2);   /* rename it */
  if (safew)   /* correct the name */
    scopy(sfile2, 1L, sf, 1L);
  return safew;
}  /*safewrite*/


/* dowrite -- write lines n1..n2 into file */
Local stcode dowrite(n1, n2, fil, LINK)
long n1, n2;
uchar *fil;
struct LOC_editor *LINK;
{
  stcode Result;
  short fd;
  chstring savefn, line;
  boolean wholefile, filegone, swdone, tryrename, done;
  long junk;

  if (LINK->cfilelen || LINK->crecdlen) {
    showlit("  *** \"write\" disabled t", LINK);
    showlit("o avoid loss of data fro", LINK);
    showlit("m oversize file@n&      ", LINK);
    LINK->msgdone = true;
    return err;
  }
  if (!edbufok(LINK)) {
    showlit("  *** \"write\" disabled b", LINK);
    showlit("ecause edit buffer integ", LINK);
    showlit("rity checks failed@n&   ", LINK);
    LINK->msgdone = true;
    return err;
  }
  if (fil[0] == endstr) {
    showlit("  *** No saved filename&", LINK);
    showc(newline, LINK);
    LINK->msgdone = true;
    return err;
  }
  wholefile = (n1 == 1 && n2 == LINK->lastln);
  inform("Writing file&           ", LINK);
  swdone = safewrite(n1, n2, savefn, LINK);
  fd = create(fil, (long)iowrite);
  if (fd == ioerror_) {
    junk = reclaim(LINK);
    showlit("  *** Cannot create file", LINK);
    showlit(" or no write access :  &", LINK);
    showstr(fil, LINK);
    showc(newline, LINK);
    LINK->msgdone = true;
    return err;
  }
  tryrename = false;
  if (swdone) {
    getinf((long)clrensave, line, (long)maxstr);
    tryrename = (line[0] == lety);
    if (!tryrename && wholefile) {  /* fix journal file */
      jfcontrol((long)iowrite, LINK);
      journal(0L, 0L, lete, savefn, LINK);
      journal(0L, 0L, letf, LINK->savefile, LINK);
    }
  }

  /* may be able to just rename the save file as target .. */
  done = false;
  if (tryrename) {
    closef(fd);
    fd = ioerror_;   /* can't rename if open */
    done = renamf(savefn, fil);
  }

  /* .. if that didn't work, have to try to build target */
  if (done)
    savefn[0] = endstr;   /* no remove needed */
  else {  /* do it the hard way */
    if (fd == ioerror_)
      fd = create(fil, (long)iowrite);
    done = filebuild(n1, n2, fd, LINK->entab, LINK);
  }

  if (done && wholefile) {  /* final journal fix */
    jfcontrol((long)iowrite, LINK);
    journal(0L, 0L, lete, fil, LINK);
    journal(0L, 0L, letf, LINK->savefile, LINK);
  }
  if (savefn[0] != endstr)
    filegone = remove(savefn);
  junk = reclaim(LINK);
  if (done) {
    showlit("  Text lines written :  ", LINK);
    showdec(n2 - n1 + 1, 1L, -1L, LINK);
    if (wholefile)
      LINK->changed = false;
    Result = ok;
  } else {
    soundalarm();
    showlit("  *** Problems writing f", LINK);
    showlit("ile; check your disk spa", LINK);
    showlit("ce allocation@n&        ", LINK);
    LINK->msgdone = true;
    Result = err;
  }
  checkpoint(LINK);
  return Result;
}  /*dowrite*/


/* ****************************************************** clmcocmd *** */
/* *                                                                 * */
/* *              CURLEW :: "m" (move) and "co" (copy)               * */
/* *                                                                 * */
/* ******************************************************************* */


/* move -- move line1..line2 after line3 */
Local stcode move_(line3, glob, LINK)
long line3;
boolean glob;
struct LOC_editor *LINK;
{
  if (LINK->line1 <= 0 || line3 >= LINK->line1 && line3 < LINK->line2)
    return err;
  else {
    if (!glob)
      checkpoint(LINK);
    blkmove(LINK->line1, LINK->line2, line3, LINK);
    if (line3 > LINK->line1)
      LINK->curln = line3;
    else
      LINK->curln = line3 + LINK->line2 - LINK->line1 + 1;
    if (!glob) {
      showlit("  Text lines moved :  & ", LINK);
      showdec(LINK->line2 - LINK->line1 + 1, 1L, -1L, LINK);
    }
    return ok;
  }
}  /*move*/


/* copy -- copy line1..line2 after line3 */
Local stcode copy_(line3, glob, LINK)
long line3;
boolean glob;
struct LOC_editor *LINK;
{
  long i, lsave;
  chstring line;
  stcode stat;
  long FORLIM;

  if (LINK->line1 <= 0 || LINK->lastln <= 0) {
    stat = err;
    return stat;
  }
  if (!glob)
    checkpoint(LINK);
  lsave = LINK->lastln;
  LINK->curln = LINK->lastln;
  FORLIM = LINK->line2;
  for (i = LINK->line1; i <= FORLIM; i++) {
    gettxt(i, line, LINK);
    stat = puttxt(line, LINK);
  }
  if (stat != ok)
    return stat;
  blkmove(lsave + 1, LINK->lastln, line3, LINK);
  LINK->curln = line3 + LINK->line2 - LINK->line1 + 1;
  if (!glob) {
    showlit("  Text lines copied :  &", LINK);
    showdec(LINK->line2 - LINK->line1 + 1, 1L, -1L, LINK);
  }
  return stat;
}  /*copy*/


/* ****************************************************** clscmd ***** */
/* *                                                                 * */
/* *                    CURLEW :: "s" (substitute)                   * */
/* *                                                                 * */
/* ******************************************************************* */


/* getrhs -- get right hand side of "s" command */
Local stcode getrhs(lin, i, sub, gflag, LINK)
uchar *lin;
long *i;
uchar *sub;
boolean *gflag;
struct LOC_editor *LINK;
{
  stcode Result;
  uchar ch;

  Result = ok;
  if (lin[*i - 1] == endstr) {
    LINK->status = err;
    return Result;
  }
  if (lin[*i] == endstr) {
    LINK->status = err;
    return Result;
  }
  *i = makesub(lin, *i + 1, lin[*i - 1], sub);
  if (*i == 0)
    return err;
  (*i)++;
  skipbl(lin, i);
  ch = lowercase(lin[*i - 1]);
  if (ch == letg)
    *gflag = true;
  else {
    (*i)--;
    *gflag = false;
  }
  return Result;
}  /*getrhs*/


/* ckp -- check for "p", "l" or "n" after command */
Local stcode ckp(lin, i, pflag, lflag, status, LINK)
uchar *lin;
long i;
boolean *pflag, *lflag;
stcode *status;
struct LOC_editor *LINK;
{
  uchar ch;

  skipbl(lin, &i);
  ch = lowercase(lin[i - 1]);
  *pflag = (LINK->appstate != fsereq);   /* default off for full-screen */
  *lflag = true;
  i++;
  if (ch == letp) {   /* print text only */
    *pflag = true;
    *lflag = false;
  } else if (ch == letn)   /* none of these, backtrack */
    *pflag = false;
  else if (ch == letl)
    *pflag = true;
  else
    i--;

  if (lin[i - 1] == newline)
    *status = ok;
  else
    *status = err;
  return (*status);

  /* don't print anything */
  /* force printing, with lnum */
}  /*ckp*/


/* catsub -- add replacement text to end of new */
Local Void catsub(lin, line, s1, s2, sub, new_, k, maxnew, LINK)
uchar *lin;
long *line, s1, s2;
uchar *sub, *new_;
long *k, maxnew;
struct LOC_editor *LINK;
{
  long i, j;
  boolean junk;

  i = 1;
  while (sub[i - 1] != endstr) {
    if (sub[i - 1] == mkditto) {
      for (j = s1 - 1; j <= s2 - 2; j++)
	junk = addstr(lin[j], new_, k, maxnew);
    } else {
      junk = addstr(sub[i - 1], new_, k, maxnew);
      if (sub[i - 1] == newline) {
	junk = addstr(endstr, new_, k, maxnew);
	LINK->curln = *line - 1;
	if (puttxt(new_, LINK) == ok) {
	  *k = 1;
	  (*line)++;
	  LINK->line2++;
	} else
	  (*k)--;
      }
    }
    i++;
  }
}  /*catsub*/


/* subst -- subsitute sub for occurences of pattern */
Local stcode subst(sub, gflag, glob, LINK)
uchar *sub;
boolean gflag, glob;
struct LOC_editor *LINK;
{
  chstring new_, old;
  long j, k, lastm, line, m, nsub;
  stcode stat;
  boolean done, subbed, junk;

  if (glob)
    stat = ok;
  else {
    stat = err;
    checkpoint(LINK);
    nsub = 0;
  }
  done = (LINK->line1 <= 0);
  line = LINK->line1;
  while (!done && line <= LINK->line2) {
    j = 1;
    subbed = false;
    gettxt(line, old, LINK);
    lastm = 0;
    k = 1;
    while (old[k - 1] != endstr) {
      if (gflag || !subbed)
	m = amatch(old, k, LINK->pat, 1L);
      else
	m = 0;
      if (m > 0 && lastm != m) {  /* replace matched text */
	subbed = true;
	catsub(old, &line, k, m, sub, new_, &j, (long)maxstr, LINK);
	lastm = m;
	if (!glob)
	  nsub++;
      }
      if (m == 0 || m == k) {  /* no match or null match */
	junk = addstr(old[k - 1], new_, &j, (long)maxstr);
	k++;
      } else  /* skip matched text */
	k = m;
    }
    if (subbed) {
      if (!addstr(endstr, new_, &j, (long)maxstr)) {
	stat = err;
	done = true;
      } else {
	stat = reptxt(new_, line, LINK);
	LINK->line2 += LINK->curln - line;
	line = LINK->curln;
	if (stat == err)
	  done = true;
	else
	  stat = ok;
      }
    }
    line++;
  }
  if (glob)
    return stat;
  if (stat != ok) {
    showlit("  *** No substitution@n&", LINK);
    LINK->msgdone = true;
    return stat;
  }
  if (LINK->appstate == fsereq) {
    showlit("  Substitutions made :  ", LINK);
    showdec(nsub, 1L, -1L, LINK);
  }
  return stat;
}  /*subst*/


/* ****************************************************** clfocmd **** */
/* *                                                                 * */
/* *                    CURLEW :: "fo" (format)                      * */
/* *                                                                 * */
/* ******************************************************************* */


/* fmtdef -- set defaulted line numbers for format command */
Local stcode fmtdef(status, LINK)
stcode *status;
struct LOC_editor *LINK;
{
  long line, j;
  chstring lin;

  if (LINK->nlines == 0) {
    line = LINK->curln;
    gettxt(line, lin, LINK);
    j = 1;
    skipbl(lin, &j);
    if (lin[j - 1] != newline && line > 1) {
      do {
	line--;
	gettxt(line, lin, LINK);
	j = 1;
	skipbl(lin, &j);
      } while (lin[j - 1] != newline && line > 0);
      line++;
    }
    LINK->line1 = line;
  }
  if (LINK->nlines <= 1) {
    line = LINK->line1;
    do {
      gettxt(line, lin, LINK);
      j = 1;
      skipbl(lin, &j);
      if (lin[j - 1] != newline)
	line++;
    } while (lin[j - 1] != newline && line <= LINK->lastln);
    LINK->line2 = line - 1;
  }
  if (LINK->line1 > LINK->line2 || LINK->line1 <= 0)
    *status = err;
  else
    *status = ok;
  return (*status);
}  /*fmtdef*/

/* Local variables for dofmt: */
struct LOC_dofmt {
  struct LOC_editor *LINK;
  long line1, line2;
  /* global parameters */
  fmttype reqfmt;   /* required formatting action; init=lj */
  long osval;   /* offset of text output; >=0; init=0 */
  long inval;   /* current indent; >= 0; init=0 */
  long rmval;   /* right margin; init=70 */
  long tival;   /* current temporary indent; init=0 */
  long nptival;   /* tival for new paragraph; init=0 */
  boolean hind;   /* true -> hanging; init=false -> item */

  /* output area */
  stcode stat;   /* status return */
  long outp;   /* last char pos in outbuf; init=0 */
  long outw;   /* width of text in outbuf; init=0 */
  long outwds;   /* number of words in outbuf; init=0 */
  chstring outbuf;   /* lines to be filled collect here */
  char dir;   /* direction for blank padding */
  long eosbl;   /* loaded from eosextra at sentence end */
  long eosextra;   /* extra blanks to add for sentence end */
  long lct;   /* output line count */
  boolean firstwd;   /* true -> first word of item */
} ;


/* width -- compute width of character string */
Local long width(buf, LINK)
uchar *buf;
struct LOC_dofmt *LINK;
{
  long i, w;

  w = 0;
  i = 1;
  while (buf[i - 1] != endstr) {
    if (buf[i - 1] == backspace)
      w--;
    else if (buf[i - 1] != newline)
      w++;
    i++;
  }
  return w;
}  /*width*/


/* put -- put out line with proper spacing and indenting */
Local Void put(buf, LINK)
uchar *buf;
struct LOC_dofmt *LINK;
{
  long i, toffset;
  chstring ltext;

  toffset = LINK->osval + LINK->inval + LINK->tival;
  LINK->tival = 0;
  for (i = 0; i < toffset; i++)   /* page offset + indenting */
    ltext[i] = blank;
  i = 1;
  while (buf[i - 1] != newline) {  /* convert hard blanks to blank */
    if (buf[i - 1] == del)
      buf[i - 1] = blank;
    i++;
  }
  scopy(buf, 1L, ltext, toffset + 1);
  LINK->stat = puttxt(ltext, LINK->LINK);
  LINK->lct++;
}  /*put*/


/* break -- end current filled line */
Local Void break_(LINK)
struct LOC_dofmt *LINK;
{
  if (LINK->outp > 0) {
    LINK->outbuf[LINK->outp - 1] = newline;
    LINK->outbuf[LINK->outp] = endstr;
    put(LINK->outbuf, LINK);
  }
  LINK->outp = 0;
  LINK->outw = 0;
  LINK->outwds = 0;
  LINK->eosbl = 0;
}  /*break*/


/* leadbl -- delete leading blanks */
Local Void leadbl(buf, LINK)
uchar *buf;
struct LOC_dofmt *LINK;
{
  long i, j, FORLIM;

  i = 1;
  while (buf[i - 1] == blank)   /* find first non-blank */
    i++;
  FORLIM = length_(buf) + 1;
  for (j = i; j <= FORLIM; j++)   /* move line to left */
    buf[j - i] = buf[j - 1];
  if (buf[0] == newline)   /* blank line -> new paragraph */
    break_(LINK);
}  /*leadbl*/


/* spread -- spread words to justify right margin */
Local Void spread(buf, outp, nextra, outwds, LINK)
uchar *buf;
long outp, nextra, outwds;
struct LOC_dofmt *LINK;
{
  long i, j, nb, nholes;

  if (nextra <= 0 || outwds <= 1)
    return;
  LINK->dir = 1 - LINK->dir;   /* reverse previous direction */
  nholes = outwds - 1;
  i = outp - 1;
  j = min(maxstr - 2L, i + nextra);   /* room for newline and endstr */
  while (i < j) {
    buf[j - 1] = buf[i - 1];
    if (buf[i - 1] == blank) {
      if (LINK->dir == 0)
	nb = (nextra - 1) / nholes + 1;
      else
	nb = nextra / nholes;
      nextra -= nb;
      nholes--;
      while (nb > 0) {
	j--;
	buf[j - 1] = blank;
	nb--;
      }
    }
    i--;
    j--;
  }
}  /*spread*/


/* putword -- put word in outbuf; does margin justification */
Local Void putword(wordbuf, LINK)
uchar *wordbuf;
struct LOC_dofmt *LINK;
{
  long last, llval, nextra, w, j, k;
  uchar wlc;
  long FORLIM;

  w = width(wordbuf, LINK);
  last = length_(wordbuf) + LINK->outp + LINK->eosbl + 1;
      /* new end of outbuf */
  llval = LINK->rmval - LINK->tival - LINK->inval;
  if (LINK->outp > 0 && (LINK->outw + w + LINK->eosbl > llval || last >= maxstr)) {
    last += -LINK->outp - LINK->eosbl;   /* remember end of wordbuf */
    if (LINK->reqfmt == jb) {   /* flush previous line */
      nextra = llval - LINK->outw + 1;
      if (nextra > 0 && LINK->outwds > 1) {
	spread(LINK->outbuf, LINK->outp, nextra, LINK->outwds, LINK);
	LINK->outp += nextra;
      }
    }
    break_(LINK);
  }
  FORLIM = LINK->eosbl;
  for (j = 1; j <= FORLIM; j++) {
    LINK->outp++;
    LINK->outbuf[LINK->outp - 1] = del;   /* it's a blank really */
    LINK->outw++;   /* 1 for each blank */
  }
  LINK->eosbl = 0;
  scopy(wordbuf, 1L, LINK->outbuf, LINK->outp + 1);
  LINK->outp = last;
  LINK->outbuf[LINK->outp - 1] = blank;   /* blank between words */
  LINK->outw += w + 1;   /* 1 for blank */
  if (LINK->firstwd) {  /* start of item */
    LINK->firstwd = false;
    k = -LINK->nptival;
    if (w >= k) {   /* too long to fit in */
      break_(LINK);
      return;
    }
    LINK->outp--;
    for (j = w + 1; j <= k; j++) {
      LINK->outp++;
      LINK->outbuf[LINK->outp - 1] = del;
    }
    LINK->outw += k - w - 1;
    return;
  }
  wlc = wordbuf[length_(wordbuf) - 1];
  if (wlc == period || wlc == question || wlc == exclam)
	/* extra for sentence end */
	  LINK->eosbl = LINK->eosextra;
  LINK->outwds++;

  /* indent for item text */
  /* just an ordinary word */
}  /*putword*/


/* centre -- centre a line by setting tival */
Local Void centre(buf, LINK)
uchar *buf;
struct LOC_dofmt *LINK;
{
  LINK->tival = max((LINK->rmval - LINK->inval - width(buf, LINK)) / 2, 0L);
}  /*centre*/


/* right -- right justify a line by setting tival */
Local Void right(buf, LINK)
uchar *buf;
struct LOC_dofmt *LINK;
{
  LINK->tival = max(LINK->rmval - LINK->inval - width(buf, LINK), 0L);
}  /*right*/


/* initfmt -- set format parameter to default values */
Local Void initfmt(LINK)
struct LOC_dofmt *LINK;
{
  if (LINK->LINK->gblfmt)
    LINK->reqfmt = jb;
  else
    LINK->reqfmt = lj;
  LINK->dir = 0;
  LINK->eosbl = 0;
  LINK->eosextra = LINK->LINK->gbleos;
  LINK->osval = LINK->LINK->gbloset;
  LINK->inval = 0;
  LINK->rmval = LINK->LINK->gblwidth;
  LINK->tival = 0;
  LINK->nptival = LINK->LINK->gblpara;
  LINK->hind = false;
  LINK->firstwd = false;
  LINK->outp = 0;
  LINK->outw = 0;
  LINK->outwds = 0;
  LINK->lct = 0;
}  /*initfmt*/


/* fmtpars -- get override values from the command parameter string */
Local Void fmtpars(cmd, i, LINK)
uchar *cmd;
long i;
struct LOC_dofmt *LINK;
{
  uchar ch;

  skipbl(cmd, &i);
  while (cmd[i - 1] != newline) {
    ch = lowercase(cmd[i - 1]);
    i++;
    if (ch == letl)
      LINK->reqfmt = lj;   /* left justify (ragged right) */
    else if (ch == letb)
      LINK->reqfmt = jb;   /* justify both margins */
    else if (ch == letc)
      LINK->reqfmt = cen;   /* centre text */
    else if (ch == letr)
      LINK->reqfmt = rj;   /* quad right (ragged left) */
    else if (ch == letw) {
      LINK->rmval = ctoi(cmd, &i);
      LINK->rmval = max(LINK->rmval, 24L);
      LINK->rmval = min(LINK->rmval, maxstr / 2L);
    } else if (ch == leti) {
      LINK->inval = ctoi(cmd, &i);
      LINK->inval = max(LINK->inval, 0L);
      LINK->inval = min(LINK->inval, 60L);
    } else if (ch == letp) {
      LINK->nptival = ctoi(cmd, &i);
      LINK->nptival = min(LINK->nptival, 24L);
      LINK->nptival = max(LINK->nptival, -24L);
    } else if (ch == leth)
      LINK->hind = true;   /* hanging indentation */
    else if (ch == lets) {
      LINK->eosextra = ctoi(cmd, &i) - 1;
      LINK->eosextra = min(LINK->eosextra, 3L);
      LINK->eosextra = max(LINK->eosextra, 0L);
    }
    /* else ignore */
    skipbl(cmd, &i);
  }
  if (LINK->reqfmt == cen || LINK->reqfmt == rj)
    LINK->nptival = 0;
  if (LINK->nptival >= 0) {  /* normal paragraph */
    LINK->hind = false;
    if (LINK->rmval - LINK->inval - LINK->nptival < 16)
      LINK->rmval = LINK->inval + LINK->nptival + 16;
  } else {  /* item or hanging indent */
    LINK->inval -= LINK->nptival;
    if (!LINK->hind)
      LINK->firstwd = true;
    if (LINK->rmval - LINK->inval < 16)
      LINK->rmval = LINK->inval + 16;
  }
  LINK->tival = LINK->nptival;

  /* width (right margin column */
  /* indent (left margin offset) */
  /* paragraph first line indent */
  /* sentence end spaces */
}  /*fmtpars*/


/* text -- process text lines */
Local Void text(inbuf, LINK)
uchar *inbuf;
struct LOC_dofmt *LINK;
{
  chstring wordbuf;
  long i;

  if (inbuf[0] == blank || inbuf[0] == newline)   /* move left */
    leadbl(inbuf, LINK);
  if (inbuf[0] == newline) {
    put(inbuf, LINK);   /* all blank line ... */
    LINK->tival = LINK->nptival;   /* ... signals new paragraph */
    if (!LINK->hind && LINK->nptival < 0)
      LINK->firstwd = true;   /* start of new item */
    return;
  }
  switch (LINK->reqfmt) {

  case lj:
  case jb:  /* filled text */
    i = 1;
    do {
      i = getword(inbuf, i, wordbuf);
      if (i > 0)
	putword(wordbuf, LINK);
    } while (i != 0);
    break;

  case cen:
  case rj:  /* centring or right justify */
    if (LINK->reqfmt == cen)
      centre(inbuf, LINK);
    else
      right(inbuf, LINK);
    put(inbuf, LINK);
    break;
  }/*case*/
}  /*text*/


/* fiximage -- in full-screen mode, optimise image correction */
Local Void fiximage(LINK)
struct LOC_dofmt *LINK;
{
  long pinline;   /* display backstop */
  long oldtopln;   /* old top of screen line number */
  long oldbotln;   /* old bottom of screen line number */
  long botln;   /* final bottom of screen line number */
  long relbotln;   /* botln relative to previous buffer state */
  long endln;   /* end of formatted text line number */

  pinline = LINK->LINK->topln + LINK->LINK->pinrow - LINK->LINK->toprow - 2;
  oldtopln = LINK->LINK->topln;
  oldbotln = LINK->LINK->topln + LINK->LINK->botrow - LINK->LINK->toprow;
  if (LINK->LINK->curln < LINK->LINK->topln)
    LINK->LINK->topln = LINK->LINK->curln;
  else if (LINK->LINK->curln > pinline)
    LINK->LINK->topln = LINK->LINK->curln - LINK->LINK->pinrow +
			LINK->LINK->toprow + 2;

  if (LINK->LINK->topln > oldtopln && LINK->line1 > oldtopln &&
      LINK->line1 > LINK->LINK->topln)
    scrollup(LINK->LINK->toprow,
	     (int)(LINK->LINK->toprow + LINK->line1 - oldtopln - 1),
	     (int)(LINK->LINK->topln - oldtopln));

  botln = LINK->LINK->topln + LINK->LINK->botrow - LINK->LINK->toprow;
  relbotln = botln + LINK->line2 - LINK->line1 - LINK->lct + 1;
  endln = LINK->line1 + LINK->lct - 1;
  if (oldbotln != relbotln && LINK->line2 < oldbotln && endln < botln) {
    if (oldbotln < relbotln)
      scrollup((int)(endln - LINK->LINK->topln + LINK->LINK->toprow + 1),
	       LINK->LINK->botrow, (int)(relbotln - oldbotln));
    else
      scrolldown((int)(LINK->line2 - oldtopln + LINK->LINK->toprow + 1),
		 LINK->LINK->botrow, (int)(oldbotln - relbotln));
  }
  LINK->LINK->fsrow = LINK->LINK->curln - LINK->LINK->topln + LINK->LINK->toprow;
}  /*fiximage*/


/* dofmt -- simple word processing command */
Local stcode dofmt(line1_, line2_, cmd, i, LINK)
long line1_, line2_;
uchar *cmd;
long i;
struct LOC_editor *LINK;
{  /* dofmt main routine */
  struct LOC_dofmt V;
  long svcurln;   /* current line save */

  /* text input */
  long line;   /* line being processed */
  chstring inbuf;   /* input line */
  long FORLIM;


  V.LINK = LINK;
  V.line1 = line1_;
  V.line2 = line2_;
  if (LINK->fstate != blkmode)   /* not ^B..^P */
    checkpoint(LINK);
  initfmt(&V);   /* set default values */
  fmtpars(cmd, i, &V);   /* apply parameters */

  LINK->curln = V.line2;
  FORLIM = V.line2;
  for (line = V.line1; line <= FORLIM; line++)
  {  /* fetch and format source text */
    gettxt(line, inbuf, LINK);
    text(inbuf, &V);
  }
  break_(&V);   /* flush last output, if any */
  svcurln = LINK->curln;
  V.stat = lndelete(V.line1, V.line2, &V.stat, LINK);
  LINK->curln = svcurln - V.line2 + V.line1 - 1;

  if (LINK->appstate == fsereq)
    fiximage(&V);

  if (LINK->fstate == blkmode)   /* not ^B..^P */
    return ok;
  showlit("  Text lines formatted :", LINK);
  showc(blank, LINK);
  showc(blank, LINK);
  showdec(V.line2 - V.line1 + 1, 1L, 0L, LINK);
  showlit(" into &                 ", LINK);
  showdec(V.lct, 1L, -1L, LINK);
  return ok;
}  /*dofmt*/

/* Local variables for doset: */
struct LOC_doset {
  struct LOC_editor *LINK;
  uchar *cmd;
  long i;
} ;


/* gettabs -- get and set tab stop columns from parameter string */
Local Void gettabs(LINK)
struct LOC_doset *LINK;
{
  long ti, tj;
  uchar tc, FORLIM;

  skipbl(LINK->cmd, &LINK->i);
  if (!((LINK->cmd[LINK->i - 1] == star) | isdigit_(LINK->cmd[LINK->i - 1])))
    return;
  FORLIM = maxcol();
  for (tc = 0; tc <= FORLIM; tc++)
    LINK->LINK->tabcols[tc] = notab;
  LINK->LINK->tabrpt = 0;

  /* get new stops from parameter string */
  if (LINK->cmd[LINK->i - 1] == star) {  /* regular tab stops */
    if (LINK->cmd[LINK->i - 1] == star)
      LINK->i++;
    LINK->LINK->tabrpt = ctoi(LINK->cmd, &LINK->i);
    if (LINK->LINK->tabrpt <= 0)   /* default is *8 */
      LINK->LINK->tabrpt = 8;
    LINK->LINK->tabrpt = max(LINK->LINK->tabrpt, 2L);
    LINK->LINK->tabrpt = min(LINK->LINK->tabrpt, (maxcol() + 1L) / 4);
    FORLIM = maxcol();
    for (tc = 0; tc <= FORLIM; tc++) {
      if (tc % LINK->LINK->tabrpt == 0)
	LINK->LINK->tabcols[tc] = tabset;
    }
  } else {
    do {
      ti = ctoi(LINK->cmd, &LINK->i);
      if (ti != 0) {
	tj = ti - 1;
	tj = max(tj, 0L);
	tc = min(tj, (long)maxcol());
	LINK->LINK->tabcols[tc] = tabset;
      }
    } while (ti != 0);
  }

  /* establish new tabulation stops */
  erasetabs();
  FORLIM = maxcol();
  for (tc = 0; tc <= FORLIM; tc++) {
    if (LINK->LINK->tabcols[tc] == tabset)
      settab(tc);
  }
}  /*gettabs*/


/* putkeydef -- replace definition for key n with string s */
Local Void putkeydef(n, s, LINK)
long n;
uchar *s;
struct LOC_doset *LINK;
{
  long i, j, k, koffset;

  j = n;
  k = LINK->LINK->kptrs[n - minkey];

  /* skip existing definition by moving later definitions forward */
  i = k;
  while (LINK->LINK->kdefs[i - 1] != newline)   /* skip old definition */
    i++;
  koffset = i - k + 1;
  for (j = minkey; j <= maxkey; j++) {
    if (LINK->LINK->kptrs[j - minkey] > k)   /* need to fix pointer */
      LINK->LINK->kptrs[j - minkey] -= koffset;
  }
  j = i + 1;
  while (j <= LINK->LINK->kdmax) {  /* move later defs forward */
    LINK->LINK->kdefs[k - 1] = LINK->LINK->kdefs[j - 1];
    j++;
    k++;
  }

  /* add revised definition at the end */
  i = 1;
  LINK->LINK->kptrs[n - minkey] = k;
  while (k < vkdlim && s[i - 1] != endstr) {
    LINK->LINK->kdefs[k - 1] = s[i - 1];
    k++;
    i++;
  }
  LINK->LINK->kdefs[k - 1] = newline;
  LINK->LINK->kdmax = k;

  /* check for "prompt", needs preprocessing in "context" */
  LINK->LINK->kpick[n - minkey] = false;
  i = 1;
  skipbl(s, &i);
  if (isletter(s[i - 1])) {  /* dreadful kludge here ... */
    if ((lowercase(s[i - 1]) == letp) & (lowercase(s[i]) == letr) &
	(lowercase(s[i + 1]) == leto))
	  /* "p" */
	    LINK->LINK->kpick[n - minkey] = true;   /* "prompt"! */
    /* "r" */
    /* "o" */
  }
}  /*putkeydef*/


/* getfkstr -- get and replace function key string */
Local Void getfkstr(LINK)
struct LOC_doset *LINK;
{
  chstring fks;
  uchar delim, tc;
  long knum, j;

  knum = ctoi(LINK->cmd, &LINK->i);
  skipbl(LINK->cmd, &LINK->i);
  delim = LINK->cmd[LINK->i - 1];
  if (isalphanum(delim) || delim == newline || delim == endstr)
    fks[0] = endstr;
  else {
    LINK->i++;
    j = 1;
    tc = LINK->cmd[LINK->i - 1];
    while (tc != delim && tc != newline && tc != endstr) {
      fks[j - 1] = tc;
      j++;
      LINK->i++;
      tc = LINK->cmd[LINK->i - 1];
    }
    fks[j - 1] = endstr;
  }

  if (knum >= 1 && knum <= 37)
    putkeydef(knum, fks, LINK);
}  /*getfkstr*/


/* setmask -- set control character input mask */
Local Void setmask(n, LINK)
long n;
struct LOC_doset *LINK;
{
  uchar c;

  for (c = nul; c <= del; c++)
    LINK->LINK->mask[c - nul] = c;
  LINK->LINK->mask[newline - nul] = nul;
  if (n >= 4)  /* >=4 : all except lf, nul */
    return;
  for (c = nul; c <= us; c++)
    LINK->LINK->mask[c - nul] = nul;
  LINK->LINK->mask[del - nul] = nul;   /* 0 : mask all control chars */
  if (n < 1)
    return;
  LINK->LINK->mask[ctli - nul] = ctli;   /* 1 : tab */
  if (n < 2)  /* 2 : tab, del */
    return;
  LINK->LINK->mask[del - nul] = del;
  if (n < 3)  /* 3 : tab, del, esc, bs, ff */
    return;
  LINK->LINK->mask[esc - nul] = esc;
  LINK->LINK->mask[ctlh - nul] = ctlh;
  LINK->LINK->mask[ctll - nul] = ctll;
}  /*setmask*/


/* getswitch -- get value of on (+), off (-) parameter */
Local Void getswitch(sw, LINK)
boolean *sw;
struct LOC_doset *LINK;
{
  skipbl(LINK->cmd, &LINK->i);
  if (LINK->cmd[LINK->i - 1] == plus || LINK->cmd[LINK->i - 1] == minus) {
    *sw = (LINK->cmd[LINK->i - 1] == plus);
    LINK->i++;
  }
}  /*getswitch*/


/* getivalue -- get value of integer, check for + or - postfix */
Local long getivalue(n, LINK)
long *n;
struct LOC_doset *LINK;
{
  long tnum;

  skipbl(LINK->cmd, &LINK->i);
  if (!(isdigit_(LINK->cmd[LINK->i - 1]) || LINK->cmd[LINK->i - 1] == plus ||
	LINK->cmd[LINK->i - 1] == minus))
  {   /* no number, no change */
    tnum = *n;
    return tnum;
  }
  tnum = ctoi(LINK->cmd, &LINK->i);
  if (LINK->cmd[LINK->i - 1] != plus && LINK->cmd[LINK->i - 1] != minus)
    return tnum;
  if (LINK->cmd[LINK->i - 1] == plus)   /* increment existing value */
    tnum += *n;
  else  /* minus so decrement value */
    tnum = *n - tnum;
  LINK->i++;
  return tnum;
}  /*getivalue*/


/* ****************************************************** clsecmd **** */
/* *                                                                 * */
/* *                      CURLEW :: "se" (set)                       * */
/* *                                                                 * */
/* ******************************************************************* */


/* doset -- process set command to establish user profile requests */
Local stcode doset(cmd_, i_, LINK)
uchar *cmd_;
long i_;
struct LOC_editor *LINK;
{  /* doset main routine */
  struct LOC_doset V;
  uchar ch;
  long isave, oldrend, oldxoset, newval;
  chstring pars;


  V.LINK = LINK;
  V.cmd = cmd_;
  V.i = i_;
  skipbl(V.cmd, &V.i);
  isave = V.i;
  while (V.cmd[V.i - 1] != bar && V.cmd[V.i - 1] != newline &&
	 V.cmd[V.i - 1] != endstr)
  {   /* comment */
    ch = lowercase(V.cmd[V.i - 1]);
    V.i++;
    if (islower_(ch)) {
      switch (ch) {

      case leta:   /* a<sw> : RETURN append/split */
	getswitch(&LINK->rappend, &V);
	if (LINK->rappend)   /* RETURN splits */
	  LINK->nfsomode[kappline] = ctlm;   /* RETURN appends */
	else
	  LINK->nfsomode[kappline] = nul;
	if (LINK->appstate == fsereq && LINK->fstate == normal)
	  putmode(LINK->nfsomode);
	break;

      case letb:   /* b<sw> : ^B..^P paragraphing */
	getswitch(&LINK->blkpara, &V);
	break;

      case letc:   /* c<sw> : insertion/overstrike */
	getswitch(&LINK->ciview, &V);
	if (LINK->ciview) {
	  LINK->nfsomode[icharmode] = 1;   /* insertion default */
	  LINK->ctxtmode[icharmode] = 1;   /* ... and cmd input */
	} else {
	  LINK->nfsomode[icharmode] = 0;   /* overstrike default */
	  LINK->ctxtmode[icharmode] = 0;   /* ... and cmd input */
	}
	if (LINK->appstate == fsereq && LINK->fstate == normal)
	  putmode(LINK->nfsomode);
	break;

      case letd:   /* d<sw> : detab in gettxt */
	getswitch(&LINK->detab, &V);
	break;

      case lete:   /* e<sw> : entab on write */
	getswitch(&LINK->entab, &V);
	break;

      case letf:   /* f<keydef> : function key def */
	getfkstr(&V);
	break;

      case letg:
	break;

      case leth:   /* h<sw> : horizontal tab keys */
	getswitch(&LINK->htkeys, &V);
	if (LINK->htkeys) {  /* local horizontal tab keys */
	  LINK->nfsomode[knexttab] = ctli;
	  LINK->nfsomode[kprevtab] = ctlf;
	  LINK->blokmode[knexttab] = ctli;
	  LINK->blokmode[kprevtab] = ctlf;
	} else {  /* host tab insert & prev word */
	  LINK->nfsomode[knexttab] = nul;
	  LINK->nfsomode[kprevtab] = nul;
	  LINK->blokmode[knexttab] = nul;
	  LINK->blokmode[kprevtab] = nul;
	}
	if (LINK->appstate == fsereq) {
	  if (LINK->fstate == normal)
	    putmode(LINK->nfsomode);
	  else
	    putmode(LINK->blokmode);
	}
	break;


      case leti:   /* i<sw> : auto image updating */
	getswitch(&LINK->autoimage, &V);
	if (LINK->autoimage)   /* keep till frame end */
	  LINK->nfsomode[notify] = 164;   /* ch/ln del + ln split */
	else
	  LINK->nfsomode[notify] = 0;
	if (LINK->appstate == fsereq && LINK->fstate == normal)
	  putmode(LINK->nfsomode);
	break;

      case letj:   /* j<sw> : format b/l default */
	getswitch(&LINK->gblfmt, &V);
	break;

      case letk:
	break;

      case letl:   /* l<sw> : loop search action */
	getswitch(&LINK->loopsearch, &V);
	break;

      case letm:   /* m<num> : control char mask */
	LINK->mlevel = getivalue(&LINK->mlevel, &V);
	LINK->mlevel = max(LINK->mlevel, 0L);
	LINK->mlevel = min(LINK->mlevel, 4L);
	setmask(LINK->mlevel, &V);
	break;

      case letn:   /* n<sw> : anycase scan switch */
	getswitch(&LINK->acsearch, &V);
	cssinit = !LINK->acsearch;
	break;

      case leto:   /* o<num> : format text offset */
	LINK->gbloset = getivalue(&LINK->gbloset, &V);
	LINK->gbloset = max(LINK->gbloset, 0L);
	LINK->gbloset = min(LINK->gbloset, 60L);
	break;

      case letp:   /* p<num> : format default "p" */
	LINK->gblpara = getivalue(&LINK->gblpara, &V);
	LINK->gblpara = max(LINK->gblpara, -24L);
	LINK->gblpara = min(LINK->gblpara, 24L);
	break;

      case letq:
	break;

      case letr:   /* r<num> : alt rendition */
	oldrend = LINK->rrend;
	LINK->rrend = getivalue(&LINK->rrend, &V);
	if (LINK->rrend > 9)
	  LINK->rrend = 0;
	LINK->rrend = max(LINK->rrend, 0L);
	if (LINK->rrend == concealed)   /* you must see them! */
	  LINK->rrend = defrend;
	if (LINK->rrend != oldrend && oldrend <= crossedout) {
	  if (LINK->appstate == coned)   /* correct name */
	    edname(LINK);
	  else {
	    if (LINK->appstate == fsereq) {  /* correct f/s display */
	      if (LINK->fstate == blkmode)
		viewstate(LINK->fstate, LINK);
	      ruler(LINK->toprow - 1, LINK);
	      ruler(LINK->botrow + 1, LINK);
	    }
	  }
	}
	break;

      case lets:   /* s<num> : format default */
	LINK->gbleos++;
	LINK->gbleos = getivalue(&LINK->gbleos, &V) - 1;
	LINK->gbleos = min(LINK->gbleos, 3L);
	LINK->gbleos = max(LINK->gbleos, 0L);
	break;

      case lett:   /* t<tabdef> : set tab stops */
	gettabs(&V);
	break;

      case letu:   /* u<sw> : K&P or UNIX c/tracks */
	getswitch(&LINK->alttracks, &V);
	setctk(LINK->alttracks);
	LINK->pat[0] = endstr;
	break;

      case letv:   /* v<sw> : automatic view cmd */
	getswitch(&LINK->autoview, &V);
	break;

      case letw:   /* w<num> : format default "w" */
	LINK->gblwidth = getivalue(&LINK->gblwidth, &V);
	LINK->gblwidth = max(LINK->gblwidth, 24L);
	LINK->gblwidth = min(LINK->gblwidth, maxstr / 2L);
	break;

      case letx:   /* x<num> : left margin offset */
	oldxoset = LINK->xoset;
	LINK->xoset = getivalue(&LINK->xoset, &V);
	LINK->xoset = max(LINK->xoset, 0L);
	LINK->xoset = min(LINK->xoset, maxstr - maxcol() - 3L);
	if (LINK->xoset != oldxoset && LINK->appstate == fsereq) {
	  ruler(LINK->toprow - 1, LINK);
	  newval = LINK->fscol + oldxoset - LINK->xoset;
	  newval = max(newval, 0L);
	  newval = min(newval, (long)maxcol());
	  LINK->fscol = newval;
	}
	break;

      case lety:   /* y<num> : vert csr movement */
	LINK->cscroll = getivalue(&LINK->cscroll, &V);
	LINK->cscroll = max(LINK->cscroll, 0L);
	LINK->cscroll = min(LINK->cscroll,
			    (LINK->botrow - LINK->toprow + 1L) / 2);
	break;

      case letz:   /* z<sw> : wrap insertion */
	getswitch(&LINK->inswrap, &V);
	break;

      }/*case*/
    }
    skipbl(V.cmd, &V.i);
  }
  /* line end */
  if (LINK->appstate != fsereq)
    return ok;
  showlit("  Set :  &              ", LINK);
  scopy(V.cmd, isave, pars, 1L);
  showstr(pars, LINK);
  return ok;
}  /*doset*/


/* ****************************************************** clvcmd ***** */
/* *                                                                 * */
/* *                     CURLEW :: "v" (view)                        * */
/* *                                                                 * */
/* ******************************************************************* */


/* doview -- full-screen mode entry */
Local stcode doview(line, achar, LINK)
long line;
uchar achar;
struct LOC_editor *LINK;
{
  uchar action;
  fstype ls;
  boolean chsv;
  rowtext ln;
  chstring s;
  char r;
  uchar c;
  long nd, j;

  action = achar;

  /* find out what to do */
  if (action == plus) {  /* full-screen wanted */
    if (LINK->appstate == fsereq)   /* refresh; full-screen */
      action = equals;
  } else if (action == minus) {
    if (LINK->appstate == coned)   /* refresh; context */
      action = equals;
  } else if (action != equals) {
    if (LINK->appstate == coned)
      action = plus;
    else
      action = minus;
  }

  /* manage any state change */
  if (action == equals) {  /* refresh from either state */
    if (LINK->appstate == fsereq) {
      refresh();   /* file comparison may correct */
      if (LINK->nlines > 0)
	LINK->topln = max(line, 1L);
      ls = LINK->fstate;
      action = plus;
    } else {
      refresh();   /* remake image */
      edname(LINK);   /* only fix-up required */
      action = minus;
    }
  } else if (action == plus) {
    LINK->appstate = fsereq;
    ls = normal;
    LINK->vbmark = false;
    LINK->topln = max(line, 1L);
    LINK->fsrow = LINK->toprow;
    LINK->fscol = 0;
    LINK->fschanged = false;
    LINK->fsline = 0;
    chsv = LINK->changed;
    fetch(LINK->fsrow, LINK);   /* make sure top line in buffer */
    LINK->changed = chsv;   /* reset if just one blank line */
    LINK->curln = LINK->topln;
    checkpoint(LINK);
  } else {
    replace(LINK);
    LINK->appstate = coned;
    LINK->cstate = concmd;
    LINK->cmdcol = 0;
    ctxtimage(LINK);
    getloc(&r, &c);
    getrow(1, ln);   /* all minuses from ctxtimage */
    setstring(s, " leaving full-screen ope");
    appstring(s, "ration &                ");
    slcopy(s, 1L, ln, 10, 40L);
    nd = itoc(LINK->topln, s, 1L);
    j = maxcol() - 10;
    ln[j] = blank;
    slcopy(s, 1L, ln, (int)(j + 1), 10L);
    ln[j + nd] = blank;
    putrow(maxrow() - 1, ln);
    setcursor(r, c);
    LINK->curln = min(LINK->topln, LINK->lastln);
    if (LINK->ciview)   /* get ready for next view */
      LINK->nfsomode[icharmode] = 1;
    else
      LINK->nfsomode[icharmode] = 0;
  }

  /* if full-screen, the image needs to be set up */
  if (action != plus)  /* full-screen image */
    return ok;

  header(LINK);
  viewstate(ls, LINK);   /* also sets mode array */
  ruler(LINK->toprow - 1, LINK);
  fileimage(LINK);
  ruler(LINK->botrow + 1, LINK);
  if (!LINK->msgdone)
    linestat(LINK);
  setupdate(LINK->toprow, LINK->botrow, 0, maxcol());
  setcursor(LINK->fsrow, LINK->fscol);
  return ok;

  /* context wanted */
  /* otherwise flip the state */
  /* full-screen from context */
  /* context from full-screen */
}  /*doview*/


/* exitview -- exit from full-screen forced by other commands */
Local stcode exitview(LINK)
struct LOC_editor *LINK;
{
  stcode junk;

  if (LINK->appstate == fsereq)
    junk = doview(min(LINK->topln, LINK->lastln), minus, LINK);
  return ok;
}  /*exitview*/


/* ****************************************************** clecmd ***** */
/* *                                                                 * */
/* *                      CURLEW :: "e" (edit)                       * */
/* *                                                                 * */
/* ******************************************************************* */


/* startlog -- start log for new editor session */
Local Void startlog(LINK)
struct LOC_editor *LINK;
{
  chstring name;
  long j, nd;

  setstring(name, "curlew&                 ");
  LINK->filenbr++;
  if (LINK->filenbr > 1) {   /* mark "edit" calls by number */
    j = length_(name) + 1;
    name[j - 1] = minus;
    nd = itoc(LINK->filenbr, name, j + 1);
  }
  loguse(name);
}  /*startlog*/


/* doedit -- edit new file, clears buffer and reads in file */
Local stcode doedit(LINK)
struct LOC_editor *LINK;
{
  stcode Result;

  scopy(LINK->newfile, 1L, LINK->savefile, 1L);
  clrbuf(LINK);
  startlog(LINK);
  setbuf(LINK);
  LINK->vtitle[0] = newline;
  LINK->vtitle[1] = endstr;   /* clear any title */
  Result = doread(0L, LINK->savefile, false, ioerror_, LINK);
      /* read in the file */
  if (LINK->jfout != ioerror_) {  /* log only after recovery! */
    jfcontrol((long)iowrite, LINK);
    journal(0L, 0L, lete, LINK->savefile, LINK);
  }
  LINK->curln = min(1L, LINK->lastln);
  LINK->changed = false;
  if (LINK->appstate == fsereq) {  /* autoview, already in "v" */
    LINK->topln = 1;
    LINK->fsrow = LINK->toprow;
    LINK->fscol = 0;
    LINK->fschanged = false;
    LINK->fsline = 0;
    fetch(LINK->fsrow, LINK);   /* make sure top line in buffer */
    LINK->changed = false;   /* reset if just one blank line */
    LINK->curln = LINK->topln;
    checkpoint(LINK);
    header(LINK);   /* new filename */
    viewstate(normal, LINK);   /* just in case */
    fileimage(LINK);   /* new file image */
    if (!LINK->msgdone)
      linestat(LINK);
    return Result;
  }
  if (LINK->autoview) {  /* autoview, image to change */
    LINK->status = doview(LINK->curln, plus, LINK);
    lastcb(LINK);
  }
  return Result;
}  /*doedit*/

/* Local variables for doshow: */
struct LOC_doshow {
  struct LOC_editor *LINK;
  boolean first;
} ;


/* shtext -- show text description for each profile entry */
Local Void shtext(txt, letter, LINK)
Char *txt;
uchar letter;
struct LOC_doshow *LINK;
{
  uchar ccol, pcol, tcol;

  if (LINK->first)
    LINK->first = false;
  else {
    ccol = showposn(LINK->LINK);
    pcol = maxcol() / 2;
    if (ccol >= pcol)
      showc(newline, LINK->LINK);
    else {
      for (tcol = ccol + 1; tcol <= pcol; tcol++)
	showc(blank, LINK->LINK);
    }
  }
  showc(blank, LINK->LINK);
  showc(blank, LINK->LINK);
  showlit(txt, LINK->LINK);
  if (letter != nul) {
    showlit("  :  &                  ", LINK->LINK);
    showc(letter, LINK->LINK);
  }
}  /*shtext*/


/* shswitch -- show value of boolean as plus or minus */
Local Void shswitch(txt, letter, sw, LINK)
Char *txt;
uchar letter;
boolean sw;
struct LOC_doshow *LINK;
{
  shtext(txt, letter, LINK);
  if (sw)
    showc(plus, LINK->LINK);
  else
    showc(minus, LINK->LINK);
}  /*shswitch*/


/* shnumber -- show numerical value */
Local Void shnumber(txt, letter, nbr, LINK)
Char *txt;
uchar letter;
long nbr;
struct LOC_doshow *LINK;
{
  shtext(txt, letter, LINK);
  showdec(nbr, 1L, 0L, LINK->LINK);
}  /*shnumber*/


/* shtstops -- show tabulation stops */
Local Void shtstops(txt, letter, LINK)
Char *txt;
uchar letter;
struct LOC_doshow *LINK;
{
  uchar tc, FORLIM;

  shtext(txt, letter, LINK);
  LINK->LINK->cindent = 10;
  if (LINK->LINK->tabrpt > 0) {
    showc(star, LINK->LINK);
    showdec(LINK->LINK->tabrpt, 1L, 0L, LINK->LINK);
    return;
  }
  FORLIM = maxcol();
  for (tc = 0; tc <= FORLIM; tc++) {
    if (LINK->LINK->tabcols[tc] == tabset) {
      showdec(tc + 1L, 1L, 0L, LINK->LINK);
      showc(blank, LINK->LINK);
    }
  }
}  /*shtstops*/


/* shfnkey -- show function key string */
Local Void shfnkey(nbr, LINK)
long nbr;
struct LOC_doshow *LINK;
{
  long k;
  uchar c;

  k = LINK->LINK->kptrs[nbr - minkey];
  c = LINK->LINK->kdefs[k - 1];
  if (c == newline)
    return;
  shtext("  F&                    ", nul, LINK);
  showdec(nbr, 1L, 3L, LINK->LINK);
  if (nbr < 10)
    showc(blank, LINK->LINK);
  LINK->LINK->cindent = 10;
  do {
    showc(c, LINK->LINK);
    k++;
    c = LINK->LINK->kdefs[k - 1];
  } while (c != newline);
}  /*shfnkey*/


/* ****************************************************** clshcmd **** */
/* *                                                                 * */
/* *                      CURLEW :: "sh" (show)                      * */
/* *                                                                 * */
/* ******************************************************************* */


/* doshow -- show editor profile */
Local stcode doshow(LINK)
struct LOC_editor *LINK;
{  /* doshow main routine */
  struct LOC_doshow V;
  stcode junk;
  long j;
  chstring s;


  V.LINK = LINK;
  junk = exitview(LINK);
  showc(newline, LINK);
  V.first = true;
  shswitch("Append line on RETURN   ", leta, LINK->rappend, &V);
  shswitch("Block mode paragraphing ", letb, LINK->blkpara, &V);
  shswitch("Character insertion set ", letc, LINK->ciview, &V);
  shswitch("Detab file buffer text  ", letd, LINK->detab, &V);
  shswitch("Entab when writing file ", lete, LINK->entab, &V);
  shswitch("Horizontal tab keys     ", leth, LINK->htkeys, &V);
  shswitch("Image updating by host  ", leti, LINK->autoimage, &V);
  shswitch("Justify both margins    ", letj, LINK->gblfmt, &V);
  shswitch("Loop search over buffer ", letl, LINK->loopsearch, &V);
  shnumber("Mask control characters ", letm, LINK->mlevel, &V);
  shswitch("Anycase at pattern start", letn, LINK->acsearch, &V);
  shnumber("Formatted text offset   ", leto, LINK->gbloset, &V);
  shnumber("Paragraph indentation   ", letp, LINK->gblpara, &V);
  shnumber("Rendition (alternate)   ", letr, LINK->rrend, &V);
  shnumber("Sentence separation     ", lets, LINK->gbleos + 1, &V);
  shtstops("Tabulation stops        ", lett, &V);
  shswitch("UNIX (tm) metacharacters", letu, LINK->alttracks, &V);
  shswitch("View command on entry   ", letv, LINK->autoview, &V);
  shnumber("Width of paragraph      ", letw, LINK->gblwidth, &V);
  shnumber("Left margin offset      ", letx, LINK->xoset, &V);
  shnumber("Vertical cursor action  ", lety, LINK->cscroll, &V);
  shswitch("Word wrap (power-typing)", letz, LINK->inswrap, &V);

  showc(newline, LINK);
  showc(newline, LINK);
  V.first = true;
  shtext("Function key strings    ", capf, &V);
  showdec((long)minkey, 1L, 0L, LINK);
  showc(period, LINK);
  showc(period, LINK);
  showdec((long)maxkey, 1L, -1L, LINK);
  V.first = true;
  for (j = minkey; j <= maxkey; j++)
    shfnkey(j, &V);

  showc(newline, LINK);
  showc(newline, LINK);
  showlit("  Version code :  pr188 ", LINK);   /* curlew source code */
  showlit("&   (interface : ?????) ", LINK);   /* system interface */
  getinf((long)clprofile, s, (long)maxstr);
  showlit("@n  Profile file :  &   ", LINK);
  LINK->cindent = 18;
  showstr(s, LINK);
  showc(newline, LINK);

  return ok;
}  /*doshow*/


/* ****************************************************** clhcmd ***** */
/* *                                                                 * */
/* *                      CURLEW :: "h" (help)                       * */
/* *                                                                 * */
/* ******************************************************************* */


/* help -- display help summary information */
Local stcode help(lin, i, LINK)
uchar *lin;
long i;
struct LOC_editor *LINK;
{
  short fd;
  chstring s, hfn;
  stcode stat, junk;
  long cmdrc;

  stat = ok;
  getinf((long)clhelpcmd, s, (long)maxstr);
  if (s[0] != endstr) {  /* call system help utility */
    syscmd(s, &cmdrc);
    if (LINK->appstate == fsereq)
      linestat(LINK);
    return stat;
  }
  getinf((long)clhelpfn, hfn, (long)maxstr);
  fd = openf(hfn, (long)ioread);
  if (fd == ioerror_) {
    showlit("  *** Cannot read the he", LINK);
    showlit("lp file :  &            ", LINK);
    showstr(hfn, LINK);
    showc(newline, LINK);
    LINK->msgdone = true;
    stat = err;
    return stat;
  }
  junk = exitview(LINK);
  while (getline(s, fd, (long)maxstr))
    showstr(s, LINK);
  closef(fd);
  showlit("@n@nFILES@n&            ", LINK);
  getinf((long)clprofile, s, (long)maxstr);
  showlit("@n  Profile file :  &   ", LINK);
  LINK->cindent = 18;
  showstr(s, LINK);
  showlit("@n  Summary file :  &   ", LINK);
  showstr(hfn, LINK);
  showc(newline, LINK);
  return stat;
}  /*help*/


/* ****************************************************** clnulcmd *** */
/* *                                                                 * */
/* *              CURLEW :: null command (relocation)                * */
/* *                                                                 * */
/* ******************************************************************* */


/* nullcmd -- null command in either context or full-screen operation */
Local stcode nullcmd(glob, LINK)
boolean glob;
struct LOC_editor *LINK;
{
  boolean up;
  long nr, mr, cs;
  stcode junk;

  if (glob) {
    cs = LINK->curln;
    junk = exitview(LINK);
    LINK->curln = cs;
  }
  if (LINK->appstate == fsereq) {  /* full-screen relocation */
    if (LINK->nlines == 0) {
      showlit("  Request cancelled@n&  ", LINK);
      return ok;
    } else if (LINK->line2 < 1)
      return err;
    else {
      up = true;
      nr = LINK->line2 - LINK->topln;
      if (LINK->line2 != LINK->topln) {
	if (nr < 0) {
	  nr = -nr;
	  up = false;
	}
	mr = maxrow() / 4 * 3;   /* scrolling limit */
	if (nr <= mr) {
	  if (LINK->nlines == 1 || !LINK->topused) {
	    if (up)
	      scrollup(LINK->toprow, LINK->botrow, (int)nr);
	    else
	      scrolldown(LINK->toprow, LINK->botrow, (int)nr);
	  }
	}
	LINK->topln = LINK->line2;
      }
      if (LINK->topused)   /* ..was "(nlines = 1) and ..." */
      {  /* check for char still in view */
	if (up) {
	  if (LINK->fsrow - LINK->toprow > nr)
	    LINK->fsrow -= nr;
	  else
	    LINK->fsrow = LINK->toprow;
	} else {
	  if (LINK->botrow - LINK->fsrow > nr)
	    LINK->fsrow += nr;
	  else
	    LINK->fsrow = LINK->botrow;
	}
      } else
	LINK->fsrow = LINK->toprow;
      /* otherwise cursor to top */
      linestat(LINK);
      return ok;
    }
  } else {  /* context null command */
    if (LINK->nlines != 0)
      return (doprint(LINK->line2, LINK->line2, true, LINK));
    LINK->line2 = LINK->curln;
    if (glob)
      return (doprint(LINK->line2, LINK->line2, true, LINK));
    LINK->line2 = nextln(LINK->curln, LINK);
    if (LINK->line2 != 0) {
      LINK->cfirst++;   /* suppress null cmd echo */
      return (doprint(LINK->line2, LINK->line2, true, LINK));
    }
    if (LINK->lastln < 1)
      showlit("  *** Buffer is empty@n&", LINK);
    else
      showlit("  *** At end of buffer@n", LINK);
    LINK->msgdone = true;
    return (doprint(LINK->line2, LINK->line2, true, LINK));
  }
}  /*nullcmd*/


/* ****************************************************** clcmd ****** */
/* *                                                                 * */
/* *       CURLEW :: Recognition of commands other than globals      * */
/* *                                                                 * */
/* ******************************************************************* */


/* testverb -- test command verb against possible matches */
Local Void testverb(lin, i, guess, cmdlist, verb, LINK)
uchar *lin;
long *i;
cmdtype guess;
Char *cmdlist;
cmdtype *verb;
struct LOC_editor *LINK;
{
  long j, jmax, k;
  chstring cl;
  uchar ch;
  boolean found, looking;

  *verb = guess;
  setstring(cl, cmdlist);
  j = *i;
  jmax = j;
  k = 1;
  looking = true;
  do {
    ch = lowercase(lin[j - 1]);
    found = (!islower_(ch) || cl[k - 1] == slash || cl[k - 1] == endstr);
    if (found)
      *i = j - 1;
    else {
      if (ch == cl[k - 1])
	j++;   /* point at next character */
      else {
	jmax = max(jmax, j);
	do {
	  k++;
	} while (cl[k - 1] != slash && cl[k - 1] != endstr);
	if (cl[k - 1] == endstr)
	  looking = false;   /* no more in list */
	else {
	  j = *i;   /* start again for next */
	  *verb = (cmdtype)((long)(*verb) + 1);   /* bump command guess */
	}
      }
      k++;
    }
  } while (!found && looking);

  if (found)
    return;
  *verb = badcmd;
  if (guess == yesrep)
    showlit("  *** Invalid reply (&  ", LINK);
  else
    showlit("  *** Invalid command (&", LINK);
  for (j = *i - 1; j < jmax; j++)
    showc(lowercase(lin[j]), LINK);
  showc(rparen, LINK);
  if (cl[0] != question) {
    showc(semicol, LINK);
    showc(blank, LINK);
    showstr(cl, LINK);
    showlit(" intended?&             ", LINK);
  }
  showc(newline, LINK);
  LINK->msgdone = true;
}  /*testverb*/


/* cmdverb -- identify context editor command verb */
Local cmdtype cmdverb(lin, i, LINK)
uchar *lin;
long *i;
struct LOC_editor *LINK;
{
  uchar firstch;
  cmdtype verb;
  textlit badmask;

  verb = badcmd;
  memcpy(badmask, "?&                      ", sizeof(textlit));
  firstch = lowercase(lin[*i - 1]);
  if (!isletter(firstch))
    return verb;

  switch (firstch) {

  case leta:
    testverb(lin, i, acmd, "append&                 ", &verb, LINK);
    break;

  case letb:
    testverb(lin, i, badcmd, badmask, &verb, LINK);
    break;

  case letc:
    testverb(lin, i, ccmd, "change/copy/curlew/cl&  ", &verb, LINK);
    break;

  case letd:
    testverb(lin, i, dcmd, "delete&                 ", &verb, LINK);
    break;

  case lete:
    testverb(lin, i, ecmd, "edit/exit&              ", &verb, LINK);
    break;

  case letf:
    testverb(lin, i, fcmd, "filename/format&        ", &verb, LINK);
    break;

  case letg:
    testverb(lin, i, badcmd, badmask, &verb, LINK);
    break;

  case leth:
    testverb(lin, i, hcmd, "help&                   ", &verb, LINK);
    break;

  case leti:
    testverb(lin, i, icmd, "insert&                 ", &verb, LINK);
    break;

  case letj:
    testverb(lin, i, badcmd, badmask, &verb, LINK);
    break;

  case letk:
    testverb(lin, i, kcmd, "keep&                   ", &verb, LINK);
    break;

  case letl:
    testverb(lin, i, lcmd, "list&                   ", &verb, LINK);
    break;

  case letm:
    testverb(lin, i, mcmd, "move&                   ", &verb, LINK);
    break;

  case letn:
  case leto:
    testverb(lin, i, badcmd, badmask, &verb, LINK);
    break;

  case letp:
    testverb(lin, i, pcmd, "print/prompt&           ", &verb, LINK);
    break;

  case letq:
    testverb(lin, i, qcmd, "quit&                   ", &verb, LINK);
    break;

  case letr:
    testverb(lin, i, rcmd, "read&                   ", &verb, LINK);
    break;

  case lets:
    testverb(lin, i, scmd, "substitute/set/show/stop", &verb, LINK);
    break;

  case lett:
    testverb(lin, i, badcmd, badmask, &verb, LINK);
    break;

  case letu:
    testverb(lin, i, ucmd, "undo&                   ", &verb, LINK);
    break;

  case letv:
    testverb(lin, i, vcmd, "view&                   ", &verb, LINK);
    break;

  case letw:
    testverb(lin, i, wcmd, "write&                  ", &verb, LINK);
    break;

  case letx:
  case lety:
  case letz:
    testverb(lin, i, badcmd, badmask, &verb, LINK);
    break;

  }/*case*/
  return verb;
}  /*cmdverb*/


/* docmd -- handle all commands except globals */
Local stcode docmd(lin, i, glob, status, LINK)
uchar *lin;
long *i;
boolean glob;
stcode *status;
struct LOC_editor *LINK;
{
  chstring fil, sub;
  long line3, cs;
  boolean gflag, pflag, lflag;
  uchar ch;
  stcode junk;

  pflag = false;
  lflag = true;
  *status = err;
  ch = lowercase(lin[*i - 1]);
  if (ch == newline)
    *status = nullcmd(glob, LINK);   /* null command */
  else {  /* command letter? */
    switch (cmdverb(lin, i, LINK)) {

    case badcmd:   /* error */
      break;

    case acmd:   /* append command */
      if (lin[*i] == newline)
	*status = exitview(LINK);
      *status = doappend(LINK->line2, lin, *i + 1, glob, LINK);
      break;

    case ccmd:   /* change command */
      if (default_(LINK->curln, LINK->curln, status, LINK) == ok) {
	if (lndelete(LINK->line1, LINK->line2, status, LINK) == ok) {
	  if (lin[*i] == newline)
	    *status = exitview(LINK);
	  *status = doappend(prevln(LINK->line1, LINK), lin, *i + 1, glob,
			     LINK);
	}
      }
      break;

    case cocmd:   /* copy command */
      pflag = (!glob && LINK->appstate != fsereq);
      (*i)++;
      if (getone(lin, i, &line3, status, LINK) == enddata) {
	line3 = LINK->curln;
	*status = ok;
      }
      if (lin[*i - 1] != newline)
	*status = err;
      if (*status == ok) {
	if (default_(LINK->curln, LINK->curln, status, LINK) == ok)
	  *status = copy_(line3, glob, LINK);
      }
      break;

    case dcmd:   /* delete command */
      pflag = (!glob && LINK->appstate != fsereq);
      if (lin[*i] == newline) {
	if (default_(LINK->curln, LINK->curln, status, LINK) == ok)
	  *status = dodelete(LINK->line1, LINK->line2, glob, LINK);
      }
      if (LINK->lastln <= 0)
	pflag = false;
      break;

    case ecmd:
    case cucmd:
    case clcmd:   /* edit (curlew, cl) command */
      if (LINK->nlines == 0) {
	if (getfn(lin, i, fil, LINK) == ok) {
	  if (!LINK->autoview || LINK->changed)
	    *status = exitview(LINK);
	  scopy(fil, 1L, LINK->newfile, 1L);
	  if (!LINK->changed)
	    *status = doedit(LINK);   /* just do it */
	  else {
	    *status = ok;
	    LINK->cstate = conecmd;   /* check first */
	  }
	}
      }
      break;

    case excmd:   /* exit command */
      if (lin[*i] == newline && LINK->nlines == 0 && !glob) {
	*status = exitview(LINK);
	*status = dowrite(1L, LINK->lastln, LINK->savefile, LINK);
	if (*status == ok)
	  *status = enddata;
	else {
	  *status = ok;
	  LINK->cstate = conqcmd;   /* check first */
	}
      }
      break;

    case fcmd:   /* filename command */
      if (LINK->nlines == 0) {
	scopy(LINK->savefile, 1L, sub, 1L);
	if (getfn(lin, i, fil, LINK) == ok)
	  scopy(fil, 1L, LINK->savefile, 1L);
	if ((LINK->savefile[0] != endstr) & (!equal(sub, LINK->savefile))) {
	  journal(0L, 0L, letf, LINK->savefile, LINK);
	  LINK->changed = true;
	}
	filestat(LINK);
	*status = ok;
      }
      break;

    case focmd:   /* format command */
      if (!glob) {
	if (fmtdef(status, LINK) == ok)
	  *status = dofmt(LINK->line1, LINK->line2, lin, *i + 1, LINK);
      }
      break;

    case hcmd:   /* help command */
      if (LINK->nlines == 0 && !glob)
	*status = help(lin, *i + 1, LINK);
      break;

    case icmd:   /* insert command */
      if (lin[*i] == newline)
	*status = exitview(LINK);
      if (LINK->line2 == 0)
	*status = doappend(0L, lin, *i + 1, glob, LINK);
      else
	*status = doappend(prevln(LINK->line2, LINK), lin, *i + 1, glob, LINK);
      break;

    case kcmd:   /* keep command */
      if (LINK->nlines == 0 && lin[*i] == newline && !glob) {
	checkpoint(LINK);
	if (!LINK->rebuild)
	  jfcontrol((long)ioappend, LINK);
	showlit("  Buffer checkpointed@n&", LINK);
	*status = ok;
      }
      break;

    case lcmd:   /* list command */
      if (lin[*i] == newline) {
	if (default_(LINK->curln, LINK->curln, status, LINK) == ok) {
	  *status = exitview(LINK);
	  *status = doprint(LINK->line1, LINK->line2, true, LINK);
	}
      }
      break;

    case mcmd:   /* move command */
      pflag = (!glob && LINK->appstate != fsereq);
      (*i)++;
      if (getone(lin, i, &line3, status, LINK) == enddata)
	*status = err;
      if (lin[*i - 1] != newline)
	*status = err;
      if (*status == ok) {
	if (default_(LINK->curln, LINK->curln, status, LINK) == ok)
	  *status = move_(line3, glob, LINK);
      }
      break;

    case pcmd:   /* print command */
      if (lin[*i] == newline) {
	if (default_(LINK->curln, LINK->curln, status, LINK) == ok) {
	  *status = exitview(LINK);
	  *status = doprint(LINK->line1, LINK->line2, false, LINK);
	}
      }
      break;

    case prcmd:   /* prompt command */
      if (LINK->appstate == fsereq && LINK->nlines == 0 &&
	  lin[*i] == newline && !glob) {
	LINK->appstate = fsecmd;   /* next time, read command */
	LINK->cmdcol = 0;
	*status = ok;
      }
      break;

    case qcmd:
    case stcmd:   /* quit (stop) command */
      if (lin[*i] == newline && LINK->nlines == 0 && !glob) {
	*status = exitview(LINK);
	*status = enddata;
	if (LINK->changed) {
	  *status = ok;
	  LINK->cstate = conqcmd;   /* check first */
	}
      }
      break;

    case rcmd:   /* read command */
      *status = getfn(lin, i, fil, LINK);
      *status = doread(LINK->line2, fil, glob, LINK->jfout, LINK);
      break;

    case scmd:   /* substitute command */
      (*i)++;
      skipbl(lin, i);
      if (optpat(lin, i, LINK) == ok) {
	if (getrhs(lin, i, sub, &gflag, LINK) == ok) {
	  if (ckp(lin, *i + 1, &pflag, &lflag, status, LINK) == ok) {
	    if (default_(LINK->curln, LINK->curln, status, LINK) == ok) {
	      if (LINK->appstate == fsereq && pflag) {
		cs = LINK->curln;
		junk = exitview(LINK);
		LINK->curln = cs;
	      }
	      *status = subst(sub, gflag, glob, LINK);
	    }
	  }
	}
      }
      break;

    case secmd:   /* set command */
      if (LINK->nlines == 0 && !glob)
	*status = doset(lin, *i + 1, LINK);
      break;

    case shcmd:   /* show command */
      if (lin[*i] == newline && LINK->nlines == 0 && !glob)
	*status = doshow(LINK);
      break;

    case ucmd:   /* undo command */
      if (LINK->nlines == 0 && lin[*i] == newline && !glob) {
	restore(LINK);   /* exchange checkpoints */
	journal(0L, 0L, letu, lin, LINK);
	showlit("  Buffer restored from l", LINK);
	showlit("ast checkpoint@n&       ", LINK);
	filestat(LINK);
	*status = ok;
      }
      break;

    case vcmd:   /* view command */
      if (!glob) {
	(*i)++;
	skipbl(lin, i);
	*status = default_(LINK->curln, LINK->curln, status, LINK);
	if (lin[*i - 1] == plus)
	  scopy(lin, *i + 1, LINK->vtitle, 1L);
	*status = doview(LINK->line2, lin[*i - 1], LINK);
	if (LINK->appstate == coned)
	  LINK->cfirst++;   /* suppress cmd echo */
      }
      break;

    case wcmd:   /* write command */
      if (!glob) {
	*status = getfn(lin, i, fil, LINK);
	if (default_(1L, LINK->lastln, status, LINK) == ok)
	  *status = dowrite(LINK->line1, LINK->line2, fil, LINK);
      }
      break;

    }/*case*/
  }

  if (*status == ok && pflag)
    *status = doprint(LINK->curln, LINK->curln, lflag, LINK);
  return (*status);
}  /*docmd*/


/* ****************************************************** clglob ***** */
/* *                                                                 * */
/* *             CURLEW :: "g" (global) and "x" (xglobal)            * */
/* *                                                                 * */
/* ******************************************************************* */


/* function ckglob -- if global prefix, mark lines to be affected */
Local stcode ckglob(lin, i, status, LINK)
uchar *lin;
long *i;
stcode *status;
struct LOC_editor *LINK;
{
  long n;
  boolean gflag;
  chstring temp;
  uchar ch;
  cmdtype verb;
  long FORLIM;

  *status = ok;
  ch = lowercase(lin[*i - 1]);
  if (ch == letg || ch == letx) {
    testverb(lin, i, gcmd, "global/xglobal&         ", &verb, LINK);
    if (verb == badcmd)
      *status = err;
  } else
    *status = enddata;

  if (*status != ok)
    return (*status);
  gflag = (verb == gcmd);
  (*i)++;
  skipbl(lin, i);
  if (optpat(lin, i, LINK) == err) {
    *status = err;
    return (*status);
  }
  if (default_(1L, LINK->lastln, status, LINK) == err)
	/* mark affected lines */
	  return (*status);
  (*i)++;
  skipbl(lin, i);
  FORLIM = LINK->line2;
  /* mark affected lines */
  for (n = LINK->line1; n <= FORLIM; n++) {
    gettxt(n, temp, LINK);
    putmark(n, match(temp, LINK->pat) == gflag, LINK);
  }
  FORLIM = LINK->line1;
  /* erase all other marks */
  for (n = 0; n < FORLIM; n++)
    putmark(n, false, LINK);
  FORLIM = LINK->lastln;
  for (n = LINK->line2 + 1; n <= FORLIM; n++)
    putmark(n, false, LINK);
  *status = ok;
  return (*status);
}  /*ckglob*/


/* doglob -- do command at lin[i] on all marked lines */
Local stcode doglob(lin, i, cursave, status, LINK)
uchar *lin;
long *i, *cursave;
stcode *status;
struct LOC_editor *LINK;
{
  long count, nproc, istart, n;

  checkpoint(LINK);
  *status = ok;
  count = 0;
  nproc = 0;
  n = LINK->line1;
  istart = *i;
  do {
    if (getmark(n, LINK)) {
      nproc++;
      putmark(n, false, LINK);
      LINK->curln = n;
      *cursave = LINK->curln;
      *i = istart;
      if (getlist(lin, i, status, LINK) == ok) {
	if (docmd(lin, i, true, status, LINK) == ok)
	  count = 0;
      }
    } else {
      n = nextln(n, LINK);
      count++;
    }
  } while (count <= LINK->lastln && *status == ok);
  if (*status != ok)
    return (*status);
  showlit("  Text lines processed :", LINK);
  showc(blank, LINK);
  showc(blank, LINK);
  showdec(nproc, 1L, -1L, LINK);
  return (*status);
}  /*doglob*/


/* ****************************************************** clpfkey **** */
/* *                                                                 * */
/* *            CURLEW :: Program function key processing            * */
/* *                                                                 * */
/* ******************************************************************* */


/* fnkey -- check token request code for program function key */
Local long fnkey(rc, key, LINK)
long rc, *key;
struct LOC_editor *LINK;
{
  uchar lc;

  *key = 0;
  if (isdigit_((int)rc)) {
    *key = rc - dig0;
    if (*key == 0)
      *key = 10;
    return (*key);
  }
  if (!isletter((int)rc)) {
    if (rc == semicol)
      *key = 30;
    return (*key);
  }
  lc = lowercase((int)rc);
  switch (lc) {

  case letq:
    *key = 11;
    break;

  case letw:
    *key = 12;
    break;

  case lete:
    *key = 13;
    break;

  case letr:
    *key = 14;
    break;

  case lett:
    *key = 15;
    break;

  case lety:
    *key = 16;
    break;

  case letu:
    *key = 17;
    break;

  case leti:
    *key = 18;
    break;

  case leto:
    *key = 19;
    break;

  case letp:
    *key = 20;
    break;

  case leta:
    *key = 21;
    break;

  case lets:
    *key = 22;
    break;

  case letd:
    *key = 23;
    break;

  case letf:   /* last "Fawn Book" PF key */
    *key = 24;
    break;

  case letg:
    *key = 25;
    break;

  case leth:
    *key = 26;
    break;

  case letj:
    *key = 27;
    break;

  case letk:
    *key = 28;
    break;

  case letl:
    *key = 29;
    break;

  /* semi-colon is PF key 30 */
  case letz:
    *key = 31;
    break;

  case letx:
    *key = 32;
    break;

  case letc:
    *key = 33;
    break;

  case letv:
    *key = 34;
    break;

  case letb:
    *key = 35;
    break;

  case letn:
    *key = 36;
    break;

  case letm:
    *key = 37;
    break;

  }/* case */
  return (*key);
}  /*fnkey*/


/* getprevious -- pick up previous text and place on command line */
Local Void getprevious(LINK)
struct LOC_editor *LINK;
{
  char r;
  uchar c, mc;
  long i, j;
  rowtext ln;

  getloc(&r, &c);
  mc = maxcol();
  if (r != LINK->cmdrow) {
    getrow(r, ln);
    i = 1;
    j = c;
    while (j <= mc) {
      LINK->prevlin[i - 1] = ln[j];
      i++;
      j++;
    }
    LINK->prevlin[i - 1] = newline;
    LINK->prevlin[i] = endstr;
  }

  setcursor(LINK->cmdrow, LINK->cmdcol);
  erasetoright();
  getrow(LINK->cmdrow, ln);
  slcopy(LINK->prevlin, 1L, ln, LINK->cmdcol, maxcol() - LINK->cmdcol + 1L);
  putrow(LINK->cmdrow, ln);
  getloc(&r, &c);
  if (c < LINK->cmdcol)
    setcursor(LINK->cmdrow, LINK->cmdcol);
}  /*getprevious*/


/* plantkeynbr -- place function key code in command line */
Local Void plantkeynbr(n, LINK)
long n;
struct LOC_editor *LINK;
{
  long i, j;

  setcursor(LINK->cmdrow, LINK->cmdcol);
  textchar(keyleadin);
  i = n / 10;
  j = n % 10;
/* p2c: cl-sun.p, line 8024:
 * Note: Using % for possibly-negative arguments [317] */
  textchar((int)(i + dig0));
  textchar((int)(j + dig0));
  erasetoright();
}  /*plantkeynbr*/


/* getkeydef -- return string s associated with Fn */
Local Void getkeydef(n, s, LINK)
long n;
uchar *s;
struct LOC_editor *LINK;
{
  long i, j;
  uchar c;

  i = LINK->kptrs[n - minkey];
  j = 1;
  do {
    c = LINK->kdefs[i - 1];
    s[j - 1] = c;
    j++;
    i++;
  } while (c != newline);
  s[j - 1] = endstr;
}  /*getkeydef*/


/* subkeytext -- substitute function key definition for "@n" */
Local Void subkeytext(lin, LINK)
uchar *lin;
struct LOC_editor *LINK;
{
  long n;

  n = 0;
  if (isdigit_(lin[1])) {
    n = lin[1] - dig0;
    if (isdigit_(lin[2]))
      n = n * 10 + lin[2] - dig0;
  }
  if (n >= minkey && n <= maxkey)
    getkeydef(n, lin, LINK);
  else {
    lin[0] = newline;
    lin[1] = endstr;
  }
}  /*subkeytext*/


/* cfkcmd -- set up context editor command from function key literal */
Local Void cfkcmd(lit, LINK)
Char *lit;
struct LOC_editor *LINK;
{
  long j;

  setstring(LINK->prevlin, lit);
  j = length_(LINK->prevlin);
  LINK->prevlin[j] = newline;
  LINK->prevlin[j + 1] = endstr;
  setcursor(LINK->cmdrow, LINK->cmdcol);
  getprevious(LINK);
}  /*cfkcmd*/


/* ****************************************************** clyesno **** */
/* *                                                                 * */
/* *                CURLEW :: Yes/No reply processing                * */
/* *                                                                 * */
/* ******************************************************************* */


/* ckyes -- check for "y" reply to e/q prompt */
Local stcode ckyes(lin, LINK)
uchar *lin;
struct LOC_editor *LINK;
{
  long i;
  cmdtype verb;
  stcode stat;

  i = 1;
  skipbl(lin, &i);
  verb = norep;   /* all function keys cancel */
  if (isletter(lin[i - 1]))
    testverb(lin, &i, yesrep, "yes/no/ok/cancel&       ", &verb, LINK);
  if (verb == yesrep || verb == okrep)
    stat = enddata;
  else if (verb == norep || verb == canrep) {
    showlit("  *** Use a \"write\" comm", LINK);
    showlit("and to save your work.@n", LINK);
    stat = ok;
  } else
    stat = err;
  if (stat != err)
    LINK->cstate = concmd;
  return stat;
}  /*ckyes*/


/* ****************************************************** clcontxt *** */
/* *                                                                 * */
/* *           CURLEW :: Context editor command dispatch             * */
/* *                                                                 * */
/* ******************************************************************* */


/* context -- control passed to context editor */
Local Void context(rc, LINK)
uchar rc;
struct LOC_editor *LINK;
{
  rowtext ln;
  chstring lin, fks;
  long i, j, k, len;
  char r;
  uchar c;
  long acode;
  stcode junk;
  long cmdrc;
  boolean cmasked, dopfkey, resend;
  long FORLIM;

  dopfkey = false;
  resend = false;
  if (rc > del) {
    setmode(knextfld, 50);   /* next field key ... */
    setmode(kprevfld, 53);   /* prev field key ... */
    setmode(knextfld, 0);
    setmode(kprevfld, 0);   /* reset both */
    getloc(&r, &c);
    if (LINK->appstate == coned)
      edname(LINK);
    else {
      ruler(LINK->toprow - 1, LINK);
      ruler(LINK->botrow + 1, LINK);
    }
    setcursor(r, c);
    acode = nul;
  } else if (fnkey((long)rc, &acode, LINK) == 0) {
    acode = rc;
    getloc(&r, &c);
    resend = (acode == ctlm && r != LINK->cmdrow);
  } else
    dopfkey = true;

  if (dopfkey || resend) {
    if (resend || LINK->kpick[acode - minkey])
    {  /* command string to pick up */
      getprevious(LINK);
      setmode(cursor, 0);
      setmode(kenter, ctlm);
      if (resend)   /* it's being executed */
	setcursor(LINK->cmdrow, LINK->cmdcol);
      else
	acode = nul;   /* it's ready to edit */
    } else {  /* got command string already */
      plantkeynbr(acode, LINK);
      acode = ctlm;
    }
  }

  if (acode != ctlm) {  /* not command entry */
    if (acode != ctlk) {
      if (acode != nul)   /* not cmd pickup; invalid request */
	soundalarm();
      return;
    }
    setmode(cursor, 1);
    setmode(kenter, 0);
    getloc(&r, &c);   /* going back to pickup */
    setcursor(r - 1, c);
    return;
  }
  getrow(LINK->cmdrow, ln);
  cmasked = false;
  FORLIM = maxcol();
  for (i = 0; i <= FORLIM; i++) {
    if (LINK->mask[ln[i] - nul] == nul) {
      ln[i] = blank;   /* control character mask */
      cmasked = true;
    }
  }
  if (cmasked)   /* correct image before scroll */
    putrow(LINK->cmdrow, ln);
  showcmd(ln, LINK);

  /* read the command line from the screen image */
  i = maxcol();
  while (i > LINK->cmdcol && ln[i] == blank)
    i--;
  j = LINK->cmdcol;
  k = 1;
  while (j <= i) {
    lin[k - 1] = ln[j];
    j++;
    k++;
  }
  lin[k - 1] = newline;
  lin[k] = endstr;
  if (LINK->cstate == concmd || LINK->cstate == conapp)
	/* save for re-entry */
	  scopy(lin, 1L, LINK->prevlin, 1L);

  /* if this is one-shot from f/s operation, reset needed */
  if (LINK->appstate == fsecmd) {
    LINK->appstate = fsereq;   /* corrects mode array */
    viewstate(LINK->fstate, LINK);
  }
  if (LINK->appstate == fsereq) {
    setcursor(LINK->cmdrow, 0);   /* clears f/s command text */
    erasetoright();
  }

  switch (LINK->cstate) {

  case conapp:   /* append new a/c/i text line */
    LINK->status = newtext(lin, LINK);
    break;

  case conecmd:  /* returned from e cmd prompt */
    LINK->status = ckyes(lin, LINK);
    if (LINK->status == enddata)
      LINK->status = doedit(LINK);
    break;

  case conqcmd:   /* returned from q cmd prompt */
    LINK->status = ckyes(lin, LINK);
    break;

  case concmd:  /* process new command line */
    LINK->msgdone = false;
    LINK->errcode = basic;
    if (lin[0] == keyleadin) {  /* function key evaluation */
      subkeytext(lin, LINK);
      len = length_(lin) - 1;
      if (len < maxcol() - 2) {  /* we can afford to show it */
	scopy(lin, 1L, LINK->prevlin, 1L);
	memcpy(ln, LINK->blankrow, sizeof(rowtext));
	ln[0] = cmdprompt;
	slcopy(lin, 1L, ln, 2, len);
	showcmd(ln, LINK);
      }
    }
    i = 1;
    skipbl(lin, &i);
    if (lin[i - 1] == exclam) {  /* system command request */
      checkpoint(LINK);
      scopy(lin, i + 1, fks, 1L);
      syscmd(fks, &cmdrc);
      if (LINK->appstate == fsereq || cmdrc > 0) {
	showlit("  Command completed&    ", LINK);
	if (cmdrc > 0) {
	  showlit(" with code :  &         ", LINK);
	  showdec(cmdrc, 1L, 0L, LINK);
	}
	showc(newline, LINK);
      }
      LINK->status = ok;
    } else {  /* editor command */
      LINK->cursave = LINK->curln;
      if (getlist(lin, &i, &LINK->status, LINK) == ok) {
	if (ckglob(lin, &i, &LINK->status, LINK) == ok) {
	  LINK->status = doglob(lin, &i, &LINK->cursave, &LINK->status, LINK);
	  LINK->errcode = basic;
	} else if (LINK->status != err) {
	  LINK->status = docmd(lin, &i, false, &LINK->status, LINK);
	  /* else err, do nothing */
	}
      }
    }
    break;
  }/*case*/

  /* command aftermath */
  if (LINK->status == err) {
    if (!LINK->msgdone) {
      switch (LINK->errcode) {

      case basic:
	showlit("  *** Invalid command@n&", LINK);
	break;

      case search:
	showlit("  *** Search failed@n&  ", LINK);
	break;

      case lrange:
	showlit("  *** Line range error@n", LINK);
	break;
      }
    }
    LINK->curln = min(LINK->cursave, LINK->lastln);
    if (LINK->appstate != fsereq && LINK->cstate == concmd)
      junk = doprint(LINK->curln, LINK->curln, true, LINK);
  } else if (LINK->status == enddata) {
    LINK->screenmode = false;
    if (LINK->vtitle[0] == newline && LINK->savefile[0] != endstr) {
      showlit("  Last edit filename :  ", LINK);
      showstr(LINK->savefile, LINK);
      showc(newline, LINK);
    }
  }

  if (LINK->appstate == fsereq) {
    lastcb(LINK);   /* message line */
    LINK->topln = min(LINK->topln, LINK->lastln);
    LINK->topln = max(1L, LINK->topln);
    fetch(LINK->toprow, LINK);
    fileimage(LINK);   /* corrections */
    setcursor(LINK->fsrow, LINK->fscol);   /* put it back */
    /* make sure */
    setupdate(LINK->toprow, LINK->botrow, 0, maxcol());
    return;
  }
  if (LINK->appstate != fsecmd)   /* show verification of last command */
    flushcb(LINK);

  /* if still active, prompt for the next command */
  if (LINK->status != enddata) {
    edprompt(LINK);
    return;
  }
  jfcontrol((long)nul, LINK);   /* empty journal file */
  setcursor(LINK->cmdrow, 0);
  erasetoright();
  literal("  End of editor session.");

  /* return key; command entered */
}  /*context*/


/* ****************************************************** clreport *** */
/* *                                                                 * */
/* *     CURLEW :: Full-screen editor change report processing       * */
/* *                                                                 * */
/* ******************************************************************* */


/* repchar -- replacement character in full-screen mode */
Local Void repchar(c, LINK)
uchar c;
struct LOC_editor *LINK;
{
  long i, j, k;

  fetch(LINK->fsrow, LINK);
  i = length_(LINK->fstext) - 1;   /* existing text length */
  j = LINK->fscol + LINK->xoset + 1;
  if (j > i) {  /* make the text longer */
    for (k = i; k < j; k++)
      LINK->fstext[k] = blank;
    LINK->fstext[j] = newline;
    LINK->fstext[j + 1] = endstr;
  }
  if (LINK->fstext[j - 1] != c)   /* track cursor */
  {  /* true replacement */
    if (LINK->mask[c - nul] != nul) {   /* ...not masked out */
      LINK->fstext[j - 1] = c;
      LINK->fschanged = true;
    } else
      LINK->fscmask = true;   /* fix-up required */
  }
  getloc(&LINK->fsrow, &LINK->fscol);
}  /*repchar*/


/* gettoken -- request premature return of the token */
Local Void gettoken(LINK)
struct LOC_editor *LINK;
{
  if (LINK->tokreq)
    return;
  if (LINK->autoimage)
    reqtoken();
  LINK->tokreq = true;
}  /*gettoken*/


/* deltoeol -- deltoeol line before cursor position */
Local Void deltoeol(LINK)
struct LOC_editor *LINK;
{
  long j, q;

  fetch(LINK->fsrow, LINK);
  j = length_(LINK->fstext) - 1;
  q = LINK->fscol + LINK->xoset;
  if (j <= q)  /* truncation necessary */
    return;
  LINK->fstext[q] = newline;
  LINK->fstext[q + 1] = endstr;
  LINK->fschanged = true;
}  /*deltoeol*/


/* insspaces -- correct file from spaces inserted report */
Local Void insspaces(nsp, LINK)
long nsp;
struct LOC_editor *LINK;
{
  long i, j, k, q, lim;
  chstring rem;

  fetch(LINK->fsrow, LINK);
  i = length_(LINK->fstext) - 1;   /* existing text length */
  q = LINK->fscol + LINK->xoset;
  if (i <= q)  /* insertion within text */
    return;
  k = q + 1;
  scopy(LINK->fstext, k, rem, 1L);
  j = 1;
  lim = maxstr - 2;
  while (j <= nsp && k < lim) {
    LINK->fstext[k - 1] = blank;
    j++;
    k++;
  }
  j = 1;
  while (rem[j - 1] != endstr && k <= lim) {
    LINK->fstext[k - 1] = rem[j - 1];
    j++;
    k++;
  }
  LINK->fstext[k - 2] = newline;   /* make sure */
  LINK->fstext[k - 1] = endstr;
  LINK->fschanged = true;
}  /*insspaces*/


/* delchars -- correct file from characters deleted report */
Local Void delchars(nch, LINK)
long nch;
struct LOC_editor *LINK;
{
  chstring rem;
  long i, k, q;

  fetch(LINK->fsrow, LINK);
  i = length_(LINK->fstext) - 1;   /* existing text length */
  q = LINK->fscol + LINK->xoset;
  if (i <= q)  /* characters deleted */
    return;
  k = q + 1;
  if (nch > i - k) {  /* rest of line taken out */
    LINK->fstext[k - 1] = newline;
    LINK->fstext[k] = endstr;
  } else {  /* chars at/beyond cursor */
    scopy(LINK->fstext, k + nch, rem, 1L);
    scopy(rem, 1L, LINK->fstext, k);
    if (LINK->fstate != blkmode) {   /* not from bdelchars call */
      if (i > maxcol() + LINK->xoset + 1)   /* for image correction */
	gettoken(LINK);
    }
  }
  LINK->fschanged = true;
}  /*delchars*/


/* bserases -- correct file from backspace erasure report */
Local Void bserases(nch, LINK)
long nch;
struct LOC_editor *LINK;
{
  uchar ecol;
  long j;

  ecol = LINK->fscol;
  for (j = 1; j <= nch; j++) {
    ecol--;
    LINK->fscol = ecol;
    repchar(blank, LINK);
  }
}  /*bserases*/


/* inslines -- correct file from lines inserted report */
Local Void inslines(nln, LINK)
long nln;
struct LOC_editor *LINK;
{
  stcode stat;
  chstring bs;
  long j;

  fetch(LINK->fsrow, LINK);   /* reload; may be beyond eof */
  replace(LINK);   /* dump cache */
  LINK->curln = LINK->topln + LINK->fsrow - LINK->toprow - 1;
  bs[0] = newline;
  bs[1] = endstr;
  for (j = 1; j <= nln; j++)
    stat = puttxt(bs, LINK);   /* before cursor line */
}  /*inslines*/


/* dellines -- correct file from lines deleted report */
Local Void dellines(nln, LINK)
long nln;
struct LOC_editor *LINK;
{
  long i, j, oldlast, lastdel;
  stcode stat;

  replace(LINK);   /* dump cache */
  i = LINK->topln + LINK->fsrow - LINK->toprow;
  if (i > LINK->lastln)
    return;
  oldlast = LINK->lastln;
  lastdel = min(LINK->lastln, i + nln - 1);
  stat = lndelete(i, lastdel, &stat, LINK);
  if (LINK->fsrow == LINK->toprow && LINK->topln > LINK->lastln)
	/* make it a blank line */
	  fetch(LINK->fsrow, LINK);
  j = LINK->topln + LINK->botrow - LINK->toprow;
  if (lastdel < oldlast && oldlast > j)   /* for image correction */
    gettoken(LINK);
}  /*dellines*/


/* apponeline -- correct file from line appended report */
Local Void apponeline(LINK)
struct LOC_editor *LINK;
{
  stcode stat;
  chstring bs;

  fetch(LINK->fsrow, LINK);   /* reload; may be beyond eof */
  LINK->curln = LINK->topln + LINK->fsrow - LINK->toprow;
  bs[0] = newline;
  bs[1] = endstr;
  stat = puttxt(bs, LINK);   /* after previous cursor line */
  if (LINK->fsrow >= LINK->pinrow)   /* text displaced upwards */
    LINK->topln++;
  /* track new cursor position */
  getloc(&LINK->fsrow, &LINK->fscol);
}  /*apponeline*/


/* splitoneline -- correct file from line split report */
Local Void splitoneline(LINK)
struct LOC_editor *LINK;
{
  chstring rem;
  long i, k, q, p;
  uchar ic;

  ic = LINK->fscol;
  fetch(LINK->fsrow, LINK);   /* reload; may be beyond eof */
  rem[0] = newline;
  rem[1] = endstr;
  i = length_(LINK->fstext) - 1;   /* existing text length */
  q = LINK->fscol + LINK->xoset;
  if (i > q) {  /* line split within text */
    k = q + 1;
    p = 0;
    while (p < LINK->xoset) {  /* blanks to left of margin */
      p++;
      rem[p - 1] = blank;
    }
    scopy(LINK->fstext, k, rem, p + 1);
  }
  deltoeol(LINK);   /* truncate the old line */
  replace(LINK);   /* dump cache */
  apponeline(LINK);   /* add the new line */
  fetch(LINK->fsrow, LINK);   /* reload with it */
  scopy(rem, 1L, LINK->fstext, 1L);   /* move the text in */
  LINK->fschanged = true;   /* cache changed */
  if (length_(rem) - LINK->xoset > maxcol() - ic + 2)
	/* for image correction */
	  gettoken(LINK);
}  /*splitoneline*/


/* ****************************************************** clhostrq *** */
/* *                                                                 * */
/* *      CURLEW :: Full-screen editor host request processing       * */
/* *                                                                 * */
/* ******************************************************************* */


/* blockset -- set block mode */
Local Void blockset(LINK)
struct LOC_editor *LINK;
{
  checkpoint(LINK);
  LINK->blktopln = LINK->topln;
  LINK->blkrow = LINK->fsrow;
  LINK->blkcol = LINK->fscol;
  LINK->blkoset = LINK->xoset;
  viewstate(blkmode, LINK);
}  /*blockset*/


/* blockget -- return line limits for block mode action */
Local Void blockget(start, fin, vreset, LINK)
long *start, *fin;
boolean vreset;
struct LOC_editor *LINK;
{
  long temp;

  *fin = LINK->topln + LINK->fsrow - LINK->toprow;
  if (LINK->fstate != blkmode) {
    *start = *fin;
    LINK->blktopln = LINK->topln;
    LINK->blkrow = LINK->fsrow;
    LINK->blkcol = LINK->fscol;
    return;
  }
  if (vreset)
    vbmreset(LINK);
  *start = LINK->blktopln + LINK->blkrow - LINK->toprow;
  if (*start <= *fin)
    return;
  temp = *start;
  *start = *fin;
  *fin = temp;
}  /*blockget*/


/* blockreset -- reset state after block mode action */
Local Void blockreset(reloc, LINK)
boolean reloc;
struct LOC_editor *LINK;
{
  if (LINK->fstate != blkmode)
    return;
  viewstate(normal, LINK);
  vbmreset(LINK);
  if (reloc) {
    LINK->topln = min(LINK->blktopln, LINK->lastln);
    LINK->topln = max(LINK->topln, 1L);
    fetch(LINK->toprow, LINK);
    if (LINK->xoset != LINK->blkoset) {
      LINK->xoset = LINK->blkoset;
      ruler(LINK->toprow - 1, LINK);
    }
    fileimage(LINK);
    LINK->fsrow = LINK->blkrow;
    LINK->fscol = LINK->blkcol;
  }
  setupdate(LINK->toprow, LINK->botrow, 0, maxcol());
  LINK->mousetrap = safe;
}  /*blockreset*/


/* verify -- change screen image to reflect change to file */
Local Void verify(LINK)
struct LOC_editor *LINK;
{
  char ir;
  uchar ic;

  LINK->fschanged = true;   /* cache changed */
  replace(LINK);   /* buffer updated */
  if (LINK->fstate == blkmode)
    return;
  ir = LINK->fsrow;
  ic = LINK->fscol;
  filetext(LINK->fsrow, LINK);   /* correct the screen image */
  LINK->fsrow = ir;
  LINK->fscol = ic;   /* but don't lose cursor posn */
}  /*verify*/


/* align -- align first non-blank with cursor */
Local Void align(LINK)
struct LOC_editor *LINK;
{
  chstring rem;
  long start, fin, cl, j, k, lim;

  blockget(&start, &fin, true, LINK);
  for (cl = start; cl <= fin; cl++) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
    fetch(LINK->fsrow, LINK);
    j = 1;
    while (LINK->fstext[j - 1] == blank)
      j++;
    k = LINK->fscol + LINK->xoset + 1;
    if (LINK->fstext[j - 1] != newline && k != j) {
      scopy(LINK->fstext, j, rem, 1L);
      for (j = 0; j <= k - 2; j++)
	LINK->fstext[j] = blank;
      lim = maxstr - 2;
      j = 1;
      while (rem[j - 1] != endstr && k <= lim) {
	LINK->fstext[k - 1] = rem[j - 1];
	j++;
	k++;
      }
      LINK->fstext[k - 1] = endstr;   /* correct the screen image */
      verify(LINK);
    }
  }
  blockreset(true, LINK);
}  /*align */


/* deltosol -- delete from cursor to start of line */
Local Void deltosol(LINK)
struct LOC_editor *LINK;
{
  long start, fin, cl, j, k;
  chstring rem;

  blockget(&start, &fin, true, LINK);
  for (cl = start; cl <= fin; cl++) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
    fetch(LINK->fsrow, LINK);
    j = length_(LINK->fstext) - 1;
    if (j > 0) {
      k = LINK->fscol + LINK->xoset + 1;
      if (j > k) {   /* correct the screen image */
	scopy(LINK->fstext, k + 1, rem, 1L);
	scopy(rem, 1L, LINK->fstext, 1L);
      } else {
	LINK->fstext[0] = newline;
	LINK->fstext[1] = endstr;
      }
      verify(LINK);
    }
  }
  blockreset(true, LINK);
}  /*deltosol*/


/* bdeltoeol -- block delete to end of line */
Local Void bdeltoeol(LINK)
struct LOC_editor *LINK;
{
  long start, fin, cl;

  blockget(&start, &fin, true, LINK);
  for (cl = start; cl <= fin; cl++) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
	/* use the local report code */
    deltoeol(LINK);
  }
  blockreset(true, LINK);
}  /*bdeltoeol*/


/* bdellines -- block delete lines */
Local Void bdellines(LINK)
struct LOC_editor *LINK;
{
  long start, fin, botln;
  stcode stat;

  blockget(&start, &fin, true, LINK);
  if (start <= LINK->lastln) {
    fin = min(fin, LINK->lastln);
    stat = lndelete(start, fin, &stat, LINK);
  }
  if (LINK->topln + LINK->fsrow < LINK->blktopln + LINK->blkrow)
  {  /* leave at next line after deleted group */
    LINK->blkrow = LINK->fsrow;
    LINK->blkcol = LINK->fscol;
    LINK->blktopln = LINK->topln;
  }
  if (LINK->blktopln == LINK->topln) {  /* possible optimisation here */
    botln = LINK->topln + LINK->botrow - LINK->toprow;
    if (start < botln && fin < botln)
      scrollup((int)(LINK->toprow + start - LINK->topln), LINK->botrow,
	       (int)(fin - start + 1));
  }
  blockreset(true, LINK);
}  /*bdellines*/


/* binsspaces -- block insert spaces */
Local Void binsspaces(LINK)
struct LOC_editor *LINK;
{
  long start, fin, cl, nsp;

  blockget(&start, &fin, true, LINK);
  nsp = LINK->blkcol + LINK->blkoset - LINK->fscol - LINK->xoset;
  if (nsp >= 0)
    nsp++;
  else {  /* turn it round */
    nsp = 1 - nsp;
    LINK->fscol = LINK->blkcol;
    LINK->xoset = LINK->blkoset;
  }
  for (cl = start; cl <= fin; cl++) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
	/* use the local report code */
    insspaces(nsp, LINK);
  }
  blockreset(true, LINK);
}  /*binsspaces*/


/* bdelchars -- block delete characters */
Local Void bdelchars(LINK)
struct LOC_editor *LINK;
{
  long start, fin, cl, nch;

  blockget(&start, &fin, true, LINK);
  nch = LINK->blkcol + LINK->blkoset - LINK->fscol - LINK->xoset;
  if (nch >= 0)
    nch++;
  else {  /* turn it round */
    nch = 1 - nch;
    LINK->fscol = LINK->blkcol;
    LINK->xoset = LINK->blkoset;
  }
  for (cl = start; cl <= fin; cl++) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
	/* use the local report code */
    delchars(nch, LINK);
  }
  blockreset(true, LINK);
}  /*bdelchars*/


/* delblanks -- on one line, delete from cursor to next non-blank */
Local Void delblanks(LINK)
struct LOC_editor *LINK;
{
  long j, k, kmark;
  chstring rem;

  fetch(LINK->fsrow, LINK);
  j = length_(LINK->fstext) - 1;
  k = LINK->fscol + LINK->xoset + 1;
  if (j < k)
    return;
  if (LINK->fstext[k - 1] != blank)
    return;
  kmark = k;
  while (LINK->fstext[k - 1] == blank)
    k++;
  scopy(LINK->fstext, k, rem, 1L);
  scopy(rem, 1L, LINK->fstext, kmark);   /* correct the screen image */
  verify(LINK);
}  /*delblanks*/


/* bdelblanks -- delete from cursor to next non-blank */
Local Void bdelblanks(LINK)
struct LOC_editor *LINK;
{
  long start, fin, cl;

  blockget(&start, &fin, true, LINK);
  for (cl = start; cl <= fin; cl++) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
    delblanks(LINK);
  }
  blockreset(true, LINK);
}  /*bdelblanks*/


/* deloneword -- delete word at cursor */
Local Void deloneword(LINK)
struct LOC_editor *LINK;
{
  long j, k;

  fetch(LINK->fsrow, LINK);
  j = length_(LINK->fstext) - 1;
  k = LINK->fscol + LINK->xoset + 1;
  if (j < k)
    return;
  if (LINK->fstext[k - 1] == blank)
    return;
  while (LINK->fstext[k - 1] != blank && LINK->fstext[k - 1] != newline)
    k++;
  k--;
  while (k > 1 && LINK->fstext[k - 1] != blank) {
    LINK->fstext[k - 1] = blank;
    k--;
  }
  if (LINK->fstext[k - 1] != blank) {
    LINK->fstext[k - 1] = blank;
    k--;
  }
  k = max(k - LINK->xoset, 0L);
  LINK->fscol = k;   /* at start of former word */
  /* close up to next word */
  delblanks(LINK);
}  /*deloneword*/


/* delwords -- delete word or block delete words */
Local Void delwords(LINK)
struct LOC_editor *LINK;
{
  long start, fin, lastdel, i, j, k, l, toset, q;
  stcode stat;
  uchar tcol, svcol;

  if (LINK->fstate != blkmode) {
    deloneword(LINK);
    return;
  }
  blockget(&start, &fin, true, LINK);
  if (start != fin) {  /* not on same line */
    if (LINK->topln + LINK->fsrow > LINK->blktopln + LINK->blkrow) {
      tcol = LINK->fscol;
      LINK->fscol = LINK->blkcol;
      toset = LINK->xoset;
      LINK->xoset = LINK->blkoset;
    } else {
      tcol = LINK->blkcol;
      toset = LINK->blkoset;
      LINK->blkrow = LINK->fsrow;
      LINK->blktopln = LINK->topln;
    }
    LINK->topln = start - LINK->fsrow + LINK->toprow;
    svcol = LINK->fscol;
    repchar(star, LINK);   /* cursor over a word */
    LINK->fscol = svcol;
    deloneword(LINK);   /* first word in block */
    deltoeol(LINK);   /* remainder of first line */
    LINK->blkcol = LINK->fscol;
    replace(LINK);   /* dump cache */
    if (start < LINK->lastln && fin - start > 1)
    {  /* delete intermediate lines */
      lastdel = min(fin - 1, LINK->lastln);
      stat = lndelete(start + 1, lastdel, &stat, LINK);
    }
    LINK->topln++;
    LINK->fscol = tcol;
    LINK->xoset = toset;
    repchar(star, LINK);
    LINK->fscol = tcol;
    q = LINK->fscol + LINK->xoset + 1;
    for (k = 0; k < q; k++)   /* make sol all one word */
      LINK->fstext[k] = star;
    LINK->fscol = tcol;   /* delete the giant word */
    deloneword(LINK);
  } else {  /* on the same line */
    if (LINK->fscol + LINK->xoset <= LINK->blkcol + LINK->blkoset) {
      i = LINK->fscol + LINK->xoset;
      j = LINK->blkcol + LINK->blkoset - i + 1;
    } else {
      i = LINK->blkcol + LINK->blkoset;
      j = LINK->fscol + LINK->xoset - i + 1;
    }
    fetch(LINK->fsrow, LINK);
    l = length_(LINK->fstext);
    for (k = 0; k < j; k++)
      LINK->fstext[k + i] = star;
    if (l <= i + j) {
      LINK->fstext[i + j] = newline;
      LINK->fstext[i + j + 1] = endstr;
    }
    deloneword(LINK);   /* now delete it */
    LINK->blkcol = LINK->fscol;
  }
  blockreset(true, LINK);
}  /*delwords*/


/* splitline -- split line before cursor, don't move cursor */
Local Void splitline(LINK)
struct LOC_editor *LINK;
{
  chstring rem;
  long start, fin, cl, i, k, q;
  stcode stat;

  blockget(&start, &fin, true, LINK);
  for (cl = fin; cl >= start; cl--) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
    fetch(LINK->fsrow, LINK);   /* reload; may be beyond eof */
    rem[0] = newline;
    rem[1] = endstr;
    i = length_(LINK->fstext) - 1;   /* existing text length */
    q = LINK->fscol + LINK->xoset;
    if (i > q) {  /* line split within text */
      k = q + 1;
      scopy(LINK->fstext, k, rem, 1L);
      LINK->fstext[k - 1] = newline;
      LINK->fstext[k] = endstr;
      LINK->fschanged = true;
    }
    LINK->curln = LINK->topln + LINK->fsrow - LINK->toprow;
    stat = puttxt(rem, LINK);   /* after previous cursor line */

    if (LINK->fstate != blkmode) {
      erasetoright();   /* first image correction */
      if (LINK->fsrow < LINK->botrow) {  /* second image correction */
	if (LINK->fsrow < LINK->botrow - 1)
	  scrolldown(LINK->fsrow + 1, LINK->botrow, 1);
	filetext(LINK->fsrow + 1, LINK);
      }
    }
  }
  blockreset(true, LINK);
}  /*splitline*/


/* combinelines -- combine cursor line with next, don't move cursor */
Local Void combinelines(LINK)
struct LOC_editor *LINK;
{
  chstring rem;
  long start, fin, cl, i, j, k, lim, q;
  stcode stat;

  blockget(&start, &fin, true, LINK);
  fin = start + (fin - start) / 2;
  for (cl = start; cl <= fin; cl++) {
    LINK->topln = cl - LINK->fsrow + LINK->toprow;
    fetch(LINK->fsrow + 1, LINK);   /* next row; will be deleted */
    scopy(LINK->fstext, 1L, rem, 1L);
    i = LINK->topln + LINK->fsrow - LINK->toprow + 1;
    if (i <= LINK->lastln) {
      stat = lndelete(i, i, &stat, LINK);
      if (LINK->fstate != blkmode) {
	if (LINK->fsrow < LINK->botrow) {  /* first correction to image */
	  if (LINK->fsrow < LINK->botrow - 1)
	    scrollup(LINK->fsrow + 1, LINK->botrow, 1);
	  filetext(LINK->botrow, LINK);
	}
      }
    }
    fetch(LINK->fsrow, LINK);   /* cursor row; append text */
    k = length_(LINK->fstext) - 1;
    q = LINK->fscol + LINK->xoset;
    if (k < q) {
      for (j = k; j < q; j++)
	LINK->fstext[j] = blank;
      k = q;
    }
    lim = maxstr - 2;
    j = 1;
    k++;
    while (rem[j - 1] != endstr && k <= lim) {
      LINK->fstext[k - 1] = rem[j - 1];
      j++;
      k++;
    }
    LINK->fstext[k - 1] = endstr;   /* final correction to image */
    verify(LINK);
  }
  blockreset(true, LINK);
}  /*combinelines*/


/* paralines -- alternative processing for block combine */
Local Void paralines(LINK)
struct LOC_editor *LINK;
{
  long start, fin, pwid, i, savwidth, savpara, savoset;
  boolean savfmt;
  chstring fclin;
  stcode stat;

  /* save global profile settings */
  savfmt = LINK->gblfmt;
  savwidth = LINK->gblwidth;
  savpara = LINK->gblpara;
  savoset = LINK->gbloset;

  blockget(&start, &fin, true, LINK);
  pwid = LINK->blkcol + LINK->blkoset - LINK->fscol - LINK->xoset;
  if (pwid >= 0) {  /* "fo b" if width > 0 */
    pwid++;
    LINK->gblfmt = (pwid > 1);
  } else {  /* turn it round, "fo l" */
    pwid = 1 - pwid;
    LINK->gblfmt = false;
    LINK->fscol = LINK->blkcol;
    LINK->xoset = LINK->blkoset;
  }
  if (pwid >= 24) {
    LINK->gblwidth = pwid;
    LINK->gbloset = LINK->fscol + LINK->xoset;
  }

  if (start <= LINK->lastln) {
    fin = min(fin, LINK->lastln);
    fclin[0] = newline;
    fclin[1] = endstr;
    i = 1;
    stat = dofmt(start, fin, fclin, i, LINK);
  }

  /* restore global profile settings*/
  LINK->gblfmt = savfmt;
  LINK->gblwidth = savwidth;
  LINK->gblpara = savpara;
  LINK->gbloset = savoset;

  blockreset(false, LINK);
  fileimage(LINK);
}  /*paralines*/


/* tabinsert -- insert a new line leaving cursor at current indent */
Local Void tabinsert(LINK)
struct LOC_editor *LINK;
{
  chstring bs;
  stcode stat;
  rowtext ln;
  char trow;
  uchar tcol;
  boolean found;

  /* first insert the line */
  fetch(LINK->fsrow, LINK);   /* reload; may be beyond eof */
  replace(LINK);   /* dump cache */
  LINK->curln = LINK->topln + LINK->fsrow - LINK->toprow;
  bs[0] = newline;
  bs[1] = endstr;
  stat = puttxt(bs, LINK);   /* after cursor line */

  /* next update the screen image */
  if (LINK->fsrow >= LINK->pinrow) {
    LINK->topln++;   /* text displaced upwards */
    scrollup(LINK->toprow, LINK->fsrow, 1);
  } else {  /* text displaced downwards */
    LINK->fsrow++;
    scrolldown(LINK->fsrow, LINK->botrow, 1);
  }

  /* now find out where to put the cursor */
  LINK->fscol = 0;
  trow = LINK->fsrow;
  found = false;
  while (!found) {
    trow--;
    getrow(trow, ln);
    tcol = 0;
    while (tcol < maxcol() && ln[tcol] == blank)
      tcol++;
    if (ln[tcol] != blank) {   /* will stop on ruler if not before */
      LINK->fscol = tcol;
      found = true;
    }
  }
}  /*tabinsert*/


/* ****************************************************** clclone **** */
/* *                                                                 * */
/* *         CURLEW :: Full-screen editor clone processing           * */
/* *                                                                 * */
/* ******************************************************************* */


/* cmessage -- output bottom line messages for clone actions */
Local Void cmessage(txt, n, w, LINK)
Char *txt;
long n, w;
struct LOC_editor *LINK;
{
  setcursor(maxrow(), 0);
  erasetoright();
  textchar(lbrack);
  textchar(blank);
  literal(txt);
  outdec(n);
  if (w > 0) {
    literal(" line&                  ");
    if (n > 1)
      textchar(lets);
    literal(" of width &             ");
    outdec(w);
  }
  textchar(blank);
  textchar(rbrack);
}  /*cmessage*/


/* openclone -- open clone file for reading or writing */
Local short openclone(openmode, LINK)
long openmode;
struct LOC_editor *LINK;
{
  chstring cname;
  short fd;

  getinf((long)clclone, cname, (long)maxstr);
  if (openmode == ioread) {
    fd = openf(cname, (long)ioread);
    if (fd == ioerror_)
      fd = create(cname, (long)ioread);
  } else
    fd = create(cname, (long)iowrite);
  LINK->cloneexists = true;
  return fd;
}  /*openclone*/


/* clonetext -- block save of lines or a rectangle of characters */
Local Void clonetext(chsave, LINK)
boolean chsave;
struct LOC_editor *LINK;
{
  long start, fin, j, k, si, w;
  short fd;
  chstring rem;
  stcode stat;

  LINK->charclone = chsave;   /* remember clone type */
  if (LINK->charclone) {
    if (LINK->fscol + LINK->xoset < LINK->blkcol + LINK->blkoset) {
      si = LINK->fscol + LINK->xoset + 1;
      w = LINK->blkcol + LINK->blkoset - LINK->fscol - LINK->xoset + 1;
    } else {
      si = LINK->blkcol + LINK->blkoset + 1;
      w = LINK->fscol + LINK->xoset - LINK->blkcol - LINK->blkoset + 1;
    }
    w = min(w, maxstr - 3L);   /* can't quite save full width */
  }

  blockget(&start, &fin, false, LINK);
  fetch(LINK->fsrow, LINK);
  LINK->fileclone = (start != fin);
  if (LINK->fileclone)
    fd = openclone((long)iowrite, LINK);
  j = start;
  stat = ok;
  do {
    if (j <= LINK->lastln) {
      gettxt(j, LINK->sclone, LINK);
      if (LINK->charclone) {
	for (k = length_(LINK->sclone) - 1; k <= si + w - 2; k++)
	  LINK->sclone[k] = blank;
	LINK->sclone[si + w - 1] = endstr;
	scopy(LINK->sclone, si, rem, 1L);
	scopy(rem, 1L, LINK->sclone, 1L);
	LINK->sclone[w] = dollar;
	LINK->sclone[w + 1] = newline;
	LINK->sclone[w + 2] = endstr;
      }
      if (LINK->fileclone)
	putstr(LINK->sclone, fd);
      j++;
    } else
      stat = err;
  } while (stat == ok && j <= fin);
  if (LINK->fileclone)
    closef(fd);
  if (LINK->charclone)
    cmessage("Characters saved :  &   ", j - start, w, LINK);
  else
    cmessage("Text lines saved :  &   ", j - start, -1L, LINK);

  /* don't leave town till the mousetrap is armed */
  LINK->mousetrap = armed;
  putmode(LINK->bblkmode);
  setupdate(LINK->fsrow, LINK->fsrow, LINK->fscol, LINK->fscol);
}  /*clonetext*/


/* insclone -- insert a copy of the saved text at cursor position */
Local Void insclone(LINK)
struct LOC_editor *LINK;
{
  long j, k, svtopln, tl, p, q;
  uchar svcol;
  boolean t;
  short fd;
  stcode stat;

  checkpoint(LINK);
  if (LINK->fstate == blkmode)   /* reset, but stay here */
    blockreset(false, LINK);

  if (LINK->fileclone)
    fd = openclone((long)ioread, LINK);
  if (LINK->charclone) {   /* character rectangle */
    svtopln = LINK->topln;
    svcol = LINK->fscol;   /* where we are */
    fetch(LINK->fsrow, LINK);
    LINK->curln = LINK->topln + LINK->fsrow - LINK->toprow - 1;
    j = 0;
    if (LINK->fileclone)   /* internal; already in sclone */
      t = getline(LINK->sclone, fd, (long)maxstr);
    else
      t = true;
    while (t) {
      tl = max(length_(LINK->sclone) - 2, 1L);   /* at least one */
      tl = min(tl, maxstr - svcol - LINK->xoset - 2);   /* not beyond m/str */
      insspaces(tl, LINK);   /* use local report code */
      q = svcol + LINK->xoset + 1;
      p = length_(LINK->fstext);
      if (p < q) {  /* leading spaces needed */
	while (p < q) {
	  LINK->fstext[p - 1] = blank;
	  p++;
	}
	LINK->fstext[p - 1] = newline;
	LINK->fstext[p] = endstr;
      }
      for (k = 0; k < tl; k++) {
	if (LINK->fstext[q - 1] == newline) {  /* make the line longer */
	  LINK->fstext[q] = newline;
	  LINK->fstext[q + 1] = endstr;
	}
	LINK->fstext[q - 1] = LINK->sclone[k];   /* put the text in */
	q++;
      }
      LINK->fschanged = true;
      replace(LINK);   /* dump cache */
      j++;
      LINK->fscol = svcol;
      LINK->topln++;
      if (LINK->fileclone)
	t = getline(LINK->sclone, fd, (long)maxstr);
      else
	t = false;   /* internal; once only */
    }
    LINK->topln = svtopln;   /* where we came from */
    cmessage("Characters inserted :  &", j, tl, LINK);
  } else {  /* line range */
    fetch(LINK->fsrow, LINK);
    LINK->curln = LINK->topln + LINK->fsrow - LINK->toprow - 1;
    j = 0;
    stat = ok;
    do {
      if (LINK->fileclone)   /* internal; already in sclone */
	t = getline(LINK->sclone, fd, (long)maxstr);
      else
	t = true;
      if (t) {
	stat = puttxt(LINK->sclone, LINK);
	if (stat == ok)
	  j++;
      }
      if (!LINK->fileclone)
	t = false;
    } while (stat == ok && t);
    k = (LINK->botrow - LINK->toprow) / 2;
    if (j < LINK->botrow - LINK->fsrow && j <= k)
	  /* assist image correction */
	    scrolldown(LINK->fsrow, LINK->botrow, (int)j);
    cmessage("Text lines inserted :  &", j, -1L, LINK);
  }
  if (LINK->fileclone)   /* final image correction */
    closef(fd);
  fileimage(LINK);
}  /*insclone*/


/* ****************************************************** clfsmisc *** */
/* *                                                                 * */
/* *       CURLEW :: Full-screen editor miscellaneous requests       * */
/* *                                                                 * */
/* ******************************************************************* */


/* lmtmsg -- show file limit reached message */
Local Void lmtmsg(firstln, LINK)
boolean firstln;
struct LOC_editor *LINK;
{
  setcursor(maxrow(), 0);
  erasetoright();
  textchar(lbrack);
  literal(" *** Image top row is & ");
  if (firstln)
    literal("first line in buffer &  ");
  else
    literal("last line in buffer &   ");
  textchar(rbrack);
}  /*lmtmsg */


/* goup -- upwards cursor movement from u/l violation */
Local Void goup(LINK)
struct LOC_editor *LINK;
{
  long oldtopln;

  if (LINK->fsrow > LINK->toprow) {
    LINK->fsrow--;
    return;
  }
  if (LINK->cscroll <= 0) {
    LINK->fsrow = LINK->botrow;
    return;
  }
  oldtopln = LINK->topln;
  LINK->topln -= LINK->cscroll;
  LINK->topln = max(LINK->topln, 1L);
  if (LINK->topln == oldtopln) {
    lmtmsg(true, LINK);
    LINK->fsrow = LINK->botrow;
    return;
  }
  scrolldown(LINK->toprow, LINK->botrow, (int)(oldtopln - LINK->topln));
  fileimage(LINK);
  LINK->fsrow += oldtopln - LINK->topln - 1;
}  /*goup*/


/* godown -- downwards cursor movement from u/l violation */
Local Void godown(LINK)
struct LOC_editor *LINK;
{
  long oldtopln;

  if (LINK->fsrow < LINK->botrow) {
    LINK->fsrow++;
    return;
  }
  if (LINK->cscroll <= 0) {
    LINK->fsrow = LINK->toprow;
    return;
  }
  oldtopln = LINK->topln;
  LINK->topln += LINK->cscroll;
  LINK->topln = min(LINK->topln, LINK->lastln);
  LINK->topln = max(LINK->topln, 1L);
  if (LINK->topln == oldtopln) {
    lmtmsg(false, LINK);
    LINK->fsrow = LINK->toprow;
    return;
  }
  scrollup(LINK->toprow, LINK->botrow, (int)(LINK->topln - oldtopln));
  fileimage(LINK);
  LINK->fsrow += oldtopln - LINK->topln + 1;
}  /*godown*/


/* goleft -- left cursor movement from u/l violation */
Local Void goleft(LINK)
struct LOC_editor *LINK;
{
  if (LINK->fscol > 0)
    LINK->fscol--;
  else {
    goup(LINK);
    LINK->fscol = maxcol();
  }
}  /*goleft*/


/* goright -- right cursor movement from u/l violation */
Local Void goright(LINK)
struct LOC_editor *LINK;
{
  if (LINK->fscol < maxcol())
    LINK->fscol++;
  else {
    godown(LINK);
    LINK->fscol = 0;
  }
}  /*goright*/


/* wordright -- cursor movement to start of next word */
Local Void wordright(LINK)
struct LOC_editor *LINK;
{
  char irow, trow;
  uchar icol;
  rowtext ln;
  boolean found, looped;
  long savscroll;

  irow = LINK->fsrow;
  icol = LINK->fscol;
  trow = LINK->fsrow;
  savscroll = LINK->cscroll;
  if (LINK->cscroll == 1)   /* avoids looping problem */
    LINK->cscroll = 2;
  getrow(trow, ln);
  looped = false;
  if (ln[LINK->fscol] != blank) {
    do {   /* find blank */
      goright(LINK);
      if (LINK->fsrow != trow) {
	trow = LINK->fsrow;
	getrow(trow, ln);
      }
      found = (ln[LINK->fscol] == blank);
      looped = (LINK->fsrow == irow && LINK->fscol == icol);
    } while (!(found || looped));
  }
  if (!looped) {
    do {   /* find non-blank */
      goright(LINK);
      if (LINK->fsrow != trow) {
	trow = LINK->fsrow;
	getrow(trow, ln);
      }
      found = (ln[LINK->fscol] != blank);
      looped = (LINK->fsrow == irow && LINK->fscol == icol);
    } while (!(found || looped));
  }
  LINK->cscroll = savscroll;
}  /*wordright*/


/* wordleft -- cursor movement to start of previous word */
Local Void wordleft(LINK)
struct LOC_editor *LINK;
{
  char irow, trow;
  uchar icol;
  rowtext ln;
  boolean found, looped;
  long savscroll;

  irow = LINK->fsrow;
  icol = LINK->fscol;
  trow = LINK->fsrow;
  savscroll = LINK->cscroll;
  if (LINK->cscroll == 1)   /* avoids looping problem */
    LINK->cscroll = 2;
  getrow(trow, ln);
  looped = false;
  if (ln[LINK->fscol] != blank) {
    do {   /* find blank */
      goleft(LINK);
      if (LINK->fsrow != trow) {
	trow = LINK->fsrow;
	getrow(trow, ln);
      }
      found = (ln[LINK->fscol] == blank);
      looped = (LINK->fsrow == irow && LINK->fscol == icol);
    } while (!(found || looped));
  }
  if (!looped) {
    do {   /* find non-blank */
      goleft(LINK);
      if (LINK->fsrow != trow) {
	trow = LINK->fsrow;
	getrow(trow, ln);
      }
      found = (ln[LINK->fscol] != blank);
      looped = (LINK->fsrow == irow && LINK->fscol == icol);
    } while (!(found || looped));
  }
  if (!looped) {
    do {   /* find blank */
      goleft(LINK);
      if (LINK->fsrow != trow) {
	trow = LINK->fsrow;
	getrow(trow, ln);
      }
      found = (ln[LINK->fscol] == blank);
      looped = (LINK->fsrow == irow && LINK->fscol == icol);
    } while (!(found || looped));
  }
  if (found) {
    LINK->cscroll = 0;
    goright(LINK);   /* forward to start of word */
  }
  LINK->cscroll = savscroll;
}  /*wordleft*/


/* textlocate -- host assisted text location in basic block operation */
Local Void textlocate(start, LINK)
boolean start;
struct LOC_editor *LINK;
{
  rowtext ln;
  uchar tcol;

  getrow(LINK->fsrow, ln);
  if (start) {
    tcol = 0;
    while (tcol < maxcol() && ln[tcol] == blank)
      tcol++;
    if (ln[tcol] == blank)
      tcol = 0;
  } else {
    tcol = maxcol();
    while (tcol > 0 && ln[tcol] == blank)
      tcol--;
    if ((ln[tcol] != blank) & (tcol < maxcol()))
      tcol++;
  }
  LINK->fscol = tcol;
}  /*textlocate*/


/* wordwrap -- transfer word at end of line to new next line */
Local Void wordwrap(LINK)
struct LOC_editor *LINK;
{
  long i, j, k, p, q;
  uchar ncol;
  chstring rem;
  stcode stat;

  if (LINK->fstate != normal) {
    goright(LINK);
    return;
  }
  fetch(LINK->fsrow, LINK);
  rem[0] = newline;
  rem[1] = endstr;
  ncol = 0;
  if (LINK->inswrap) {
    j = length_(LINK->fstext) - 1;
    q = maxcol() + LINK->xoset;
    if (j > q) {
      j = q + 1;
      k = j;
      i = j - maxcol() / 2;
      while (j > i && LINK->fstext[j - 1] != blank)
	j--;
      if (LINK->fstext[j - 1] == blank)
	j++;
      else
	j = k + 1;
      p = 0;
      while (p < LINK->xoset) {  /* blanks to left of margin */
	p++;
	rem[p - 1] = blank;
      }
      scopy(LINK->fstext, j, rem, p + 1);
      LINK->fstext[j - 1] = newline;
      LINK->fstext[j] = endstr;
      verify(LINK);   /* first image correction */
      j += -LINK->xoset - 2;
      ncol = maxcol() - j;
    }
  }
  LINK->curln = LINK->topln + LINK->fsrow - LINK->toprow;
  stat = puttxt(rem, LINK);   /* after previous cursor line */
  if (LINK->fsrow >= LINK->pinrow) {
    LINK->topln++;
    scrollup(LINK->toprow, LINK->fsrow, 1);
  } else {
    LINK->fsrow++;
    scrolldown(LINK->fsrow, LINK->botrow, 1);
  }
  filetext(LINK->fsrow, LINK);   /* second image correction */
  LINK->fscol = ncol;
}  /*wordwrap*/


/* delkey -- handle DEL keystroke at left screen margin */
Local Void delkey(LINK)
struct LOC_editor *LINK;
{
  goleft(LINK);
  textlocate(false, LINK);   /* after end of text */
  setcursor(LINK->fsrow, LINK->fscol);
}  /*delkey*/


/* ****************************************************** clctrl ***** */
/* *                                                                 * */
/* *      CURLEW :: T-TOKEN from CTRL key or data u/l violation      * */
/* *                                                                 * */
/* ******************************************************************* */


/* control -- token return (full-screen) from control key */
Local Void control(rc, LINK)
uchar rc;
struct LOC_editor *LINK;
{
  switch (rc) {

  case nul:   /* block mode mousetrap */
    if (LINK->fscol == maxcol())   /* power-typing support */
      wordwrap(LINK);
    else
      goright(LINK);
    break;

  case ctla:   /* align text with cursor */
    align(LINK);
    break;

  case ctlb:   /* reset block mode */
    if (LINK->fstate != blkmode)   /* set block mode */
      blockset(LINK);
    else
      blockreset(true, LINK);
    break;

  case ctlc:   /* text end (basic block) */
    textlocate(false, LINK);
    break;

  case ctld:   /* delete blanks from cursor */
    bdelblanks(LINK);
    break;

  case ctle:   /* block delete characters */
    bdelchars(LINK);
    break;

  case ctlf:   /* cursor left to previous word */
    if (LINK->htkeys)   /* kprevtab u/l violation */
      goleft(LINK);
    else
      wordleft(LINK);
    break;

  case ctlg:  /* home (basic block) */
    LINK->fsrow = LINK->toprow;
    LINK->fscol = 0;
    break;

  case ctlh:   /* kcsrleft u/l violation */
    goleft(LINK);
    break;

  case ctli:
    if (LINK->htkeys)   /* knexttab u/l violation */
      goright(LINK);
    else {
      if (LINK->fstate == blkmode)   /* not going to play */
	soundalarm();
      else   /* insertion at current indent */
	tabinsert(LINK);
    }
    break;

  case ctlj:   /* kcsrdown u/l violation */
    godown(LINK);
    break;

  case ctlk:   /* kcsrup u/l violation */
    goup(LINK);
    break;

  case ctll:   /* kcsrright u/l violation */
    goright(LINK);
    break;

  case ctlm:  /* RETURN (basic block) */
    LINK->fscol = 0;
    if (LINK->fsrow >= LINK->botrow)
      LINK->fsrow = LINK->toprow;
    else
      LINK->fsrow++;
    break;

  case ctln:   /* insert saved text */
    insclone(LINK);
    break;

  case ctlo:   /* split line before cursor */
    splitline(LINK);
    break;

  case ctlp:   /* combine line with next */
    if (LINK->fstate == blkmode && LINK->blkpara)   /* paragraph line group */
      paralines(LINK);
    else
      combinelines(LINK);
    break;


  case ctlq:   /* block save lines */
    clonetext(false, LINK);
    break;

  case ctlr:   /* delete word */
    delwords(LINK);
    break;

  case ctls:   /* block insert spaces */
    binsspaces(LINK);
    break;

  case ctlt:   /* delete to start of line */
    deltosol(LINK);
    break;

  case ctlu:   /* block delete lines */
    bdellines(LINK);
    break;

  case ctlv:   /* cursor right to next word */
    wordright(LINK);
    break;

  case ctlw:   /* block save characters */
    clonetext(true, LINK);
    break;

  case ctlx:   /* text start (basic block) */
    textlocate(true, LINK);
    break;

  case ctly:   /* block delete to end of line */
    bdeltoeol(LINK);
    break;

  case ctlz:   /* line start (basic block) */
    LINK->fscol = 0;
    break;

  }/* case*/
  setcursor(LINK->fsrow, LINK->fscol);
}  /*control*/


/* ****************************************************** clfsreq **** */
/* *                                                                 * */
/* *         CURLEW :: Full-screen editor T-TOKEN dispatch           * */
/* *                                                                 * */
/* ******************************************************************* */


/* fsrequest -- full-screen editor request */
Local Void fsrequest(rc, LINK)
uchar rc;
struct LOC_editor *LINK;
{
  long acode;

  replace(LINK);   /* safety */
  getloc(&LINK->fsrow, &LINK->fscol);
  if (LINK->fstate == normal)   /* logs "icharmode" setting */
    getmode(LINK->nfsomode);
  if (LINK->tokreq || LINK->fscmask) {  /* correct the file image */
    fileimage(LINK);
    setcursor(LINK->fsrow, LINK->fscol);
    LINK->tokreq = false;
    LINK->fscmask = false;
  }

  if (LINK->mousetrap == armed)   /* block mode WILL be reset! */
    LINK->mousetrap = sprung;

  if (rc > del) {  /* reqtoken or session restart */
    if (rc > 128 && LINK->rrend != defrend) {  /* session restart fix */
      setmode(knextfld, 50);   /* next field key ... */
      setmode(kprevfld, 53);   /* prev field key ... */
      viewstate(LINK->fstate, LINK);   /* now correct mode array */
      ruler(LINK->toprow - 1, LINK);
      ruler(LINK->botrow + 1, LINK);
      setcursor(LINK->fsrow, LINK->fscol);
    }
  } else if (rc <= ctlz)
    control(rc, LINK);
  else if (rc == del)
    delkey(LINK);
  else if (fnkey((long)rc, &acode, LINK) == 0)
    soundalarm();
  else {
    if (LINK->mousetrap == sprung)   /* no point in leaving it set */
      blockreset(false, LINK);
    edprompt(LINK);
    plantkeynbr(acode, LINK);
    LINK->curln = min(LINK->lastln, LINK->topln + LINK->fsrow - LINK->toprow);
    context(ctlm, LINK);
  }

  if (LINK->appstate != fsereq)
    return;
  getloc(&LINK->fsrow, &LINK->fscol);   /* track cursor */
  if (LINK->mousetrap == sprung) {
    blockreset(false, LINK);
    setcursor(LINK->fsrow, LINK->fscol);
  }

  /* control key or u/l violation */
  /* del key at left margin */
  /* failed function key test */
  /* it's a function key */
}  /*fsrequest*/


/* ****************************************************** clinit ***** */
/* *                                                                 * */
/* *           CURLEW :: Editor initialisation routines              * */
/* *                                                                 * */
/* ******************************************************************* */


/* initmodes -- set up mode array copies for each editor state */
Local Void initmodes(LINK)
struct LOC_editor *LINK;
{
  setmode(reqshift, 1);   /* request host shift operations */

  /* set up session control functions as recommended */
  setmode(kinthost, 33);   /* interrupt host;   ESC then "!" */
  setmode(ksuspend, 62);   /* suspend session;  ESC then ">" */
  setmode(krestart, 63);   /* restart session;  ESC then "?" */

  getmode(LINK->bblkmode);   /* save the embryo mode array */

  /* next three will be reset, but may leave codes on terminal keys */
  setmode(kenter, 13);   /* separate enter key;  RETURN */
  setmode(knextfld, 50);   /* next field key;  ESC then "2" */
  setmode(kprevfld, 53);   /* prev field key;  ESC then "5" */

  /* set up the other elements common to each state */
  LINK->bblkmode[kcsrup] = ctlk;   /* cursor up */
  LINK->bblkmode[kcsrdown] = ctlj;   /* cursor down;  LINEFEED */
  LINK->bblkmode[kcsrleft] = ctlh;   /* cursor left;  BACKSPACE */
  LINK->bblkmode[kcsrright] = ctll;   /* cursor right */
  LINK->bblkmode[knexttab] = ctli;   /* next tab;  TAB */
  LINK->bblkmode[kprevtab] = ctlf;   /* previous tab */
  LINK->bblkmode[kleftupd] = ctlz;   /* line start */
  LINK->bblkmode[kfirstns] = ctlx;   /* text start */
  LINK->bblkmode[kalastns] = ctlc;   /* text end */
  LINK->bblkmode[keraprev] = del;   /* erase previous;  DEL */
  LINK->bblkmode[khomefld] = ctlg;   /* home field */

  /* set up the mode array for context operation */
  memcpy(LINK->ctxtmode, LINK->bblkmode, sizeof(modearray));
  LINK->ctxtmode[kenter] = ctlm;   /* enter;  RETURN */
  LINK->ctxtmode[kinsmode] = ctlw;   /* insertion toggle */
  LINK->ctxtmode[keraright] = ctly;   /* erase to right */
  LINK->ctxtmode[kinsspac] = ctls;   /* insert space */
  LINK->ctxtmode[kdelchar] = ctle;   /* delete character */
  LINK->ctxtmode[keraline] = ctlu;   /* erase line */

  /* set up the common elements for line and character operation */
  memcpy(LINK->nfsomode, LINK->bblkmode, sizeof(modearray));
  LINK->nfsomode[ilinerow] = LINK->pinrow;   /* preferred insertion row */
  LINK->nfsomode[kinsmode] = ctlw;   /* insertion toggle */
  LINK->nfsomode[keraright] = ctly;   /* erase to right */
  LINK->nfsomode[kinsspac] = ctls;   /* insert space */
  LINK->nfsomode[kdelchar] = ctle;   /* delete character */
  LINK->nfsomode[kinsline] = ctlq;   /* insert line */
  LINK->nfsomode[kdelline] = ctlu;   /* delete line */
  LINK->nfsomode[ksplline] = ctlm;   /* split line;  RETURN */
  /* set from "initprofile" : kappline (from a+) and notify (from i+) */

  /* set up the mode array for block operation */
  memcpy(LINK->blokmode, LINK->bblkmode, sizeof(modearray));
  LINK->blokmode[knewline] = ctlm;   /* new line;  RETURN */

  /* now do the resets for basic block operation */
  LINK->bblkmode[kleftupd] = nul;   /* line start */
  LINK->bblkmode[kfirstns] = nul;   /* text start */
  LINK->bblkmode[kalastns] = nul;   /* text end */
  LINK->bblkmode[khomefld] = nul;   /* home field */
}  /*initmodes*/


/* parsecmd -- prime argument list from command parameters */
Local Void parsecmd(LINK)
struct LOC_editor *LINK;
{
  long idx;

  getinf((long)cmdpars, cmdline, (long)maxstr);
  if (cmdline[0] == endstr)
    return;
  idx = 1;
  skipbl(cmdline, &idx);
  if (cmdline[idx - 1] == newline)  /* filename given */
    return;
  nbrcmdargs = 1;
  cmdargidx[1] = idx;
  while (cmdline[idx - 1] != blank && cmdline[idx - 1] != newline)
    idx++;
  cmdline[idx - 1] = endstr;
  idx++;
  skipbl(cmdline, &idx);
  if (cmdline[idx - 1] != newline && cmdline[idx - 1] != endstr)
  {  /* initial command given */
    nbrcmdargs = 2;
    cmdargidx[2] = idx;
  }
}  /*parsecmd*/


/* setrkey -- set up relocation function key string */
Local Void setrkey(pars, n, reloc, LINK)
uchar *pars;
long n, reloc;
struct LOC_editor *LINK;
{
  chstring tkey;
  long j, rval;

  setstring(tkey, "f?/#+&                  ");
  tkey[1] = dig0 + n;
  rval = reloc;
  if (rval < 0) {
    tkey[4] = minus;
    rval = -rval;
  }
  j = itoc(rval, tkey, 6L);
  tkey[j - 1] = slash;
  tkey[j] = blank;
  tkey[j + 1] = endstr;
  scopy(tkey, 1L, pars, length_(pars) + 1);
}  /*setrkey*/


/* seteobkey -- set up end of buffer function key string */
Local Void seteobkey(pars, n, LINK)
uchar *pars;
long n;
struct LOC_editor *LINK;
{
  chstring tkey;
  long j, k;

  setstring(tkey, "f??/$;#-&               ");
  k = n / 10;
  tkey[1] = dig0 + k;
  k = n % 10;
/* p2c: cl-sun.p, line 9970:
 * Note: Using % for possibly-negative arguments [317] */
  tkey[2] = dig0 + k;
  j = itoc((long)(LINK->botrow - LINK->toprow), tkey, 9L);
  tkey[j - 1] = slash;
  tkey[j] = blank;
  tkey[j + 1] = endstr;
  scopy(tkey, 1L, pars, length_(pars) + 1);
}  /*seteobkey*/


/* initprofile -- initialise editor global parameters */
Local Void initprofile(LINK)
struct LOC_editor *LINK;
{
  chstring pars, pfile;
  uchar litfile;
  short fd;
  long j, ht;
  stcode junk;

  /* must initialise the following before a doset f/r/s/x call */
  for (j = minkey; j <= maxkey; j++) {
    LINK->kdefs[j - 1] = newline;
    LINK->kptrs[j - minkey] = j;
    LINK->kpick[j - minkey] = false;
  }
  LINK->kdmax = maxkey;
  LINK->rrend = 99;
  LINK->gbleos = 0;
  LINK->xoset = 0;

  /* basic profile */
  setstring(pars, "a+ b- c- d- e- h+ i+ j+ ");
  appstring(pars, "l+ m2 n- o0 p0 r7 s1 t* ");
  appstring(pars, "u- v- w70 x0 y0 z+&     ");
  ht = LINK->botrow - LINK->toprow + 1;
  setrkey(pars, 1L, ht / 2, LINK);
  setrkey(pars, 2L, ht, LINK);
  appstring(pars, "f3/./ &                 ");
  setrkey(pars, 4L, -(ht / 2), LINK);
  setrkey(pars, 5L, -ht, LINK);
  appstring(pars, "f6/prompt/ f7://: &     ");
  appstring(pars, "f8/keep/ &              ");
  appstring(pars, "f9/view =/ f10/view/ &  ");
  junk = doset(pars, 1L, LINK);

  /* predefined formatting keys */
  setstring(pars, "f11/format p5/ &        ");
  appstring(pars, "f12/format/ &           ");
  appstring(pars, "f13/format p-5/ &       ");
  appstring(pars, "f14/format i5/ &        ");
  appstring(pars, "f15/format i5p-5/ &     ");
  appstring(pars, "f16/format i10/ &       ");
  appstring(pars, "f17/format lp0/ &       ");
  appstring(pars, "f18/. format lp0/ &     ");
  appstring(pars, "f19/.,. format c/ &     ");
  appstring(pars, "f20/.,. format r/ &     ");
  junk = doset(pars, 1L, LINK);

  /* predefined tab stop groups, absolute relocation, windowing */
  setstring(pars, "f21/set t10 16 35 40 45 ");
  appstring(pars, "50 71/ &                ");
  appstring(pars, "f22/set t*4/ &          ");
  appstring(pars, "f23/set t*5/ &          ");
  appstring(pars, "f24/set t*8/ &          ");
  appstring(pars, "f25/1/ &                ");
  seteobkey(pars, 26L, LINK);
  appstring(pars, "f27/set x0/ &           ");
  appstring(pars, "f28/set x40-/ &         ");
  appstring(pars, "f29/set x40+/ &         ");
  appstring(pars, "f30/filename/ &         ");
  junk = doset(pars, 1L, LINK);

  /* does any site dare to override the chosen default settings? */
  getinf((long)clsitedef, pfile, (long)maxstr);
  if (pfile[0] != endstr)   /* (how sad...) */
    junk = doset(pfile, 1L, LINK);

  /* now look for user overrides */
  getinf((long)litfchar, pfile, (long)maxstr);
  litfile = pfile[0];   /* save literal file character */
  getinf((long)clprofile, pfile, (long)maxstr);
  if (pfile[0] == endstr)  /* process user profile file */
    return;
  if (pfile[0] == litfile) {
    junk = doset(pfile, 2L, LINK);   /* literal file */
    return;
  }
  fd = openf(pfile, (long)ioread);
  if (fd == ioerror_)
    return;
  while (getline(pars, fd, (long)maxstr))
    junk = doset(pars, 1L, LINK);
  closef(fd);

  /* disk file */
}  /*initprofile*/


/* resumption -- check for journal log from previous session */
Local boolean resumption(LINK)
struct LOC_editor *LINK;
{
  boolean islog, eof;
  long i;
  chstring lin;
  stcode stat;
  char warnrow;
  uchar warncol;

  islog = getline(lin, LINK->jfin, (long)maxstr);
  if (!islog) {  /* fresh session */
    jfcontrol((long)iowrite, LINK);
    if (LINK->jfout != ioerror_)
      return islog;
    showlit("  No journal file is ava", LINK);
    showlit("ilable for this session&", LINK);
    showc(newline, LINK);
    showc(newline, LINK);
    return islog;
  }
  /* first run up a flag */
  warnrow = maxrow() / 2;
  warncol = (maxcol() - 48) / 2;
  setcursor(warnrow, warncol);
  setmode(selectgr, (int)LINK->rrend);
  literal(" Last session rebuild in");
  literal(" progress; please wait! ");
  setmode(selectgr, defrend);
  /* ... file read will flush this message out to the terminal */

  /* now rebuild session from journal file */
  LINK->autoview = false;   /* no auto "v" if "edit" */
  LINK->rebuild = true;
  do {
    showcmd(LINK->blankrow, LINK);   /* reset conversation buffer */
    i = 1;
    LINK->cursave = LINK->curln;
    if (getlist(lin, &i, &LINK->status, LINK) == ok) {
      if (LINK->status != err)
	LINK->status = docmd(lin, &i, false, &LINK->status, LINK);
    }
    LINK->appstate = coned;
    eof = !getline(lin, LINK->jfin, (long)maxstr);
    if (LINK->cstate != concmd) {
      if (LINK->cstate == conapp) {
	stat = ok;
	do {
	  if (lin[0] == period && lin[1] == newline)
	    stat = enddata;
	  else
	    stat = puttxt(lin, LINK);
	  eof = !getline(lin, LINK->jfin, (long)maxstr);
	} while (!(stat != ok || eof));
      }
      LINK->cstate = concmd;
    }
  } while (!eof);
  LINK->rebuild = false;
  jfcontrol((long)ioappend, LINK);   /* continue from there */

  /* report status to the user */
  setcursor(warnrow, 0);
  erasetoright();
  showcmd(LINK->blankrow, LINK);
  showlit("  T A K E   N O T E@n@n&", LINK);

  showlit("  CURLEW has rebuilt you", LINK);
  showlit("r interrupted last sessi", LINK);
  showlit("on as far as possible.@n", LINK);

  showlit("  Please check the file ", LINK);
  showlit("buffer contents carefull", LINK);
  showlit("y before proceeding.@n@n", LINK);
  filestat(LINK);
  return islog;

  /* previous session rebuild */
}  /*resumption*/


/* edstart -- start up editor */
Local Void edstart(LINK)
struct LOC_editor *LINK;
{
  long j;
  stcode junk;
  boolean avsave;

  LINK->appstate = coned;
  LINK->cstate = concmd;
  LINK->msgdone = false;
  LINK->toprow = 2;   /* top of conversation or file display */
  LINK->botrow = maxrow() - 2;   /* bottom of file display */
  LINK->topused = false;   /* top row not used */
  LINK->pinrow = LINK->botrow - 2;   /* preferred insertion row */
  LINK->vtitle[0] = newline;
  LINK->vtitle[1] = endstr;   /* no title yet */
  initmodes(LINK);   /* set up the various mode arrays */

  jfcontrol((long)ioread, LINK);   /* journal file recovery check open */
  setbuf(LINK);   /* sets up the file buffer */
  LINK->pat[0] = endstr;   /* no last pattern yet */
  LINK->savefile[0] = endstr;   /* no saved filename yet */

  LINK->fileclone = true;   /* no internal string clone */
  LINK->cloneexists = false;   /* no clone file */
  LINK->charclone = false;   /* but if we use it, it's a line clone */
  LINK->mousetrap = safe;   /* not near block ^Q or ^W */
  LINK->rebuild = false;   /* not rebuilding a session */

  LINK->tokreq = false;   /* no token request outstanding */
  LINK->fscmask = false;   /* ...and no control characters masked */
  LINK->cmdrow = maxrow();   /* command entry row */
  LINK->cmdcol = 0;   /* force setupdate from edprompt */
  erasedisplay();
  getrow(0, LINK->blankrow);   /* this row is, of course, blank */

  initprofile(LINK);   /* editor profile */

  LINK->cbrows = cbmax + 1;   /* rows in conversation buffer */
  for (j = 0; j <= cbmax; j++)   /* clear conversation buffer */
    memcpy(LINK->cbuf[j], LINK->blankrow, sizeof(rowtext));
  ctxtimage(LINK);
  showcmd(LINK->blankrow, LINK);

  parsecmd(LINK);   /* get command parameters */
  avsave = LINK->autoview;
  if (resumption(LINK))   /* session rebuilt from journal file */
    nbrcmdargs = 0;
  /* ignore user parameters */

  if (getarg(1L, LINK->savefile, (long)maxstr)) {
    showlit("  Reading from file  :  ", LINK);
    showstr(LINK->savefile, LINK);
    showc(newline, LINK);
    junk = doread(0L, LINK->savefile, false, ioerror_, LINK);
    LINK->curln = min(1L, LINK->lastln);
    LINK->changed = false;
    journal(0L, 0L, lete, LINK->savefile, LINK);
  }

  if (getarg(2L, LINK->prevlin, (long)maxstr)) {
    flushcb(LINK);   /* flush file read verification */
    setcursor(LINK->cmdrow, LINK->cmdcol);   /* cursor on command line */
    getprevious(LINK);   /* pick up command ... */
    context(ctlm, LINK);   /* ... and execute it */
    /* re-prime conversation buffer */
    showcmd(LINK->blankrow, LINK);
  } else {
    LINK->prevlin[0] = newline;
    LINK->prevlin[1] = endstr;
  }

  if (LINK->appstate == coned) {
    if (LINK->autoview) {  /* direct entry to "v" command */
      junk = doview(LINK->curln, plus, LINK);   /* for bottom line */
      lastcb(LINK);
    } else {  /* prompt for commands */
      showc(newline, LINK);
      showlit("  Enter commands...@n@n&", LINK);
      flushcb(LINK);
      setcursor(LINK->cmdrow, LINK->cmdcol);
    }
  }
  LINK->autoview = avsave;
}  /*edstart*/


/* ****************************************************** cldispat *** */
/* *                                                                 * */
/* *     CURLEW :: Fetch and primary dispatch of SSMP primitives     * */
/* *                                                                 * */
/* ******************************************************************* */


/* service -- application T-TOKEN processing */
Local Void service(rc, LINK)
uchar rc;
struct LOC_editor *LINK;
{
  switch (LINK->appstate) {

  case entry_:   /* start up editor */
    if ((maxrow() < 15) | (maxcol() < 71)) {
      setcursor(maxrow(), 0);
      literal("Needs 16 rows by 72 colu");
      literal("mns minimum screen size.");
      LINK->screenmode = false;
    } else
      edstart(LINK);
    break;

  case coned:   /* context editor line entry */
    context(rc, LINK);
    break;

  case fsereq:   /* full-screen editor request */
    fsrequest(rc, LINK);
    break;

  case fsecmd:   /* context cmd from f/s editor */
    context(rc, LINK);
    break;

  }/*case*/

  /* candle still burning?  return the token! */
  if (LINK->screenmode)
    sendtoken(LINK->tokencode);
}  /*service*/


/* dispatch -- host application, fetch and dispatch T-primitives */
Local Void dispatch(LINK)
struct LOC_editor *LINK;
{
  uchar pc;
  primpar pars;

  getprimitive(&pc, pars);   /* fetch next primitive */
  if (pc == capa) {   /* *must* act on T-TOKEN(reqcode) */
    service(pars[0], LINK);
    return;
  }
  if (LINK->appstate != fsereq)
    return;
  switch (pc) {

  case star:   /* T-CHARACTER(ch);  replace character */
    repchar(pars[0], LINK);
    break;

  case capb:   /* T-SETCURSOR(newrow, newcol);  track cursor */
    getloc(&LINK->fsrow, &LINK->fscol);
    break;

  case capc:   /* T-SETMODE(index, ivalue);  already logged */
    break;

  case capd:   /* T-ERASETORIGHT;     delete from cursor to eol */
    deltoeol(LINK);
    break;

  case cape:   /* T-INSERTSPACE(nsp); insert spaces before cursor */
    insspaces((long)pars[0], LINK);
    break;

  case capf:   /* T-DELETECHAR(nch);  delete characters at cursor */
    delchars((long)pars[0], LINK);
    break;

  case capg:   /* T-ERASEPREV(nch) */
    bserases((long)pars[0], LINK);
    break;

  case caph:   /* T-INSERTLINE(nln);  insert lines before cursor */
    inslines((long)pars[0], LINK);
    break;

  case capi:   /* T-DELETELINE(nln);  delete lines at cursor */
    dellines((long)pars[0], LINK);
    break;

  case capj:   /* T-APPENDLINE;       append one line at cursor */
    apponeline(LINK);
    break;

  case capk:   /* T-SPLITLINE;        split one line before cursor */
    splitoneline(LINK);
    break;

  case capl:   /* T-SETFIELD(fidx);   track cursor */
    getloc(&LINK->fsrow, &LINK->fscol);
    break;

  }/*case*/
}  /*dispatch*/


/* ****************************************************** clmain ***** */
/* *                                                                 * */
/* *          CURLEW :: Main program entry and input loop            * */
/* *                                                                 * */
/* ******************************************************************* */


/* startapp -- application initialisation */
Local Void startapp(LINK)
struct LOC_editor *LINK;
{
  LINK->filenbr = 0;
  startlog(LINK);
  LINK->appstate = entry_;
  LINK->tokencode = 0;
}  /*startapp*/


/* stopapp -- application closedown */
Local Void stopapp(LINK)
struct LOC_editor *LINK;
{
  chstring cname;
  boolean junk;

  clrbuf(LINK);
  if (LINK->cloneexists) {
    getinf((long)clclone, cname, (long)maxstr);
    junk = remove(cname);
  }
}  /*stopapp*/


/* formprompt -- set up the ssmp negotiation user text prompt */
Local Void formprompt(ps, LINK)
uchar *ps;
struct LOC_editor *LINK;
{
  setstring(ps, "Please press the RETURN ");
  appstring(ps, "key to continue.&       ");
}  /*formprompt*/


/* ****************************************************** curlew ***** */
/* *                                                                 * */
/* *                 T h e   E d i t o r   C o d e                   * */
/* *                                                                 * */
/* ******************************************************************* */


/* editor -- the editor code */
Static Void editor()
{  /* editor main routine */
  struct LOC_editor V;
  chstring lin;   /* input line */


  formprompt(lin, &V);   /* "Please press RETURN ..." */
  startssmp(&V.screenmode, lin);   /* session entry negotiation */
  if (V.screenmode) {
    startapp(&V);   /* application initialisation */
    while (V.screenmode)   /* fetch and process primitives */
      dispatch(&V);
    stopapp(&V);   /* application closedown */
    /* ssmp session exit */
    stopssmp();
    return;
  }
  setstring(lin, "*** Curlew can only be u");
  appstring(lin, "sed from a terminal whic");
  appstring(lin, "h supports SSMP.@n&     ");
  putstr(lin, stderror);
}  /*editor*/

#undef maxlines
#undef curline
#undef lastline
#undef topline
#undef cmdprompt
#undef txtprompt
#undef cbmax
#undef minkey
#undef maxkey
#undef vkdlim
#undef keyleadin
#undef clbuffer
#undef clclone
#undef clprofile
#undef clsaveone
#undef clsavetwo
#undef cljournal
#undef clhelpcmd
#undef clhelpfn
#undef clsitedef
#undef clrensave
#undef clwidchk


main(argc, argv)
int argc;
Char *argv[];
{  /* curlew main program */
  PASCAL_MAIN(argc, argv);
  initsoft((long)idef);   /* software tools environment */
  editor();   /* call the editor */
  /* close the environment */
  closesoft();
  exit(0);
}  /*main program*/



/* End. */
