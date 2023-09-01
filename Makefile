#  You may need to customize the next seven lines as described in README

HELP = /usr/local/lib
SEDSTRING = "s/external;/nonpascal;/"
PCFLAGS = -c -berkeley
LINKFLAGS = -berkeley
LIBS = -ltermcap -lp2c
PASCAL = pc
LINK = pc

all : cl2 curlew.l curlew.help curlew
	touch all

clean :
	rm -f typecheck?.o ??prim.o cl*.o
	rm -f typecheck typecheck.ok ??.h all

veryclean : clean
	rm -f cl2 curlew.l st.typ.h typedefs typedefs.o
	rm -i cl-sun.c

install : all
	rm -f /usr/local/bin/cl /usr/local/bin/cl2 /usr/local/bin/curlew
	install -c -m 711 -s cl2 /usr/local/bin
	install -c -m 755 curlew /usr/local/bin
	cd /usr/local/bin; ln -s curlew cl
	rm -f /usr/man/manl/curlew.l /usr/man/manl/cl.l
	install -c -m 444 curlew.l /usr/man/manl
	echo ".so manl/curlew.l" >/usr/man/manl/cl.l
	install -c -m 444 curlew.help ${HELP}

curlew.l : curlew.t
	tbl curlew.t >curlew.l

cl2 : stprim.o tpprim.o cl-sun.o
	${CC} -o cl2 cl-sun.o stprim.o tpprim.o ${LIBS}

cl-sun.o : cl-sun.c st.h tp.h
	${CC} -c cl-sun.c

cl-sun.c : cl-sun.p st.h tp.h
	p2c cl-sun.p

st.h : st-sun.h
	sed ${SEDSTRING} <st-sun.h >st.h

tp.h : tp-sun.h
	sed ${SEDSTRING} <tp-sun.h >tp.h

stprim.o : stprim.c st.typ.h local.h iso.h typecheck.ok
	cc -c -DHELPDIR=\"${HELP}\" -DLOCAL stprim.c

tpprim.o : tpprim.c st.typ.h local.h iso.h typecheck.ok
	cc -c tpprim.c

typecheck.ok : typecheck
	typecheck
	touch typecheck.ok

typecheck : typecheck1.o typecheck2.o
	${LINK} ${LINKFLAGS} -o typecheck typecheck1.o typecheck2.o ${LIBS}

typecheck1.o : typecheck1.p tc.h
	${PASCAL} ${PCFLAGS} typecheck1.p

typecheck2.o : st.typ.h typecheck2.c
	cc -c typecheck2.c

tc.h : typecheck1.h
	sed ${SEDSTRING} <typecheck1.h >tc.h

st.typ.h : typedefs
	typedefs >st.typ.h
	chmod a-w st.typ.h

typedefs : typedefs.o
	${LINK} ${LINKFLAGS} -o typedefs typedefs.o ${LIBS}

typedefs.o : typedefs.p
	${PASCAL} ${PCFLAGS} typedefs.p
