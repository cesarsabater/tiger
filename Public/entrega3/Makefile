# Unix makefile for tigermain example

#HOME=~
#MOSMLHOME=mosml
#MOSMLTOOLS=camlrunm /usr/share/mosml/tools
#MOSMLLEX=mosmllex
#MOSMLYACC=mosmlyac -v

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=${MOSMLHOME}/bin/camlrunm $(MOSMLHOME)/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac -v


GCC=gcc
CFLAGS= -g
#MOSMLC=mosmlc -c -liberal
#MOSMLL=mosmlc

MOSMLC=${MOSMLHOME}/bin/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/bin/mosmlc


# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo topsort.uo tigerseman.uo tigertemp.uo tigertree.uo \
	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigercanon.uo tigerinterp.uo tigercodegen.uo tigerassem.uo \
	tigerutils.uo tigergraph.uo tigerflow.uo tigermakegraph.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigerseman.ui: tigerabs.uo 
tigerflow.ui: tigergraph.ui tigertemp.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.ui 
tigertab.uo: tigertab.ui 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigerlex.uo \
    tigerutils.ui tigertrans.ui tigerpp.uo 
tigerflow.uo: tigerflow.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigergraph.uo: tigergraph.ui tigertab.ui tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigerpp.uo: tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigerframe.ui: tigertree.uo tigerassem.ui tigertemp.ui 
tigerutils.ui: tigertree.uo tigerframe.ui tigerassem.ui 
tigertemp.uo: tigertemp.ui 
tigerutils.uo: tigerutils.ui tigercodegen.ui tigerframe.ui tigerit.uo \
    tigercanon.ui tigerassem.ui tigertrans.ui 
tigergraph.ui: tigertab.ui 
tigermakegraph.ui: tigerassem.ui tigerflow.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigerseman.uo: tigerseman.ui tigercodegen.ui tigersres.uo tigertab.ui \
    tigerpila.ui topsort.uo tigercanon.ui tigerassem.ui tigertemp.ui \
    tigerabs.uo tigertrans.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigerassem.ui: tigertemp.ui 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.ui \
    tigertemp.ui 
tigermakegraph.uo: tigermakegraph.ui tigerassem.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigertree.uo: tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerassem.uo: tigerassem.ui tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.ui tigertemp.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.ui 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
