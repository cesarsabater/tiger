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
	tigerutils.uo

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
tigerit.uo: tigertree.uo tigertab.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.ui 
tigerutils.ui: tigertree.uo tigerframe.ui tigerassem.ui 
tigerseman.uo: tigerseman.ui tigercodegen.ui tigersres.uo tigertab.ui \
    tigerpila.ui topsort.uo tigercanon.ui tigerassem.ui tigertemp.ui \
    tigerabs.uo tigertrans.ui 
tigerassem.ui: tigertemp.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigertree.uo: tigertemp.ui 
tigerassem.uo: tigerassem.ui tigertemp.ui 
tigermakegraph.uo: tigermakegraph.ui tigerassem.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.ui tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
Graph.uo: Graph.ui tigertab.ui tigertemp.ui 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigerframe.ui: tigertree.uo tigerassem.ui tigertemp.ui 
tigerescap.ui: tigerabs.uo 
tigerflow.uo: Graph.ui tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigerlex.uo \
    tigerutils.ui tigertrans.ui tigerpp.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertemp.uo: tigertemp.ui 
tigermakegraph.ui: tigerassem.ui tigerflow.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.ui 
tigerpila.uo: tigerpila.ui 
Graph.ui: tigertab.ui 
tigerpp.uo: tigerabs.uo 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.ui 
tigerutils.uo: tigerutils.ui tigercodegen.ui tigerframe.ui tigerit.uo \
    tigercanon.ui tigerassem.ui tigertrans.ui 
tigertab.uo: tigertab.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.ui \
    tigertemp.ui 
tigergrm.ui: tigerabs.uo 
