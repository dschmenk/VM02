.SUFFIXES	=
AFLAGS          = -o $@
LFLAGS          = -C default.cfg
PLCOMP          = plc
BYTEDUMP        = bytedump
OBJS            = parse.o lex.o codegen.o
PLASMASYS	= plasma.sys
PLEDPLA		= pled.pla
PLEDSYS		= pled.sys
PLEXPLA		= plex.pla
PLEXSYS		= plex.sys
PLASMPLA	= plas.pla
PLASMSYS	= plas.sys
RODPLA          = rod.pla
RODSYS	        = rod.sys
FIREPLA         = fire.pla
FIRESYS	        = fire.sys
VMLDR		= loadvm.s
CMDPLA          = cmd.pla
CMDLDR		= loadcmd.s
CMDBYT          = cmd.byte
PLIDEPLA      	= plide.pla
PLIDESYS	= plide.sys
PLINKPLA      	= plink.pla
PLINKSYS	= plink.sys
VMCORE          = vmcore.s
VMCOREBIN       = vmcore.bin
VMCORECBIN      = vmcorec.bin
LOADER          = loader.pla
#
# Image filetypes for Virtual ][
#
PLATYPE	= .\$$ED
BINTYPE	= .BIN
SYSTYPE	= .SYS
TXTTYPE	= .TXT
#
# Image filetypes for CiderPress
#
#PLATYPE	= \#ed0000
#BINTYPE	= \#060000
#SYSTYPE	= \#ff0000
#TXTTYPE	= \#040000

all: $(RODSYS) $(FIRESYS) $(PLEDSYS) $(PLASMSYS) $(PLEXSYS) $(PLIDESYS) $(PLINKSYS) $(PLCOMP) $(BYTEDUMP) $(VMCOREBIN) $(VMCORECBIN) $(PLASMASYS)

$(PLCOMP): $(OBJS)
	cc $(OBJS) -o $(PLCOMP)

$(BYTEDUMP): bytedump.c
	cc bytedump.c -o $(BYTEDUMP)

$(RODSYS): $(PLCOMP) $(RODPLA)
	./$(PLCOMP) < $(RODPLA) > rod.s
	ca65 rod.s -o tmp.o
	ld65 $(LFLAGS) tmp.o -o $(RODSYS)
	rm tmp.o
	cp $(RODSYS) rod\#ff0000

$(FIRESYS): $(PLCOMP) $(FIREPLA)
	./$(PLCOMP) < $(FIREPLA) > fire.s
	ca65 fire.s -o tmp.o
	ld65 $(LFLAGS) tmp.o -o $(FIRESYS)
	rm tmp.o
	cp $(FIRESYS) fire\#ff0000

$(PLEDSYS): $(PLCOMP) $(PLEDPLA)
	./$(PLCOMP) < $(PLEDPLA) > pled.s
	ca65 pled.s -o tmp.o
	ld65 $(LFLAGS) tmp.o -o $(PLEDSYS)
	rm tmp.o
	cp $(PLEDSYS) pled\#ff0000

$(PLASMSYS): $(PLCOMP) $(PLASMPLA)
	./$(PLCOMP) < $(PLASMPLA) > plas.s
	ca65 plas.s -o tmp.o
	ld65 -C plide.cfg tmp.o -o $(PLASMSYS)
	rm tmp.o
	cp $(PLASMSYS) plas\#ff0000

$(PLEXSYS): $(PLCOMP) $(PLEXPLA)
	./$(PLCOMP) < $(PLEXPLA) > plex.s
	ca65 plex.s -o tmp.o
	ld65 -C plide.cfg tmp.o -o $(PLEXSYS)
	rm tmp.o
	cp $(PLEXSYS) plex\#ff0000

$(CMDBYT): $(PLCOMP) $(CMDLDR) $(CMDPLA) $(BYTEDUMP)
	./$(PLCOMP) $$ < $(CMDPLA) > cmd.s
	ca65 $(CMDLDR) -o tmp.o
	ld65 -C cmdloader.cfg tmp.o -o cmd.bin
	./$(BYTEDUMP) cmd.bin > $(CMDBYT)
	rm tmp.o

$(PLIDESYS): $(PLIDEPLA) $(PLCOMP)
	./$(PLCOMP) < $(PLIDEPLA) > plide.s
	ca65 plide.s -o tmp.o
	ld65 -C plide.cfg tmp.o -o $(PLIDESYS)
	rm tmp.o
	cp $(PLIDESYS) plide\#ff0000

$(PLINKSYS): $(PLINKPLA) $(PLCOMP)
	./$(PLCOMP) < $(PLINKPLA) > plink.s
	ca65 plink.s -o tmp.o
	ld65 $(LFLAGS) tmp.o -o $(PLINKSYS)
	rm tmp.o
	cp $(PLINKSYS) plink\#ff0000

$(VMCOREBIN): $(PLCOMP) $(VMCORE) $(CMDBYT) $(BYTEDUMP)
	ca65 $(VMCORE) -D IS65C02=0 -o tmp.o
	ld65 $(LFLAGS) tmp.o -o $(VMCOREBIN)
	rm tmp.o

$(VMCORECBIN): $(PLCOMP) $(VMCORE) $(CMDBYTE) $(BYTEDUMP)
	ca65 $(VMCORE) -D IS65C02=1 -o tmp.o
	ld65 $(LFLAGS) tmp.o -o $(VMCORECBIN)
	rm tmp.o

$(PLASMASYS): $(PLCOMP) $(VMLDR) $(VMCOREBIN)  $(VMCORECBIN)
	./$(BYTEDUMP) $(VMCOREBIN) > vmcore.byte
	./$(BYTEDUMP) $(VMCORECBIN) > vmcorec.byte
	ca65 $(VMLDR) -o tmp.o
	ld65 $(LFLAGS) tmp.o -o $(PLASMASYS)
	rm tmp.o
	cp $(PLASMASYS) plasma.system#ff0000


