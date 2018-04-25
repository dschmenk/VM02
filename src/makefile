.SUFFIXES	=
JAVAC	= javac
AFLAGS	= -o $@
LFLAGS	= -C javabin.cfg
JFLAGS	= -g:none -bootclasspath java -extdirs apple2
VM02FILE	= vm02.bin
VM02EFILE	= vm02e.bin
OBJS	= vm02.o memmgr.o codemgr.o strpool.o classclass.o sysclass.o string.o classload.o utils.o frame.o thread.o io.o ops.o except.o dvm.o
OBJSE	= vm02_e.o memmgr_e.o codemgr_e.o strpool_e.o classclass_e.o sysclass.o string.o classload.o utils.o frame.o thread_e.o io.o ops_e.o except.o dvm_e.o
LOADER	= java.system.sys
A2CLASSES	= apple2/vm02.class apple2/conio.class apple2/AppleStuff.class apple2/OutputConsole.class apple2/PrintConsole.class apple2/InputConsole.class apple2/ProDOS.class apple2/SystemException.class \
	  apple2/OutputSSC.class apple2/InputSSC.class apple2/Mouse.class apple2/Ethernet.class
DUMMYCLASSES    = java/lang/Object.class java/lang/Array.class java/lang/String.class
LANGCLASSES     = java/lang/Runnable.class java/lang/ThreadGroup.class java/lang/Thread.class java/lang/System.class java/lang/Throwable.class java/lang/Error.class java/lang/ThreadDeath.class java/lang/VirtualMachineError.class \
                  java/lang/RuntimeException.class java/lang/InternalError.class java/lang/OutOfMemoryError.class java/lang/StackOverflowError.class java/lang/LinkeageError.class java/lang/NoClassDefFoundError.class \
                  java/lang/ClassFormatError.class java/lang/IncompatibleClassChangeError.class java/lang/NoSuchFieldError.class java/lang/NoSuchMethodError.class \
                  java/lang/Exception.class java/lang/ClassCastException.class java/lang/IllegalArgumentException.class java/lang/IllegalMonitorStateException.class java/lang/IllegalThreadStateException.class java/lang/ClassNotFoundException.class \
                  java/lang/InterruptedException.class java/lang/NullPointerException.class java/lang/IndexOutOfBoundsException.class java/lang/ArrayIndexOutOfBoundsException.class java/lang/StringIndexOutOfBoundsException.class \
                  java/lang/NegativeArraySizeException.class java/lang/ArrayStoreException.class java/lang/ArithmeticException.class \
                  java/lang/Number.class java/lang/Byte.class java/lang/Short.class java/lang/Long.class java/lang/Boolean.class java/lang/Character.class java/lang/Integer.class java/lang/Float.class java/lang/StringBuilder.class
IOCLASSES       = java/io/OutputStream.class java/io/FilterOutputStream.class java/io/PrintStream.class java/io/InputStream.class java/io/FileInputStream.class java/io/FileOutputStream.class \
                  java/io/IOException.class java/io/FileNotFoundException.class java/io/EOFException.class
NETCLASSES	= java/net/NetworkDevice.class java/net/Loopback.class
CLASSES	= $(DUMMYCLASSES) $(A2CLASSES) $(LANGCLASSES) $(IOCLASSES) $(NETCLASSES)
SAMPLES	= samples/Hello.class samples/HelloWorld.class samples/SimplePong.class samples/Moire.class samples/RodsColors.class samples/HiResDemo.class samples/List.class samples/Terminal.class samples/Volumes.class \
	  samples/Catalog.class samples/TestArp.class samples/StressMem.class samples/TestChain.class samples/NextChain.class samples/TestSelect.class
FAVAC	= org/vm02/favac/Scanner.class org/vm02/favac/favac.class org/vm02/favac/fasm.class
CUI	= org/vm02/cui/cuiDriver.class org/vm02/cui/cui.class org/vm02/cui/cuiControl.class org/vm02/cui/cuiWindow.class org/vm02/cui/cuiButton.class org/vm02/cui/cuiPopUp.class org/vm02/cui/cuiPopUpMenu.class \
	  org/vm02/cui/cuiMenuBar.class org/vm02/cui/cuiDropDownMenu.class org/vm02/cui/cuiTopLevelWindow.class org/vm02/cui/cuiConsole.class \
	  org/vm02/cui/cuiScrollBar.class org/vm02/cui/cuiListBox.class org/vm02/cui/cuiTextEntry.class org/vm02/cui/cuiMessageBox.class org/vm02/cui/cuiApp.class samples/TestCUI.class \
	  org/vm02/cui/Launcher.class
VERSION	= rel1
RELEASEDIR	= ../$(VERSION)
SAMPLESDIR	= $(RELEASEDIR)/SAMPLES
ORGDIR	= $(RELEASEDIR)/ORG
ORGVM02DIR	= $(ORGDIR)/VM02
#
# Image filetypes for Virtual ][
#
JVMTYPE	= .\$$ED
BINTYPE	= .BIN
SYSTYPE	= .SYS
TXTTYPE	= .TXT
#
# Image filetypes for CiderPress
#
#JVMTYPE	= \#ed0000
#BINTYPE	= \#060000
#SYSTYPE	= \#ff0000
#TXTTYPE	= \#040000

all: $(VM02FILE) $(VM02EFILE) $(LOADER) $(CLASSES) $(SAMPLES) $(FAVAC) $(CUI)

vm02: $(VM02FILE) $(VM02EFILE) $(LOADER)

samples: $(SAMPLES)

classes: $(CLASSES)

clean:
	rm -f $(VM02FILE) $(OBJS) $(LOADER) loader.o $(CLASSES) apple2/vm02.o $(SAMPLES) $(FAVAC) $(CUI)
	rm -rf $(RELEASEDIR)
	rm -rf $(SAMPLESDIR)
	mkdir $(RELEASEDIR)
	mkdir $(RELEASEDIR)/APPLE2
	mkdir $(RELEASEDIR)/JAVA
	mkdir $(RELEASEDIR)/JAVA/LANG
	mkdir $(RELEASEDIR)/JAVA/IO
	mkdir $(RELEASEDIR)/JAVA/NET
	mkdir $(SAMPLESDIR)
	mkdir $(SAMPLESDIR)/SOURCE
	mkdir $(ORGDIR)
	mkdir $(ORGVM02DIR)
	mkdir $(ORGVM02DIR)/FAVAC
	mkdir $(ORGVM02DIR)/CUI
	
image:
	cp java.system.sys $(RELEASEDIR)/JAVA.SYSTEM$(SYSTYPE)
	cp vm02.bin $(RELEASEDIR)/VM02$(BINTYPE)
	cp vm02e.bin $(RELEASEDIR)/VM02E$(BINTYPE)
	cp samples/List.class $(RELEASEDIR)/STARTUP$(JVMTYPE)
	cp ../README.TXT $(RELEASEDIR)/README.TXT$(TXTTYPE)
	cp apple2/conio.class $(RELEASEDIR)/APPLE2/CONIO$(JVMTYPE)
	cp apple2/ProDOS.class $(RELEASEDIR)/APPLE2/PRODOS$(JVMTYPE)
	cp apple2/SystemException.class $(RELEASEDIR)/APPLE2/SYSTEMEXCEPTION$(JVMTYPE)
	cp apple2/OutputConsole.class $(RELEASEDIR)/APPLE2/OUTPUTCONSOLE$(JVMTYPE)
	cp apple2/PrintConsole.class $(RELEASEDIR)/APPLE2/PRINTCONSOLE$(JVMTYPE)
	cp apple2/InputConsole.class $(RELEASEDIR)/APPLE2/INPUTCONSOLE$(JVMTYPE)
	cp apple2/OutputSSC.class $(RELEASEDIR)/APPLE2/OUTPUTSSC$(JVMTYPE)
	cp apple2/InputSSC.class $(RELEASEDIR)/APPLE2/INPUTSSC$(JVMTYPE)
	cp apple2/Mouse.class $(RELEASEDIR)/APPLE2/MOUSE$(JVMTYPE)
	cp apple2/Ethernet.class $(RELEASEDIR)/APPLE2/ETHERNET$(JVMTYPE)
	cp java/lang/Runnable.class $(RELEASEDIR)/JAVA/LANG/RUNNABLE$(JVMTYPE)
	cp java/lang/ThreadGroup.class $(RELEASEDIR)/JAVA/LANG/THREADGROUP$(JVMTYPE)
	cp java/lang/Thread.class $(RELEASEDIR)/JAVA/LANG/THREAD$(JVMTYPE)
	cp java/lang/System.class $(RELEASEDIR)/JAVA/LANG/SYSTEM$(JVMTYPE)
	cp java/lang/Throwable.class $(RELEASEDIR)/JAVA/LANG/THROWABLE$(JVMTYPE)
	cp java/lang/Error.class $(RELEASEDIR)/JAVA/LANG/ERROR$(JVMTYPE)
	cp java/lang/ThreadDeath.class $(RELEASEDIR)/JAVA/LANG/THREADDEATH$(JVMTYPE)
	cp java/lang/VirtualMachineError.class $(RELEASEDIR)/JAVA/LANG/VIRTUALMACHINEERROR$(JVMTYPE)
	cp java/lang/OutOfMemoryError.class $(RELEASEDIR)/JAVA/LANG/OUTOFMEMORYERROR$(JVMTYPE)
	cp java/lang/StackOverflowError.class $(RELEASEDIR)/JAVA/LANG/STACKOVERFLOWERROR$(JVMTYPE)
	cp java/lang/LinkeageError.class $(RELEASEDIR)/JAVA/LANG/LINKEAGEERROR$(JVMTYPE)
	cp java/lang/NoClassDefFoundError.class $(RELEASEDIR)/JAVA/LANG/NOCLASSDEFFOUNDERROR$(JVMTYPE)
	cp java/lang/ClassFormatError.class $(RELEASEDIR)/JAVA/LANG/CLASSFORMATERROR$(JVMTYPE)
	cp java/lang/IncompatibleClassChangeError.class $(RELEASEDIR)/JAVA/LANG/INCOMPATIBLECLASSCHANGEERROR$(JVMTYPE)
	cp java/lang/NoSuchMethodError.class $(RELEASEDIR)/JAVA/LANG/NOSUCHMETHODERROR$(JVMTYPE)
	cp java/lang/NoSuchFieldError.class $(RELEASEDIR)/JAVA/LANG/NOSUCHFIELDERROR$(JVMTYPE)
	cp java/lang/Exception.class $(RELEASEDIR)/JAVA/LANG/EXCEPTION$(JVMTYPE)
	cp java/lang/RuntimeException.class $(RELEASEDIR)/JAVA/LANG/RUNTIMEEXCEPTION$(JVMTYPE)
	cp java/lang/IllegalArgumentException.class $(RELEASEDIR)/JAVA/LANG/ILLEGALARGUMENTEXCEPTION$(JVMTYPE)
	cp java/lang/IllegalMonitorStateException.class $(RELEASEDIR)/JAVA/LANG/ILLEGALMONITORSTATEEXCEPTION$(JVMTYPE)
	cp java/lang/IllegalThreadStateException.class $(RELEASEDIR)/JAVA/LANG/ILLEGALTHREADSTATEEXCEPTION$(JVMTYPE)
	cp java/lang/ClassCastException.class $(RELEASEDIR)/JAVA/LANG/CLASSCASTEXCEPTION$(JVMTYPE)
	cp java/lang/InterruptedException.class $(RELEASEDIR)/JAVA/LANG/INTERRUPTEDEXCEPTION$(JVMTYPE)
	cp java/lang/ClassNotFoundException.class $(RELEASEDIR)/JAVA/LANG/CLASSNOTFOUNDEXCEPTION$(JVMTYPE)
	cp java/lang/NullPointerException.class $(RELEASEDIR)/JAVA/LANG/NULLPOINTEREXCEPTION$(JVMTYPE)
	cp java/lang/IndexOutOfBoundsException.class $(RELEASEDIR)/JAVA/LANG/INDEXOUTOFBOUNDSEXCEPTION$(JVMTYPE)
	cp java/lang/ArrayIndexOutOfBoundsException.class $(RELEASEDIR)/JAVA/LANG/ARRAYINDEXOUTOFBOUNDSEXCEPTION$(JVMTYPE)
	cp java/lang/StringIndexOutOfBoundsException.class $(RELEASEDIR)/JAVA/LANG/STRINGINDEXOUTOFBOUNDSEXCEPTION$(JVMTYPE)
	cp java/lang/NegativeArraySizeException.class $(RELEASEDIR)/JAVA/LANG/NEGATIVEARRAYSIZEEXCEPTION$(JVMTYPE)
	cp java/lang/ArrayStoreException.class $(RELEASEDIR)/JAVA/LANG/ARRAYSTOREEXCEPTION$(JVMTYPE)
	cp java/lang/ArithmeticException.class $(RELEASEDIR)/JAVA/LANG/ARITHMETICEXCEPTION$(JVMTYPE)
	cp java/lang/Byte.class $(RELEASEDIR)/JAVA/LANG/BYTE$(JVMTYPE)
	cp java/lang/Short.class $(RELEASEDIR)/JAVA/LANG/SHORT$(JVMTYPE)
	cp java/lang/Long.class $(RELEASEDIR)/JAVA/LANG/LONG$(JVMTYPE)
	cp java/lang/Boolean.class $(RELEASEDIR)/JAVA/LANG/BOOLEAN$(JVMTYPE)
	cp java/lang/Character.class $(RELEASEDIR)/JAVA/LANG/CHARACTER$(JVMTYPE)
	cp java/lang/Number.class $(RELEASEDIR)/JAVA/LANG/NUMBER$(JVMTYPE)
	cp java/lang/Integer.class $(RELEASEDIR)/JAVA/LANG/INTEGER$(JVMTYPE)
	cp java/lang/Float.class $(RELEASEDIR)/JAVA/LANG/FLOAT$(JVMTYPE)
	cp java/lang/StringBuilder.class $(RELEASEDIR)/JAVA/LANG/STRINGBUILDER$(JVMTYPE)
	cp java/io/OutputStream.class $(RELEASEDIR)/JAVA/IO/OUTPUTSTREAM$(JVMTYPE)
	cp java/io/FileOutputStream.class $(RELEASEDIR)/JAVA/IO/FILEOUTPUTSTREAM$(JVMTYPE)
	cp java/io/FilterOutputStream.class $(RELEASEDIR)/JAVA/IO/FILTEROUTPUTSTREAM$(JVMTYPE)
	cp java/io/PrintStream.class $(RELEASEDIR)/JAVA/IO/PRINTSTREAM$(JVMTYPE)
	cp java/io/InputStream.class $(RELEASEDIR)/JAVA/IO/INPUTSTREAM$(JVMTYPE)
	cp java/io/FileInputStream.class $(RELEASEDIR)/JAVA/IO/FILEINPUTSTREAM$(JVMTYPE)
	cp java/io/IOException.class $(RELEASEDIR)/JAVA/IO/IOEXCEPTION$(JVMTYPE)
	cp java/io/FileNotFoundException.class $(RELEASEDIR)/JAVA/IO/FILENOTFOUNDEXCEPTION$(JVMTYPE)
	cp java/io/EOFException.class $(RELEASEDIR)/JAVA/IO/EOFEXCEPTION$(JVMTYPE)
	cp java/net/NetworkDevice.class $(RELEASEDIR)/JAVA/NET/NETWORKDEVICE$(JVMTYPE)
	cp java/net/Loopback.class $(RELEASEDIR)/JAVA/NET/LOOPBACK$(JVMTYPE)
	cp samples/Hello.class $(SAMPLESDIR)/HELLO$(JVMTYPE)
	cp samples/HelloWorld.class $(SAMPLESDIR)/HELLOWORLD$(JVMTYPE)
	cp samples/List.class $(SAMPLESDIR)/LIST$(JVMTYPE)
	cp samples/SimplePong.class $(SAMPLESDIR)/SIMPLEPONG$(JVMTYPE)
	cp samples/Moire.class $(SAMPLESDIR)/MOIRE$(JVMTYPE)
	cp samples/HiResDemo.class $(SAMPLESDIR)/HIRESDEMO$(JVMTYPE)
	cp samples/RodsColors.class $(SAMPLESDIR)/RODSCOLORS$(JVMTYPE)
	cp samples/Terminal.class $(SAMPLESDIR)/TERMINAL$(JVMTYPE)
	cp samples/Volumes.class $(SAMPLESDIR)/VOLUMES$(JVMTYPE)
	cp samples/Catalog.class $(SAMPLESDIR)/CATALOG$(JVMTYPE)
	cp samples/TestArp.class $(SAMPLESDIR)/TESTARP$(JVMTYPE)
	cp samples/StressMem.class $(SAMPLESDIR)/STRESSMEM$(JVMTYPE)
	cp samples/TestChain.class $(SAMPLESDIR)/TESTCHAIN$(JVMTYPE)
	cp samples/NextChain.class $(SAMPLESDIR)/NEXTCHAIN$(JVMTYPE)
	cp samples/TestSelect.class $(SAMPLESDIR)/TESTSELECT$(JVMTYPE)
	cp samples/TestCUI.class $(SAMPLESDIR)/TESTCUI$(JVMTYPE)
	cp samples/HelloWorld.java $(SAMPLESDIR)/SOURCE/HelloWorld.java$(TXTTYPE)
	cp samples/List.java $(SAMPLESDIR)/SOURCE/List.java$(TXTTYPE)
	cp samples/SimplePong.java $(SAMPLESDIR)/SOURCE/SimplePong.java$(TXTTYPE)
	cp samples/Moire.java $(SAMPLESDIR)/SOURCE/Moire.java$(TXTTYPE)
	cp samples/Terminal.java $(SAMPLESDIR)/SOURCE/Terminal.java$(TXTTYPE)
	cp samples/HiResDemo.java $(SAMPLESDIR)/SOURCE/HiResDemo.java$(TXTTYPE)
	cp samples/RodsColors.java $(SAMPLESDIR)/SOURCE/RodsColors.java$(TXTTYPE)
	cp samples/Volumes.java $(SAMPLESDIR)/SOURCE/Volumes.java$(TXTTYPE)
	cp samples/Catalog.java $(SAMPLESDIR)/SOURCE/Catalog.java$(TXTTYPE)
	cp samples/TestArp.java $(SAMPLESDIR)/SOURCE/TestArp.java$(TXTTYPE)
	cp samples/StressMem.java $(SAMPLESDIR)/SOURCE/StressMem.java$(TXTTYPE)
	cp samples/TestSelect.java $(SAMPLESDIR)/SOURCE/TestSelect.java$(TXTTYPE)
	cp samples/TestCUI.java $(SAMPLESDIR)/SOURCE/TestCUI.java$(TXTTYPE)
	cp samples/TestChain.java $(SAMPLESDIR)/SOURCE/TestChain.java$(TXTTYPE)
	cp samples/NextChain.java $(SAMPLESDIR)/SOURCE/NextChain.java$(TXTTYPE)
	cp org/vm02/favac/Scanner.class $(ORGVM02DIR)/FAVAC/SCANNER$(JVMTYPE)
	cp org/vm02/favac/favac.class $(ORGVM02DIR)/FAVAC/FAVAC$(JVMTYPE)
	cp org/vm02/favac/fasm.class $(ORGVM02DIR)/FAVAC/FASM$(JVMTYPE)
	cp org/vm02/cui/cui.class $(ORGVM02DIR)/CUI/CUI$(JVMTYPE)
	cp org/vm02/cui/cuiControl.class $(ORGVM02DIR)/CUI/CUICONTROL$(JVMTYPE)
	cp org/vm02/cui/cuiWindow.class $(ORGVM02DIR)/CUI/CUIWINDOW$(JVMTYPE)
	cp org/vm02/cui/cuiTopLevelWindow.class $(ORGVM02DIR)/CUI/CUITOPLEVELWINDOW$(JVMTYPE)
	cp org/vm02/cui/cuiConsole.class $(ORGVM02DIR)/CUI/CUICONSOLE$(JVMTYPE)
	cp org/vm02/cui/cuiButton.class $(ORGVM02DIR)/CUI/CUIBUTTON$(JVMTYPE)
	cp org/vm02/cui/cuiPopUp.class $(ORGVM02DIR)/CUI/CUIPOPUP$(JVMTYPE)
	cp org/vm02/cui/cuiPopUpMenu.class $(ORGVM02DIR)/CUI/CUIPOPUPMENU$(JVMTYPE)
	cp org/vm02/cui/cuiMenuBar.class $(ORGVM02DIR)/CUI/CUIMENUBAR$(JVMTYPE)
	cp org/vm02/cui/cuiDropDownMenu.class $(ORGVM02DIR)/CUI/CUIDROPDOWNMENU$(JVMTYPE)
	cp org/vm02/cui/cuiMessageBox.class $(ORGVM02DIR)/CUI/CUIMESSAGEBOX$(JVMTYPE)
	cp org/vm02/cui/cuiScrollBar.class $(ORGVM02DIR)/CUI/CUISCROLLBAR$(JVMTYPE)
	cp org/vm02/cui/cuiTextEntry.class $(ORGVM02DIR)/CUI/CUITEXTENTRY$(JVMTYPE)
	cp org/vm02/cui/cuiApp.class $(ORGVM02DIR)/CUI/CUIAPP$(JVMTYPE)
	cp org/vm02/cui/Launcher.class $(RELEASEDIR)/LAUNCHER$(JVMTYPE)

$(LOADER): loader.o
	ld65 -C loader.cfg loader.o -o $(LOADER)
	cp $(LOADER) $(RELEASEDIR)/JAVA.SYSTEM$(SYSTYPE)
    
$(VM02FILE): $(OBJS)
	ld65 $(LFLAGS) -m vm02.map $(OBJS) -o $(VM02FILE)
	cp $(VM02FILE) $(RELEASEDIR)/VM02$(BINTYPE)

$(VM02EFILE): $(OBJSE)
	ld65 $(LFLAGS) -m vm02e.map $(OBJSE) -o $(VM02EFILE)
	cp $(VM02EFILE) $(RELEASEDIR)/VM02E$(BINTYPE)

#
# VM02 source
#

loader.o: loader.s
	ca65 $(AFLAGS) $<
	
vm02.o: vm02.s global.inc
	ca65 $(AFLAGS) $<

vm02_e.o: vm02.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

memmgr.o: memmgr.s global.inc
	ca65 $(AFLAGS) $<

memmgr_e.o: memmgr.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

codemgr.o: codemgr.s global.inc
	ca65 $(AFLAGS) $<

codemgr_e.o: codemgr.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

strpool.o: strpool.s global.inc
	ca65 $(AFLAGS) $<

strpool_e.o: strpool.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

thread.o: thread.s global.inc
	ca65 $(AFLAGS) $<

thread_e.o: thread.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

frame.o: frame.s global.inc frame.inc
	ca65 $(AFLAGS) $<

classclass.o: classclass.s global.inc
	ca65 $(AFLAGS) $<

classclass_e.o: classclass.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

sysclass.o: sysclass.s global.inc object.clasm array.clasm
	ca65 $(AFLAGS) $<

string.o: string.s global.inc string.clasm
	ca65 $(AFLAGS) $<

classload.o: classload.s global.inc
	ca65 $(AFLAGS) $<

ops.o: ops.s global.inc
	ca65 $(AFLAGS) $<

ops_e.o: ops.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

io.o: io.s consoledrvr.s sscdrvr.s mousedrvr.s global.inc
	ca65 $(AFLAGS) $<

except.o: except.s global.inc class.inc frame.inc
	ca65 $(AFLAGS) $<

utils.o: utils.s global.inc
	ca65 $(AFLAGS) $<

dvm.o: dvm.s global.inc
	ca65 $(AFLAGS) $<

dvm_e.o: dvm.s global.inc
	ca65 $(AFLAGS) -D BIGMEM=1 $<

#
# Dummy Java base classes
#

java/lang/Object.class: ObjectObj.s
	ca65 ObjectObj.s
	ld65 --target apple2 -o java/lang/Object.class ObjectObj.o

java/lang/Array.class: ArrayObj.s
	ca65 ArrayObj.s
	ld65 --target apple2 -o java/lang/Array.class ArrayObj.o

java/lang/String.class: StringObj.s
	ca65 StringObj.s
	ld65 --target apple2 -o java/lang/String.class StringObj.o

#
# Apple II classes
#

apple2/vm02.class: apple2/vm02.java apple2/vm02.clasm
	$(JAVAC) $(JFLAGS) $<
	ca65 apple2/vm02.clasm
	ld65 -C clasm.cfg apple2/vm02.o -o $(RELEASEDIR)/APPLE2/VM02$(JVMTYPE)

apple2/conio.class: apple2/conio.java apple2/vm02.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/conio.class $(RELEASEDIR)/APPLE2/CONIO$(JVMTYPE)

apple2/AppleStuff.class: apple2/AppleStuff.java apple2/vm02.java
	$(JAVAC) $(JFLAGS) $<
	ca65 apple2/AppleStuff.clasm
	ld65 -C clasm.cfg apple2/AppleStuff.o -o $(RELEASEDIR)/APPLE2/APPLESTUFF$(JVMTYPE)

apple2/ProDOS.class: apple2/ProDOS.java apple2/vm02.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/ProDOS.class $(RELEASEDIR)/APPLE2/PRODOS$(JVMTYPE)

apple2/SystemException.class: apple2/SystemException.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/SystemException.class $(RELEASEDIR)/APPLE2/SYSTEMEXCEPTION$(JVMTYPE)

apple2/OutputConsole.class: apple2/OutputConsole.java java/io/OutputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/OutputConsole.class $(RELEASEDIR)/APPLE2/OUTPUTCONSOLE$(JVMTYPE)

apple2/PrintConsole.class: apple2/PrintConsole.java java/io/PrintStream.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/PrintConsole.class $(RELEASEDIR)/APPLE2/PRINTCONSOLE$(JVMTYPE)

apple2/InputConsole.class: apple2/InputConsole.java java/io/InputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/InputConsole.class $(RELEASEDIR)/APPLE2/INPUTCONSOLE$(JVMTYPE)

apple2/OutputSSC.class: apple2/OutputSSC.java java/io/OutputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/OutputSSC.class $(RELEASEDIR)/APPLE2/OUTPUTSSC$(JVMTYPE)

apple2/InputSSC.class: apple2/InputSSC.java java/io/InputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/InputSSC.class $(RELEASEDIR)/APPLE2/INPUTSSC$(JVMTYPE)

apple2/Mouse.class: apple2/Mouse.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/Mouse.class $(RELEASEDIR)/APPLE2/MOUSE$(JVMTYPE)

apple2/Ethernet.class: apple2/Ethernet.java
	$(JAVAC) $(JFLAGS) $<
	cp apple2/Ethernet.class $(RELEASEDIR)/APPLE2/ETHERNET$(JVMTYPE)

#
# Standard Java lang classes
#

java/lang/Runnable.class: java/lang/Runnable.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Runnable.class $(RELEASEDIR)/JAVA/LANG/RUNNABLE$(JVMTYPE)

java/lang/ThreadGroup.class: java/lang/ThreadGroup.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ThreadGroup.class $(RELEASEDIR)/JAVA/LANG/THREADGROUP$(JVMTYPE)

java/lang/Thread.class: java/lang/Thread.java apple2/vm02.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Thread.class $(RELEASEDIR)/JAVA/LANG/THREAD$(JVMTYPE)

java/lang/System.class: java/lang/System.java apple2/vm02.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/System.class $(RELEASEDIR)/JAVA/LANG/SYSTEM$(JVMTYPE)

java/lang/Throwable.class: java/lang/Throwable.java apple2/vm02.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Throwable.class $(RELEASEDIR)/JAVA/LANG/THROWABLE$(JVMTYPE)

java/lang/Error.class: java/lang/Error.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Error.class $(RELEASEDIR)/JAVA/LANG/ERROR$(JVMTYPE)

java/lang/InternalError.class: java/lang/InternalError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/InternalError.class $(RELEASEDIR)/JAVA/LANG/INTERNALERROR$(JVMTYPE)

java/lang/ThreadDeath.class: java/lang/ThreadDeath.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ThreadDeath.class $(RELEASEDIR)/JAVA/LANG/THREADDEATH$(JVMTYPE)

java/lang/VirtualMachineError.class: java/lang/VirtualMachineError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/VirtualMachineError.class $(RELEASEDIR)/JAVA/LANG/VIRTUALMACHINEERROR$(JVMTYPE)

java/lang/OutOfMemoryError.class: java/lang/OutOfMemoryError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/OutOfMemoryError.class $(RELEASEDIR)/JAVA/LANG/OUTOFMEMORYERROR$(JVMTYPE)

java/lang/StackOverflowError.class: java/lang/StackOverflowError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/StackOverflowError.class $(RELEASEDIR)/JAVA/LANG/STACKOVERFLOWERROR$(JVMTYPE)

java/lang/LinkeageError.class: java/lang/LinkeageError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/LinkeageError.class $(RELEASEDIR)/JAVA/LANG/LINKEAGEERROR$(JVMTYPE)

java/lang/NoClassDefFoundError.class: java/lang/NoClassDefFoundError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/NoClassDefFoundError.class $(RELEASEDIR)/JAVA/LANG/NOCLASSDEFFOUNDERROR$(JVMTYPE)

java/lang/ClassFormatError.class: java/lang/ClassFormatError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ClassFormatError.class $(RELEASEDIR)/JAVA/LANG/CLASSFORMATERROR$(JVMTYPE)

java/lang/IncompatibleClassChangeError.class: java/lang/IncompatibleClassChangeError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/IncompatibleClassChangeError.class $(RELEASEDIR)/JAVA/LANG/INCOMPATIBLECLASSCHANGEERROR$(JVMTYPE)

java/lang/NoSuchFieldError.class: java/lang/NoSuchFieldError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/NoSuchFieldError.class $(RELEASEDIR)/JAVA/LANG/NOSUCHFIELDERROR$(JVMTYPE)

java/lang/NoSuchMethodError.class: java/lang/NoSuchMethodError.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/NoSuchMethodError.class $(RELEASEDIR)/JAVA/LANG/NOSUCHMETHODERROR$(JVMTYPE)

java/lang/Exception.class: java/lang/Exception.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Exception.class $(RELEASEDIR)/JAVA/LANG/EXCEPTION$(JVMTYPE)

java/lang/RuntimeException.class: java/lang/RuntimeException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/RuntimeException.class $(RELEASEDIR)/JAVA/LANG/RUNTIMEEXCEPTION$(JVMTYPE)

java/lang/IllegalArgumentException.class: java/lang/IllegalArgumentException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/IllegalArgumentException.class $(RELEASEDIR)/JAVA/LANG/ILLEGALARGUMENTEXCEPTION$(JVMTYPE)

java/lang/IllegalMonitorStateException.class: java/lang/IllegalMonitorStateException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/IllegalMonitorStateException.class $(RELEASEDIR)/JAVA/LANG/ILLEGALMONITORSTATEEXCEPTION$(JVMTYPE)

java/lang/IllegalThreadStateException.class: java/lang/IllegalThreadStateException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/IllegalThreadStateException.class $(RELEASEDIR)/JAVA/LANG/ILLEGALTHREADSTATEEXCEPTION$(JVMTYPE)

java/lang/ClassCastException.class: java/lang/ClassCastException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ClassCastException.class $(RELEASEDIR)/JAVA/LANG/CLASSCASTEXCEPTION$(JVMTYPE)

java/lang/InterruptedException.class: java/lang/InterruptedException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/InterruptedException.class $(RELEASEDIR)/JAVA/LANG/INTERRUPTEDEXCEPTION$(JVMTYPE)

java/lang/ClassNotFoundException.class: java/lang/ClassNotFoundException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ClassNotFoundException.class $(RELEASEDIR)/JAVA/LANG/CLASSNOTFOUNDEXCEPTION$(JVMTYPE)

java/lang/NullPointerException.class: java/lang/NullPointerException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/NullPointerException.class $(RELEASEDIR)/JAVA/LANG/NULLPOINTEREXCEPTION$(JVMTYPE)

java/lang/IndexOutOfBoundsException.class: java/lang/IndexOutOfBoundsException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/IndexOutOfBoundsException.class $(RELEASEDIR)/JAVA/LANG/INDEXOUTOFBOUNDSEXCEPTION$(JVMTYPE)

java/lang/ArrayIndexOutOfBoundsException.class: java/lang/ArrayIndexOutOfBoundsException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ArrayIndexOutOfBoundsException.class $(RELEASEDIR)/JAVA/LANG/ARRAYINDEXOUTOFBOUNDSEXCEPTION$(JVMTYPE)

java/lang/StringIndexOutOfBoundsException.class: java/lang/StringIndexOutOfBoundsException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/StringIndexOutOfBoundsException.class $(RELEASEDIR)/JAVA/LANG/STRINGINDEXOUTOFBOUNDSEXCEPTION$(JVMTYPE)

java/lang/NegativeArraySizeException.class: java/lang/NegativeArraySizeException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/NegativeArraySizeException.class $(RELEASEDIR)/JAVA/LANG/NEGATIVEARRAYSIZEEXCEPTION$(JVMTYPE)

java/lang/ArrayStoreException.class: java/lang/ArrayStoreException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ArrayStoreException.class $(RELEASEDIR)/JAVA/LANG/ARRAYSTOREEXCEPTION$(JVMTYPE)

java/lang/ArithmeticException.class: java/lang/ArithmeticException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/ArithmeticException.class $(RELEASEDIR)/JAVA/LANG/ARITHMETICEXCEPTION$(JVMTYPE)

java/lang/Boolean.class: java/lang/Boolean.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Boolean.class $(RELEASEDIR)/JAVA/LANG/BOOLEAN$(JVMTYPE)

java/lang/Byte.class: java/lang/Byte.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Byte.class $(RELEASEDIR)/JAVA/LANG/BYTE$(JVMTYPE)

java/lang/Short.class: java/lang/Short.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Short.class $(RELEASEDIR)/JAVA/LANG/SHORT$(JVMTYPE)

java/lang/Long.class: java/lang/Long.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Long.class $(RELEASEDIR)/JAVA/LANG/LONG$(JVMTYPE)

java/lang/Character.class: java/lang/Character.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Character.class $(RELEASEDIR)/JAVA/LANG/CHARACTER$(JVMTYPE)

java/lang/Number.class: java/lang/Number.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Number.class $(RELEASEDIR)/JAVA/LANG/NUMBER$(JVMTYPE)

java/lang/Integer.class: java/lang/Integer.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Integer.class $(RELEASEDIR)/JAVA/LANG/INTEGER$(JVMTYPE)

java/lang/Float.class: java/lang/Float.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Float.class $(RELEASEDIR)/JAVA/LANG/FLOAT$(JVMTYPE)

java/lang/Void.class: java/lang/Void.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Void.class $(RELEASEDIR)/JAVA/LANG/VOID$(JVMTYPE)

java/lang/Class.class: java/lang/Class.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/Class.class $(RELEASEDIR)/JAVA/LANG/CLASS$(JVMTYPE)

java/lang/StringBuilder.class: java/lang/StringBuilder.java
	$(JAVAC) $(JFLAGS) $<
	cp java/lang/StringBuilder.class $(RELEASEDIR)/JAVA/LANG/STRINGBUILDER$(JVMTYPE)

#
# Standard Java I/O classes
#

java/io/OutputStream.class: java/io/OutputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/OutputStream.class $(RELEASEDIR)/JAVA/IO/OUTPUTSTREAM$(JVMTYPE)

java/io/FileOutputStream.class: java/io/FileOutputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/FileOutputStream.class $(RELEASEDIR)/JAVA/IO/FILEOUTPUTSTREAM$(JVMTYPE)

java/io/FilterOutputStream.class: java/io/FilterOutputStream.java java/io/OutputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/FilterOutputStream.class $(RELEASEDIR)/JAVA/IO/FILTEROUTPUTSTREAM$(JVMTYPE)

java/io/PrintStream.class: java/io/PrintStream.java java/io/FilterOutputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/PrintStream.class $(RELEASEDIR)/JAVA/IO/PRINTSTREAM$(JVMTYPE)

java/io/InputStream.class: java/io/InputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/InputStream.class $(RELEASEDIR)/JAVA/IO/INPUTSTREAM$(JVMTYPE)

java/io/FileInputStream.class: java/io/FileInputStream.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/FileInputStream.class $(RELEASEDIR)/JAVA/IO/FILEINPUTSTREAM$(JVMTYPE)

java/io/IOException.class: java/io/IOException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/IOException.class $(RELEASEDIR)/JAVA/IO/IOEXCEPTION$(JVMTYPE)

java/io/FileNotFoundException.class: java/io/FileNotFoundException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/FileNotFoundException.class $(RELEASEDIR)/JAVA/IO/FILENOTFOUNDEXCEPTION$(JVMTYPE)

java/io/EOFException.class: java/io/EOFException.java
	$(JAVAC) $(JFLAGS) $<
	cp java/io/EOFException.class $(RELEASEDIR)/JAVA/IO/EOFEXCEPTION$(JVMTYPE)

#
# Java networking classes
#

java/net/NetworkDevice.class: java/net/NetworkDevice.java
	$(JAVAC) $(JFLAGS) $<
	cp java/net/NetworkDevice.class $(RELEASEDIR)/JAVA/NET/NETWORKDEVICE$(JVMTYPE)

java/net/Loopback.class: java/net/Loopback.java
	$(JAVAC) $(JFLAGS) $<
	cp java/net/Loopback.class $(RELEASEDIR)/JAVA/NET/LOOPBACK$(JVMTYPE)

#
# Sample classes
#

samples/Hello.class: samples/Hello.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/Hello.class $(SAMPLESDIR)/HELLO$(JVMTYPE)

samples/HelloWorld.class: samples/HelloWorld.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/HelloWorld.class $(SAMPLESDIR)/HELLOWORLD$(JVMTYPE)

samples/List.class: samples/List.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/List.class $(SAMPLESDIR)/LIST$(JVMTYPE)

samples/SimplePong.class: samples/SimplePong.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/SimplePong.class $(SAMPLESDIR)/SIMPLEPONG$(JVMTYPE)

samples/Moire.class: samples/Moire.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/Moire.class $(SAMPLESDIR)/MOIRE$(JVMTYPE)

samples/HiResDemo.class: samples/HiResDemo.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/HiResDemo.class $(SAMPLESDIR)/HIRESDEMO$(JVMTYPE)

samples/RodsColors.class: samples/RodsColors.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/RodsColors.class $(SAMPLESDIR)/RODSCOLORS$(JVMTYPE)

samples/Terminal.class: samples/Terminal.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/Terminal.class $(SAMPLESDIR)/TERMINAL$(JVMTYPE)

samples/Volumes.class: samples/Volumes.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/Volumes.class $(SAMPLESDIR)/VOLUMES$(JVMTYPE)

samples/Catalog.class: samples/Catalog.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/Catalog.class $(SAMPLESDIR)/CATALOG$(JVMTYPE)

samples/TestArp.class: samples/TestArp.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/TestArp.class $(SAMPLESDIR)/TESTARP$(JVMTYPE)

samples/StressMem.class: samples/StressMem.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/StressMem.class $(SAMPLESDIR)/STRESSMEM$(JVMTYPE)

samples/TestChain.class: samples/TestChain.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/TestChain.class $(SAMPLESDIR)/TESTCHAIN$(JVMTYPE)

samples/NextChain.class: samples/NextChain.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/NextChain.class $(SAMPLESDIR)/NEXTCHAIN$(JVMTYPE)

samples/TestSelect.class: samples/TestSelect.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/TestSelect.class $(SAMPLESDIR)/TESTSELECT$(JVMTYPE)
#
# Fava compiler
#
org/vm02/favac/Scanner.class: org/vm02/favac/Scanner.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/favac/Scanner.class $(ORGVM02DIR)/FAVAC/SCANNER$(JVMTYPE)

org/vm02/favac/favac.class: org/vm02/favac/favac.java  org/vm02/favac/Scanner.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/favac/favac.class $(ORGVM02DIR)/FAVAC/FAVAC$(JVMTYPE)

org/vm02/favac/fasm.class: org/vm02/favac/fasm.java  org/vm02/favac/Scanner.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/favac/fasm.class $(ORGVM02DIR)/FAVAC/FASM$(JVMTYPE)
#
# Character User Interface
#
org/vm02/cui/cuiDriver.class: org/vm02/cui/cuiDriver.java org/vm02/cui/cuiDriver.clasm
	$(JAVAC) $(JFLAGS) $<
	ca65 org/vm02/cui/cuiDriver.clasm
	ld65 -C clasm.cfg org/vm02/cui/cuiDriver.o -o \$(ORGVM02DIR)/CUI/CUIDRIVER$(JVMTYPE)

org/vm02/cui/cui.class: org/vm02/cui/cui.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cui.class $(ORGVM02DIR)/CUI/CUI$(JVMTYPE)

org/vm02/cui/cuiControl.class: org/vm02/cui/cuiControl.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiControl.class $(ORGVM02DIR)/CUI/CUICONTROL$(JVMTYPE)

org/vm02/cui/cuiButton.class: org/vm02/cui/cuiButton.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiButton.class $(ORGVM02DIR)/CUI/CUIBUTTON$(JVMTYPE)

org/vm02/cui/cuiWindow.class: org/vm02/cui/cuiWindow.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiWindow.class $(ORGVM02DIR)/CUI/CUIWINDOW$(JVMTYPE)

org/vm02/cui/cuiTopLevelWindow.class: org/vm02/cui/cuiTopLevelWindow.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiTopLevelWindow.class $(ORGVM02DIR)/CUI/CUITOPLEVELWINDOW$(JVMTYPE)

org/vm02/cui/cuiConsole.class: org/vm02/cui/cuiConsole.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiConsole.class $(ORGVM02DIR)/CUI/CUICONSOLE$(JVMTYPE)

org/vm02/cui/cuiPopUp.class: org/vm02/cui/cuiPopUp.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiPopUp.class $(ORGVM02DIR)/CUI/CUIPOPUP$(JVMTYPE)

org/vm02/cui/cuiPopUpMenu.class: org/vm02/cui/cuiPopUpMenu.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiPopUpMenu.class $(ORGVM02DIR)/CUI/CUIPOPUPMENU$(JVMTYPE)

org/vm02/cui/cuiListBox.class: org/vm02/cui/cuiListBox.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiListBox.class $(ORGVM02DIR)/CUI/CUILISTBOX$(JVMTYPE)

org/vm02/cui/cuiMenuBar.class: org/vm02/cui/cuiMenuBar.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiMenuBar.class $(ORGVM02DIR)/CUI/CUIMENUBAR$(JVMTYPE)

org/vm02/cui/cuiDropDownMenu.class: org/vm02/cui/cuiDropDownMenu.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiDropDownMenu.class $(ORGVM02DIR)/CUI/CUIDROPDOWNMENU$(JVMTYPE)

org/vm02/cui/cuiMessageBox.class: org/vm02/cui/cuiMessageBox.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiMessageBox.class $(ORGVM02DIR)/CUI/CUIMESSAGEBOX$(JVMTYPE)

org/vm02/cui/cuiScrollBar.class: org/vm02/cui/cuiScrollBar.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiScrollBar.class $(ORGVM02DIR)/CUI/CUISCROLLBAR$(JVMTYPE)

org/vm02/cui/cuiTextEntry.class: org/vm02/cui/cuiTextEntry.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiTextEntry.class $(ORGVM02DIR)/CUI/CUITEXTENTRY$(JVMTYPE)

org/vm02/cui/cuiApp.class: org/vm02/cui/cuiApp.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/cuiApp.class $(ORGVM02DIR)/CUI/CUIAPP$(JVMTYPE)

org/vm02/cui/Launcher.class: org/vm02/cui/Launcher.java
	$(JAVAC) $(JFLAGS) $<
	cp org/vm02/cui/Launcher.class $(RELEASEDIR)/LAUNCHER$(JVMTYPE)

samples/TestCUI.class: samples/TestCUI.java
	$(JAVAC) $(JFLAGS) $<
	cp samples/TestCUI.class $(SAMPLESDIR)/TESTCUI$(JVMTYPE)
