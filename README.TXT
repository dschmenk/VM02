VM02 Apple II Java Virtual Machine
==================================

Introduction: 
-------------
VM02 is a Java compatible environment for the Apple II series of computers. Most basic features of a Java VM are implemented to allow direct execution of standard Java class files.

However, in order to fit in the constraints of a 64K, 8 bit, 1 MHz computer, some aspects of a JVM have been reduced or removed.

What is implemented: 
- Standard Java class file loading and execution. 
- Full object inheritence 
- Multithreading (4 threads max by default) 
- Exception handling 
- Arrays up to 15 dimension 
- 32 bit integers and single precision floating point 
- Garbage collection 
- Demand memory swapping to RAM disk or other available volume
- Finalizers 
- Software and hardware implemented timing 
- Device drivers for common hardware
- 128K memory support for //e and //c
- Exec-ing another class

What is missing:
- 64 bit data types unimplemented
- Standard class library missing or limited implementation
- Lots of memory

Running VM02:
-------------
VM02 is run by executing the JAVA.SYSTEM file.  You must start JAVA.SYSTEM from the prefix where it exists.  This is required to find the class base path to the support classes.  Parameter passing is supported if your shell implements it. If no paramter is passed in and a file named STARTUP exists, it will be loaded as the initial class.  Otherwise, you will be greeted with with a prompt to type in the class name to execute.  You can start classes in other directories by typing in the fully qualified path. Class filenames under ProDOS are missing the ".class" suffix due to limited filename lengths.  For example, to execute the Hello.class file on the /VM02.EXTRAS/SAMPLES volume, with VM02 you would type:

/VM02.EXTRAS/SAMPLES/HELLO

at the "Main class:" prompt.  You would see this:

Main class:/VM02.EXTRAS/SAMPLES/HELLO

and then VM02 would attempt to find and load the class file.  If an error occurs along the way, you will get an Unhandled Exception message, probably because the file (or superclass file) was not found.  The preferred directory structure is to place all classes as subdirectories of the main VM02 directory.

Model Compatibility:
--------------------
VM02 has been tested on a variety of Apple II models from a Rev 0 Apple ][ with Integer Basic to a //c+ and much in-between.  Most models run without issue, however some need a little support.  Noteably, my ROM 1 IIgs needs to run VM02 under GS/OS to run without problems.  Without pathces applied by GS/OS, the system quickly gets random values blasted throughout memory.  When I added support for 128K and more sophisticated memory management, it caused the IIgs to falter more and more.  Rock solid under GS/OS.  I was unable to test a ROM 3 IIgs, so I don't know if those problems persist.  Earlier models of the //c might lose VBL interrupts after being run for awhile.  The unenhanced IIe should run the CUI programs but will look funny without the MouseText characters.  Pre-IIe models won't run the CUI programs at all.

Implementation of VM02: 
----------------------- 
VM02 is implemented from scratch in 6502 assembly, optimized for the 6502 and Apple II platform.  The entire VM fits in about 20K of RAM.  This leaves about 30K of RAM available for data and code in the 64K version, or about 32K for data and 44K for bytecode in the 128K version. Not a lot, but Java was defined for small platforms so you can get some reasonable programs even in such a small space.  Swapping from main memory extends the available RAM, dependent on the swap volume.  A new low level pseudo-machine was written to implement some of the non-time critical routines.  Somewhat of a cross between the Pascal p-machine and Sweet-16, DVM (Dave's Virtual Machine) is optimized for position independent 16 bit operations.  The class loader and some memory manager routines are implemented in DVM to save code space (about 4X) or allow for on-the-fly relocation.  A high-resolution graphics page is available for use with small programs.  Performance is acceptable for a 1 MHz, 8 bit CPU; on par with the UCSD p-System VM used in Apple Pascal.  All features of the Apple II are made available through either specific classes, a low-level class that can call ROM routines, or both.  There is a device driver architecture that currently supports 80 column cards, the Super Serial card, and the Apple Mouse card with preliminary support of the Uthernet card and Apple Graphics Tablet.  Probing for the Uthernet card can cause problems in some machines with hardware VM02 doesn't detect.  If probing for the Uthernet card causes the machine to hang at VM02 init time, hold down the Open-Apple key (or PushButton 1) during initialization and the Uthernet probe will be skipped.

As noted earlier, class filenames are missing the ".class" suffix, however the file type of $ED is used to identify class files and are given the type identifier of JVM.  System classes are in the JAVA/* subdirectories.  Apple II specific classes are found in the APPLE2/ directory.  VM02 specific class are found in the ORG/VM02/* subdirectories.

Developing for VM02: 
-------------------- 
There isn't a native Java compiler for the Apple II (yet).  All Java source must be cross-developed on a capable computer.  The resulting class file needs to be transferred to an Apple or emulator using any number of available tools. One way is to use a tool like a2tools, AppleCommander, or CiderPress to copy the class file to a .DSK disk image file.  The disk image can be run directly from an emulator or copied to real hardware with ADT or ADTPro.  To compile against the Apple II specific classes, either download the source package (you will also need cc65, the 6502 C compiler package) or copy the APPLE2/ directory contents and rename the files with mixed case and add the .class suffix.

The sample classes: 
------------------- 
Included with VM02 are sample classes that show off the capabilites of the environment.  They can be executed from the samples disk or copied to another location.

HELLO        - A simple text entry program 
HELLOWORLD   - sample multi-threaded program 
MOIRE        - Hires example, requires AppleSoft in ROM 
HIRESDEMO    - Another hires demo 
RODSCOLORS   - Lores demo 
SIMPLEPONG   - Lores demo with paddle input 
TERMINAL     - Simple terminal program using the SSC 
VOLUMES      - List online volumes
CATALOG      - Catalog a directory
LIST         - List a text file to the screen 
STRESSMEM    - Allocate 64K 2D array.  Stresses swapping code
TESTSELECT   - select() type call using mouse, keyboard, and timeouts
TESTCHAIN    - Chain to another class passing parameters and exit status
TESTCUI      - Show off the CUI's controls (needs 128K)

The Launcher class:
-------------------
With the release of version 1.0, a file launcher is included to navigate through volumes and directories and execute Java classes.  Files of type $ED show up as JVM executable files.

Release Version 1.0:
--------------------
This represents the first release of VM02.  Much of the implementation exists as skeletons of a complete Java environment.  There is enough to run basic programs and do minimalistic file I/O.

Known issues:
-------------
No know issues exist.

March 31st 2010, 11:59:59 PM
Dave Schmenk...
