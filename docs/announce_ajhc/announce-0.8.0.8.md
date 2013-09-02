# ANNOUNCE: Ajhc Haskell Compiler 0.8.0.8 Release

We are happy to announce Ajhc 0.8.0.8.
This version is a point release to fix some BUGs, and support new platforms.

You can get Ajhc using "cabal install ajhc" command.
The usage is found at Ajhc's project web site http://ajhc.metasepi.org/.
The source code at https://github.com/ajhc/ajhc/tags.

Welcome sending any bugs or your ideas to https://github.com/ajhc/ajhc/issues.

## Support new platforms

### mbed http://mbed.org/

The mbed platform is a single-board microcontroller using ARM Cortex-M3 CPU.
It has only 32kB RAM for application.

From this release, Ajhc support mbed and Text LCD device driver.
You can find the demo source code at https://github.com/ajhc/demo-cortex-m3,
and watch the demo movie at http://www.youtube.com/watch?v=fOsjEfNmTkY.

### ChibiOS/RT http://www.chibios.org/

The ChibiOS/RT is embedded real time OS.

From this release, Ajhc can compile Haskell application running on ChibiOS/RT.
The application is able to use forkOS API with ChibiOS/RT's thread.
You will find the demo source code at https://github.com/metasepi/chibios-arafura.

## Other changes

* Use cpphs command for C preprocessor.
* Don't insert (void) cast for function call.
* Add --targetsini option to specify the targets.ini file.
* Data.Bits support CInt.
* Retry download when some error occured on getting tar ball from Hackage DB.
* Fix BUGs
    * Don't increment megablock count, when re-use the megablock in free list.
    * Fix IORef compile BUG.

Enjoy! :)
- - -
Metasepi team
