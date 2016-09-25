# ProtectedMode
Switch to protected mode and draw text or graphics on the screen.

Program Notes for w.exe:
Most of the program notes are inside the program as comments.

The program is initially set up to run in text mode with paging enabled.  It runs under FreeDos with 

no Device drivers enabled.  A full screen of asterisks are output directly to the screen.  After a 

short delay asterisk will be displayed in the upper left corner of the screen.  If letters of the 

alphabet are keyed in after the asterisks are displayed, and they will replace the asterisks after 

the <enter> key is pressed.  The program will then return to DOS.
    To switch to graphics mode, Blocks marked text must be commented out and blocks marked graphics 

must be uncommented.  Also, the correct Linear Frame Buffer Address must be used, which can be 

discovered using BIOS call 10h function 4f01h.  On the machine used to test this program, that 

address happens to be 0e8000000h. In addition, if paging is enabled, then the 512th entry in the page 

table must be correctly set to this address.  In the program, the instruction mov 

eax,0e8020000h-4096+7 accomplishes this task because the table is filled in backwards from high to 

low memory.  
In graphics mode, one sixth of the screen is filled with blue pixels in 1024 x 768 256 color Vesa vbe 

mode. If more lines are drawn to the screen than 128 lines, the program will crash because only 32 

entries (128k) are filled in the page table.  The program then return to the DOS prompt after a short 

delay.
	Some of the delays in the program are implemented as tight loops, and others are coded using 

the timer interrupt.  If the keyboard interrupt is enabled while the timer interrupt occurs, then the 

program might crash (It usually does!)
	Lastly, great care must be exercised when switching back and forth between text and graphic 

mode, and between paging and non-paging.  In other words, it is very easy to crash this program, as 

there are many "magic" numbers in it.  Therefore, code blocks that need to be swapped have been 

marked with letters (A, B, C, D, E, F) to make it easier to switch.

P.S. If anyone can get the floppy disk write routine to work, please let me know.
I just could not figure out how to set the input buffer address of the write in protected mode.  The 

code does work in real mode, in a separate program.  

P.S.S 	Many thanks to David Lindaur, who created the original program that w.exe has extended.

Good luck,
Steve Fink
srfink0@gmail.com
