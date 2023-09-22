;-------------------------------------------------------------------------;
; Programmer: Steven Fink                                                 ;
; Date: September 3, 2016                                                 ;
; Email Address: srfink0@gmail.com                                        ;
;                                                                         ;
; This program runs under FreeDos. It moves the system from real to       ;
; protected mode with paging enabled or disabled.                         ;
; It can display text characters on the screen, or high resolution        ;
; graphics using the VESA standard.                                       ;
; It can read characters from the keyboard, translate some of them, and   ;
; display them on the screen.                                             ;
; It can allow breakpoints to be entered using the int3 instruction.      ;
; It can allow the user to set a timer using interrupts.                  ;
; Finally, it reads the 1st sector of the 1st cylinder (The Boot Sector)  ;
; on a floppy diskette in drive A:.                                       ;
; with DMA mode turned off, thereby sidestepping the need for a buffer    ;
; address.  Then is displays the first four bytes of that sector.         ;
;                                                                         ;
; Many thanks to David Lindaur, who created the original program in 1995  ;
; that this program has built on.  The original program displayed text on ;
; screen only. His program was the best example of protected mode         ;
; programming on the Web that I could find.                               ;
;                                                                         ;
; This program may be built using TASM.  It may be debugged using int 3   ;
; instructions and displaying memory or by using the displayDebugInfo     ;
; function defined in this program.                                       ;
;                                                                         ;
; Important notes:                                                        ;
; (1) For the graphics to work, the correct Linear Frame Buffer address   ;
;     must be found for your video card.                                  ;
;     On my DOS machine, the Linear Frame Buffer Address happens to be    ;
;     0e8000000h, which was discovered using the bios call int 10h,       ;;     function 4f01h.                                                     ;
; (2) SoftIce cannot be used to debug this program, as far as I can tell. ;;                                                                         ;
; The following commands in a batch file were used to build this program. ;
;  tasm /zi /l w.asm, w.obj                                               ;
;  tlink /3 /v /m w.obj, w.exe                                            ;
;-------------------------------------------------------------------------;


  .MODEL SMALL
  .386P

_DPT_  struc
  srt_hut db  ?
  dma_hlt db  ?
  motor_w db  ?
  sec_size db ?
  eot   db  ?
  gap_rw  db  ?
  dtl     db  ?
  gap_f   db  ?
  fill_char db ?
  hst     db  ?
  mot_start db ?
_DPT_ ends

GDTENTRY  MACRO base,limit,type,flag
  dw  flag
  dw  limit AND 0ffffh
  dw  base AND 0ffffh
  db  (base SHR 16) AND 0ffh
  dw  type OR ((limit SHR 8) AND 0f00h)
  db  base SHR 24
ENDM

NUMGDT = 6    ;Number of entries in GDT

IDTSIZE = 800h
GDTSIZE = NUMGDT*8
gdtzerooffset = 1
gdtlastentry = 2

CS386 EQU 8   ;386 code segment selector
DS386 EQU 10h ;386 data segment selector
CS8086  EQU 18h ;8086 code segment selector
DS8086  EQU 20h ;8086 data segment selector

DSABS EQU 28h   ;Absolute data segment selector

CYL  equ  0

DGROUP  GROUP seg8086,seg386

SEG8086  SEGMENT DWORD USE16 public 'CODE'
SEG8086  ENDS

SEG386  SEGMENT DWORD USE32 PUBLIC 'CODE'
SEG386  ENDS

ABSDATA SEGMENT DWORD USE32 AT 0
ABSDATA ENDS

  .STACK 4096
tos LABEL BYTE

SEG386  SEGMENT
buffer   db  'ABC'
         db  509 DUP('A')
tGDT  db  8 DUP(-1)   ;protected mode GET
      db  GDTSIZE-8 DUP(0)

tIDT  db  IDTSIZE DUP (0)

pGDT  dw  GDTSIZE-1   ;Protected mode GDT register
gdtadr  dd  offset tGDT

pIDT    dw  IDTSIZE-1
idtadr  dd  offset tIDT

ridt    dw  03ffh
        dd  0
        align

zero    dd  0   ;Offset of Protected mode from absolute zero

kbuffer db 200 DUP(0) 
keyTable db 15 DUP(0)
        db 'QWERTYUIOP'
        db 4 DUP(0) 
        db 'ASDFGHJKL'
        db 5 DUP(0)
        db 'ZXCVBNM'
        db 33 DUP(0)  ; PAD to end of scan code table
kbIndex dd  0
IRQ     db  0
of      dw  ?
timer   dd  0   ;initial value of timer
sg      dw  ?

SEG386  ENDS

ABSDATA SEGMENT
    org 90000h
 PageDir    dd  1024  DUP(?)
 PageTables dd  1024 * 4 DUP(?)
 VideoPageTable dd  1024 DUP(?)
 ABSDATA  ENDS

seg8086 SEGMENT
  assume  cs:dgroup
workarea  db  256 DUP('X')
modeblock  db 256 DUP('C') 
returnMsg  db 'Press <Enter> to return to DOS$'
bootstrap:
;-----------------------------------------------------------------;
; Either use the first block right below to set the video mode to 
; high resolution graphics, or the next block for text mode.
; First, the vesa mode information is retrieved
; Set vesa vbe mode to 1024x768 256 colors
;  mov   ax, 4f01h ; get vesa mode information
;  mov   cx, 4105h ; 1024*768 256 colors using linear frame buffer
;  push  es
;  push  cs
;  pop   es
;  mov   di, offset modeblock
;  int   10h
;  pop   es
;  mov   ax,4f02h ; set vesa mode
;  mov   bx,4105h ; bit 14 is Linear frame buffer mode
;  int   10h      ; It is OK to use BIOS before moving to protected mode
;-------------------------------------------------------------------;
;mov ax,0003h  ;text mode
;int 10h       ;It is OK to use BIOS before moving to protected mode 
;-------------------------------------------------------------------;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converts binary value in ax 
; to ascii for display
; and returns the result in bx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  cld
  cli     ;Interrupts clear throughout

  mov bx,cs   ;Set ES=DS=CS=DGROUP
  mov ds,bx
  mov es,bx

  mov [word ptr zero],bx
  shl [zero],4    ;Fix ZERO address
  mov ebx,[zero]  ;Load it
  mov dl,[byte ptr zero + 2]  ;Load 64k segment
  add [gdtadr],ebx      ;GDT and IDT regs must have a physical value
  add [idtadr],ebx      

  mov esi,offset iGDT   ;Load GDT table pointers
  mov edi,offset tGDT + 8

  ; Create the GDT
  mov cx,NUMGDT-1
gdtinit:
  lodsw       ;Get flags
  movsd       ;Move GDT entry to table
  movsd
  test  ax,gdtZeroOffset    ;Adjust this entry?
  jz  short nooffset        ;No, continue
  add [word ptr di+2-8],bx  ;Else add in the ZERO offset
  add [byte ptr di+4-8],dl  ;to make the segment base address (seg addr 0)

nooffset:
  test  ax,gdtLastEntry
  jnz short endGDT
  loop  gdtinit
endGDT:
; Create the single IDT trap gate entry for breakpoints
;  mov esi,offset iIDT   ;Load GDT table pointers
;  mov edi,offset tIDT + 24  ;Interrupt number 3 for breakpoint
;  mov cx,2  ; two double words (8 bytes)
;  cld   ;forward direction
;  rep movsd ; copy idt entry (4th idt entry)
;
  ;Load the GDT descripter
  lgdt  [fword ptr pGDT]
  ;Load the IDT descripter
  lidt  [fword ptr pIDT]

  ; Save Linear Frame Buffer address
;  mov edi, dword ptr [modeblock+28h] ; save frame buffer base

  ;
  ;Switch to protected mode.
  ;
    mov ebx,CR0
    inc ebx
    mov CR0,ebx
; Perform an inter-segment jump to our protected mode code.
  db  066h    ;Far jump into protected mode
  db  0eah
  dw  SMALL DGROUP:ProtectedMode
  dw  0
  dw  CS386   ;Here we tell it to switch to prot mode seg
SEG8086 ends

seg386  SEGMENT
iIDT  LABEL WORD
  dw  SMALL DGROUP:userprog
  dw  CS386
  db  0
  db  0efh  ; present, dpl 3 trap gate
  dw  0
iGDT  LABEL WORD
  GDTENTRY  0,0fffffh,0c09Ah,gdtZeroOffset  ; 386 code
  GDTENTRY  0,0fffffh,0c092h,gdtZeroOffset  ; 386 data
;  dw  0ffffh  ; segment length
;  db  0,0,0   ; segment address 0-15, 16-23
;  db  91h     ; flags 1
;  db  0cfh    ; flags 2
;  db  0       ; segment address 24-31
  GDTENTRY  0,00ffffh,9Ah,gdtZeroOffset     ; 8086 code
  GDTENTRY  0,00ffffh,92h,gdtZeroOffset     ; 8086 data
  GDTENTRY  0,0fffffh,0c092h,gdtLastEntry    ; Absolute
; Work area to store data to be displayed at breakpoint
work  db 8 DUP(42,7)
dpt   db 16 DUP(0)   ;Diskette Parameter Table
ProtectedMode LABEL BYTE
  mov ebx,ss      ;Readjust stack for protected mode
  shl ebx,4
  movzx esp,sp
  add esp,ebx
  mov bx,DS386    ;Protected mode stack segments
  mov ss,bx
  sub esp,ss:[zero]
  mov ds,bx       ;DS,ES = primary data segment
  mov es,bx
  mov bx,DSABS    ;FS,GS = Absolute zero data segment
  mov fs,bx
  mov gs,bx
  call  initpagetables
  call  initinterrupt
; Turn paging on
  mov eax,offset pageDir;   set the page directory base register
  mov CR3,eax
  mov eax,CR0
  or  eax,80000000h
  mov CR0,eax
  ; Here we are in protected mode, with a sensible environment
  ; set up in the selectors.
protection:
  mov   bx,0724h  ;char and attribute to fill screen
  call  userprog
endprotection:
  cli
  ;Turn paging off by resetting the page enable bit
  ;Again, physical and linear addresses must be the same for
  ;this segment of code
  mov eax,CR0
  and eax,7fffffffh
  mov CR0,eax
  ;Prepare to return to real mode
  ;Reload our selectors with real mode compatible segment values.
  add esp,[zero]    ; Stack adjust
  ror esp,4
  mov ebx,DS8086    ; Real mode compatible data segments
  mov ds,ebx
  mov es,ebx
  mov fs,ebx
  mov gs,ebx
  mov ss,ebx
  ;
  ;Perform an inter-segment jump to our 16 it protected mode
  ;code.  This is necessary to ensure that CS is reloaded with
  ;a 16 it segment selector.  This jump has to be 'hand coded' as
  ;it is not supported by the assembler because its a very
  ;dangerous thing to do unless you know what you are doing.
  db  0eah    ;Jump to 8086 compatible segment
  dw  SMALL DGROUP:RealMode1
  dw  0
  dw  CS8086  ;Switch to real mode compatible segment
  ; Switch to Real mode.
RealMode1:
  mov ebx,CR0   ;Back to real mode
  dec ebx
  mov CR0,ebx
;Perform an inter-segment jump to our real mode code.
  db  0eah
  dw  DGROUP:RealMode2
  dw  DGROUP
SEG386  ENDS

SEG8086 SEGMENT
RealMode2:
; Restore our segment registers to GS=FS=ES=DS=CS=DGROUP  
  mov bx,dgroup
  mov ds,bx
  mov es,bx
  mov fs,bx
  mov gs,bx
  sub esp,10h     ; Finish adjusting stack
  mov ss,sp       ; Leave at least 256 bytes for 8086
  shr esp,28
  add sp,100h
  lidt  [fword ptr ridt]  ; Load the real mode idt 

  ; Here we are back in Real mode with our segment registers set
  ; up to something sensible.
; mov ax,0003h
; int 10h ; Return to text mode before returning to DOS
  mov ah,0
  int 16h ; wait for keypress before returning to DOS

  mov ah,04ch
  int 21h

SEG8086 ENDS
SEG386  SEGMENT
; Create all page tables
initpagetables  proc
  push  ds
  push  es
  push  fs
  pop   ds    ; Switch to the absolute data segment
  push  fs
  pop   es
  ;
  ; First create the page tables that will allow 'see-though'
  ; access to the lower MB

  mov edi,offset PageTables
  mov eax,16 * 1024 * 1024 + 7-4096 ;Physical address 16MB-4096
                                    ; + lower 3 bits set
  mov ecx,1024 * 4    ; 4 tables of 1024 bytes is 16MB
                      ; which allocates 4096 bytes is 16MB
fill16mb:
  mov [edi+ecx*4-4],eax ;  Fill in the table entry
  sub eax,4096          ; Move to offset of next lower page
  loop  fill16mb
  ;
  ; Now create the page table where we map 0a0000 to 80000000h
  ; It will be 128k long, so we only need fill in the first 32 entries
  ;
  ; Before we fill it in we are going to invalidate everything in it
  ;
  mov edi,offset videopagetable
  mov ecx,1024
  mov eax,0fffffffeh
  rep stosd
  ;
  mov edi,offset videopagetable
; mov eax,0e8020000h-4096+7  ; Top page of the address to access(graphic)
  mov eax,0c0000h-4096+7  ; Top page of the address to access(text)
  mov ecx,32
fillvideo:
  mov [edi+ecx*4-4],eax ; Fill in the table entry
  sub eax,4096          ; Move to offst of next lower page
  loop  fillvideo       ; continue till done
  ;
  ; Now create the page directory
  mov edi,offset PageDir
  mov ecx,1024
  mov eax,0fffffffeh
  rep stosd
  ;
  mov eax,offset PageTables
  or  al,7
  mov edi,offset pagedir
  mov [edi],eax
  add eax,4096
  mov [edi+4],eax
  add eax,4096
  mov [edi+8],eax
  add eax,4096
  mov [edi+12],eax
  mov eax,offset videopagetable
  or  al,7    ; Add in the page table mode bits
  mov [edi+512*4],eax
  pop es
  pop ds
  ret
initpagetables  endp
;
; This is the int 8 timer handler
;
timerinterrupt  proc
  push  ds
  push  eax
  push  DS386
  pop   ds      ; Make sure DS is the data segment
  inc   [timer] ; Increment the timer
  mov   al,20h
  out   20h,al
  pop   eax
  pop   ds
  iretd
timerinterrupt  endp
 
diskinterrupt proc
  mov ax,fs
  mov es,ax
  sub di,di
  mov ax,0f2ah
  mov word ptr es:[di],ax
  iretd
diskinterrupt endp

keyboardRtn proc
  push  es
  push  eax
  push  ebx
  push  ecx
  push  edx
  push  esi
  push  edi
; Store entered key scan code into keyboard buffer  
  lea ebx,kbuffer
  mov edx,kbIndex
  xor eax,eax
  in  al,60h    ; Get scan code of down keypress
  cmp al,1Ch
  je  EnterKey
  cmp al,9Ch
  je  EnterKey
  ; Convert scan code to ascii
  lea esi,keyTable
  xor edi,edi 
  mov edi,eax
  dec edi
  mov al,keyTable[edi]
  mov byte ptr[ebx][edx],al
  inc edx
  mov kbIndex,edx
  jmp LoadES
EnterKey:
  mov byte ptr[ebx][edx],1Ch
  call displayKbBuffer
;  call displayDisketteParmTab
LoadES:
; Acknowledge reception of the keyboard event
  in  al,61h    ; Send acknowledgment without
  or  al,10000000b  ; modifying the other bits
  out 61h,al
  and al,01111111b
  out 61h,al
  mov al,20h  ; Send End-of-Interrupt signal
  out 20h,al
;
;  mov ax,DSABS
;  mov es,ax
;  call  displayDebugInfo
  pop edi
  pop esi
  pop edx
  pop ecx
  pop ebx
  pop eax
  pop es
  iretd
keyboardRtn endp

floppyDisketteRtn proc
  ;mov eax,11223344h
  ;call  displayDebugInfo2
  mov IRQ,1
;  outportb (0x20, 0x20);
  mov al,20h
  out 20h,al
  iretd
floppyDisketteRtn endp

debugRtn  proc
;  push  eax
  mov eax,09090909h
  call  displayDebugInfo
;  call  displayDebugInfo2
;  pop eax
;  cli
;  pushfd
;  pushad
;  mov   ax,0721h  ;exclamation mark char with normal attribute
;  mov   cx,24*80  ;fill entire screen
;  mov   edi,80000000h + 18000h  ; b800 for text mode
;  push  DSABS
;  pop   es
;  rep stosw
;  sti
;  push  ds
;  push  DS386 
;  pop   ds
;tloop2:
;    cmp [timer],90  ;Wait five seconds
;    jc  tloop2
;  pop ds
;  popad
;  popfd
;  sti
;  push  eax
;  mov eax,offset breakpt
;  mov byte ptr[eax],90h
;  pop   eax
;  cli
  iretd
debugRtn  endp

 initinterrupt  proc
  mov edi,8 * 8 + offset tIDT     ; interrupts are 8 bytes long

  mov eax,offset timerinterrupt
  mov [edi],ax          ; Start by plopping in the code offset
  shr eax,16
  mov [edi + 6],ax      ; High 16 bits
  mov word ptr [edi + 2],CS386  ; Now plop in the Code segment selector
  mov byte ptr [edi + 4],0      ; Always 0 for interrupts
  mov byte ptr [edi + 5],08eh   ; Signifies an interrupt gate, present 

; Setup IDT entry for the floppy diskette device
;  mov edi,8 * 10 + offset tIDT     ; interrupts are 8 bytes long
;  mov eax,offset diskinterrupt
;  mov [edi],ax          ; Start by plopping in the code offset
;  shr eax,16
;  mov [edi + 6],ax      ; High 16 bits
;  mov word ptr [edi + 2],CS386  ; Now plop in the Code segment selector
;  mov byte ptr [edi + 4],0      ; Always 0 for interrupts
;  mov byte ptr [edi + 5],08eh   ; Signifies an interrupt gate, present 

  ; setup idt entry for the breakpoint interrupt
  mov edi,8 * 3 + offset tIDT     ; interrupts are 8 bytes long
                                   ; we are hooking int 3
  mov eax,offset debugRtn
  mov [edi],ax          ; Start by plopping in the code offset
  shr eax,16
  mov [edi + 6],ax      ; High 16 bits
  mov word ptr [edi + 2],CS386  ; Now plop in the Code segment selector
  mov byte ptr [edi + 4],0      ; Always 0 for interrupts
  mov byte ptr [edi + 5],0efh   ; Signifies an trap gate, present 

; setup idt entry for the keyboard interrupt
  mov edi,8 * 9 + offset tIDT     ; interrupts are 8 bytes long
                                   ; we are hooking int 9
  mov eax,offset KeyboardRtn
  mov [edi],ax          ; Start by plopping in the code offset
  shr eax,16
  mov [edi + 6],ax      ; High 16 bits
  mov word ptr [edi + 2],CS386  ; Now plop in the Code segment selector
  mov byte ptr [edi + 4],0      ; Always 0 for interrupts
  mov byte ptr [edi + 5],08eh   ; Signifies an trap gate, present 

  ; Setup idt entry for the interrupt for the floppy diskette controller
  mov edi,8 * 14 + offset tIDT     ; interrupts are 8 bytes long

  mov eax,offset floppyDisketteRtn
  mov [edi],ax          ; Start by plopping in the code offset
  shr eax,16
  mov [edi + 6],ax      ; High 16 bits
  mov word ptr [edi + 2],CS386  ; Now plop in the Code segment selector
  mov byte ptr [edi + 4],0      ; Always 0 for interrupts
  mov byte ptr [edi + 5],08eh   ; Signifies an interrupt gate, present 


  ret
initinterrupt endp
;;
; Protected mode user program.  At this oint everything is initialized
; except for unhandled interrupts.  Display data on the screen then
; disable the keyboard while we wait around for a cuple of seconds
;
; Input to userprog:  bx = char and attribute to fill screen
displayDebugInfo  proc
  ; Sample program to fill the screen with '!'
  ;
  push  es      ; Load the screen up with '!'
  push  fs
  push  ecx
  push  DSABS
  pop   es
  mov   ebx,offset work
; call  htoa        ; Convert hex value in ax to ascii
  ;mov   ax,0721h    ; Black & White '!'
  ;
  ; Uncomment this line to access at the proper address
   mov edi,80000000h + 18000h    ; B*00 = a000 + 18000
;  mov edi,0b8000h      ;Linear Frame Buffer screen address
  ;
  ; The next line accesses screen memory at the 80000000h page
   mov  esi,offset work ;point to work area where data to display is stored
;   mov edi,80000000h + 18000h    ; B*00 = a000 + 18000
;   mov ecx,80*24     ; 24x80 screen
 ;  mov  eax,01010101h
 ;  mov  ecx,1024*16/4
    mov ecx,4   ;Setup to display 8 bytes (4 ascii characters)
    rep movsd
    pop ecx
    pop fs
    pop es
    ;
    ; Enable interrupts and wait around a couple of seconds
    ;
    ; First, disable keyboard interrupts
    ; This way, if a key comes in we won't crash
    ;
; try to read in the enter key
      in  al,60h    ;read key press
      in  al,21h    ;PIC interrupt control port
      or  al,2      ; Bit to disable keyboard interrupts
      out 21h,al    ; Do disable (without this out instruction, it crashes
      sti
    mov ecx,1000
loopit2: loop loopit2
tloop1:
      cmp [timer],36  ; Wait until timer has ticked 36 times (two seconds)
      jc  tloop1
      cli
;
; Reenable keyboard interrupts
  in  al,21h    ; Read PIC interrupt port
  and al,0fdh   ; reenable keyboard
  out 21h,al    ; Do enable
  ret
;
displayDebugInfo  endp

displayDebugInfo2 proc
  push  ds
  push  es      ; Load the screen up with '!'
  push  fs
  push  ebx
  push  ecx
  push  edx
  mov   dx,fs
; mov   dx,DSABS
  mov   es,dx
  mov   ds,dx
  mov   ebx,offset work
; call  htoa        ; Convert hex value in ax to ascii
  mov   edi,0b8000h      ;Linear Frame Buffer screen address
; mov edi,80000000h + 18000h    ; B*00 = a000 + 18000
  mov   esi,offset work ;point to work area where data to display is stored
  mov   ecx,4   ;Setup to display 8 bytes (4 ascii characters)
  rep   movsd
  mov   ecx,4194000000
loopit: loop loopit
  ; recalibrate
  pop   edx
  pop   ecx
  pop   ebx
  pop   fs
  pop   es
  pop   ds
  ret
displayDebugInfo2 endp

displayKbBuffer proc
  push  es
  push  esi
  push  edi
  push  ax

  mov   ax,DSABS
  mov   es,ax
  mov   edi,0b8000h  ; Screen origin
  lea   esi,kbuffer  ; Point to keyboard buffer origin
nextChar:
  mov   al,byte ptr[esi]
;  mov   ebx,dword ptr[si]
;  call  displayDebugInfo
  mov   ah,0fh  ; high intensity attribute
  mov   word ptr es:[edi],ax ; Display one keyboard character
  inc   esi
  inc   esi  ; Next keyboard character
  inc   edi
  inc   edi  ; Next screen location
  cmp   byte ptr[esi],1Ch  ;Enter key?
  jnz   nextChar

  pop   ax
  pop   edi
  pop   esi
  pop   es

  ret
displayKbBuffer endp

displayDisketteParmTab  proc
  push  es
  push  ds
  push  esi
  push  edi
  push  ecx
  push  eax
  push  ebx

  mov   esi,0f9c32h  ; Point to the diskette parameter table
;  mov   eax,dword ptr [0f9c32h]   ; Get dpt data
;  mov   eax,42424242h ; Get dpt data
  mov   edi,offset dpt  ; address of diskette parameter table save area
  mov   ecx,11    ; Prepare to copy 11 bytes
  cld
  rep   movsb   ; Save the diskette parameter table data
  mov   esi,0f9c32h  ; Point to the diskette parameter table
  mov   eax,dword ptr ds:[esi]
  mov   ebx,offset dpt
  mov   al,mot_start[ebx]
  mov   ebx,offset work
; call  htoa         ; Convert hex bytes to ascii
  mov   ecx,4
  mov   edi,0b8000h  ; Screen origin
  mov   esi,offset work
  cld
  rep movsd
  pop   ebx
  pop   eax
  pop   ecx
  pop   edi
  pop   esi
  pop   ds
  pop   es

  ret
displayDisketteParmTab  endp
; Protected mode user program.  At this oint everything is initialized
; except for unhandled interrupts.  Display data on the screen then
; disable the keyboard while we wait around for a cuple of seconds
;
; Input to userprog:  bx = char and attribute to fill screen
userprog  proc
  ; Sample program to fill the screen with '!'
  ;
  push  es      ; Load the screen up with '!'
  push  fs
  push  fs
  pop   es      ; Load ES with DDSABS
  mov   ax,0721h    ; Black & White '!'
  ;
  ; The next line accesses screen memory at the 80000000h page
  ; which is mapped to address A000, although the corresponding entry
  ; in the page table is actually 0c0000h-4096+7
    mov edi,80000000h + 18000h    ; B*00 = a000 + 18000
;   mov edi,0b8000h
    mov ecx,80*24     ; 24x80 screen
    rep stosw
    call  readSector
    pop fs
    pop es
    ;
    ; Enable interrupts and wait around a couple of seconds
    ;
    ; First, disable keyboard interrupts
    ; This way, if a key comes in we won't crash
    ;
;      in  al,21h    ;PIC interrupt control port
;      or  al,2      ; Bit to disable keyboard interrupts
;      out 21h,al    ; Do disable (without this out instruction, it crashes
;      sti
;;
;; Reenable keyboard interrupts
;  in  al,21h    ; Read PIC interrupt port
;  and al,0fdh   ; reenable keyboard
;  out 21h,al    ; Do enable
  ret
;
userprog  endp

readSector  proc
; sti
; Turn on the diskette motor
; and disable the DMA
  mov ax,14h      ;bit 3 is zero, which turns off DMA mode
  mov dx,3f2h
  out dx,ax

  ; Waiting while motor speeds up
  mov eax,18
  call tdelay

  ; recalibrate
  mov ah,7
  call  fdc_out 
  mov ah,0
  call  fdc_out 
; call  int_wait

  ;int 3
  ; We need to move drive head to the CYL track

  ; "Seek" command
  mov ah,0fh
  call  fdc_out 

  ; The "Seek" command needs 2 parameters:
  ; a Head/Drive number and a Track number.
  ; Since we're working with "A:" drive and 0 head,
  ; first parameter is 0, second parameter is CYL
  mov ah,0
  call  fdc_out 
  mov ah,CYL
  call  fdc_out 


  ; Interrupt notifies us about operation end
; call  int_wait

  ; Delay for head positioning
  mov eax,5
  call  tdelay

  ; In order to check the result of the "Seek" command
  ; we're sending "Read Interrupt State" command

  ; Displaying ST0 register and number of a track after
  ; "Seek" command execution PCN
  mov ah,8
  call  fdc_out 

  ; For more detailed info of FDC state
  ; we're sending "Read Media/Drive State" command,
  ; displaying ST3 register
  mov ah,4
  call  fdc_out 
  mov ah,0
  call  fdc_out 

  ; Setting speed of data transfer to 500 KB/sec
;  outp (0x3F7, 0);
  xor ax,ax
  mov dx,3f7h
  out dx,ax
  ; DMA initialization
; call  dma_init 
  ; "Read Data" command
  mov ah,66h
  call  fdc_out 
  mov ah,0
  call  fdc_out 

  mov ah,CYL
  call  fdc_out 
  mov ah,0
  call  fdc_out 
  mov ah,1    ;Read the first sector on cylinder 0
  call  fdc_out 

  ; The values that would normally come from the call to getDiskParmTab
  ; were discovered in a separate real mode application to avoid the 
  ; unusable BIOS int 13h call.
  ; Sending some technical info to FDC.
  ; This info may be obtained form the Diskette Parameter Table.
  ; Parameters are:
  ;    - sector size;
  ;    - last sector on a track;
  ;    - gap length;
  ;    - number of bytes to be read/write
; call  getDiskParmTab
; lea   ebx,dpt
; xor al,al
; mov ah,[ebx].sec_size
  mov ah,2h
  ;int 3
  call  fdc_out 
; mov ah,[ebx].eot
  mov ah,12h
  ;int 3
  call  fdc_out 
; mov ah,[ebx].gap_rw
  mov ah,1bh
  ;int 3
  call  fdc_out 
; mov ah,[ebx].dtl
  mov ah,0ffh
  ;int 3
  call  fdc_out 

  ; Waiting for interrupt (end of operation)
; call  int_wait
  mov   dx,3F4h
  call  fdc_inp
  mov   ax,fs
  mov   es,ax
  mov   edi,0b8000h
; mov edi,80000000h + 18000h    ; B*00 = a000 + 18000
  mov   ah,0fh
  mov   cx,4
readNext:
  push  cx
  mov   dx,3f5h
  in    al,dx
; mov   al,2ah
  mov   word ptr es:[edi],ax
  add   edi,2
  pop   cx
  loop  readNext
  mov   ecx,500000000
waitLoop:
  loop  waitLoop
; Read one byte of data from the Data Register
; call  fdc_inp
; mov   ax,1111h
; call  displaySector
; Display the first 4 bytes of the sector read
;  int 3
  ; AAA
; mov   edx,offset buffer
; mov   eax,11223344h
; sub   eax,eax
; mov   eax,dword ptr[edx]
; call  displaySector
; Turning motor off
motorOff:
   mov ax,0
   mov dx,3f2h
   out dx,ax
   ret
readSector  endp
;Writes a byte to FDC
;void fdc_out (unsigned char parm)
fdc_out proc
  mov   dx,3F4h
loop_fdc_out:

  in    al,dx
  test  al,80h      ;Is controller ready?
  jz loop_fdc_out   ;   No, waiting...

  inc   dx
  mov   al,ah    ;Writing the byte
  out   dx,al
  ret
fdc_out endp

showit  proc
  mov   ax,0b800h
  mov   es,ax
  sub   di,di
  lea   si,workarea
  mov   cx,100
  cld
  rep   movsw
  mov   ecx,500000000
toploop:
  loop  toploop
  ret
showit  endp

;Reads a byte from FDC
;int fdc_inp (void)
fdc_inp proc
  mov   dx,3F4h
loop_fdc_inp:
  in    al,dx
  test  al,80h      ;Is controller ready?
  jz loop_fdc_inp   ;  No, waiting...

  inc   dx
  in    al, dx      ;Reading a byte
  ret
fdc_inp endp

; Waits for an interrupt generated by FDC
;void int_wait (void) {
int_wait  proc
; _enable();
; while (IRQ==0) {};
  sti
int_wait_loop:
  cmp IRQ,0
  je  int_wait_loop
  mov IRQ,0
  ret
int_wait  endp

; This is the dma interrupt routine
IRQ6  proc  far
  mov IRQ,1
  mov al,20h
  out 20h,al
  iret
IRQ6  endp

; DMA initialization routine
; This function is for real mode only!!!!
; It will cause this app to crash!!!
dma_init  proc
; Computing 24-bit address for the data buffer
  push  edx
  mov   eax,ds     ; Segment which contains the data buffer
  shl   eax,4
  add   eax,offset buffer
  mov   edx,eax

  shr   eax,16
  and   eax,0ffh
  mov   sg,ax

  and   edx,0ffffh
  mov   of,dx

  pop   edx
  cli

  mov   al,46h      ;FDC read data command

  out   12,al       ; We're working with 16-bit ports.
                ; Next byte sent to 16-bit port is less significiant

  out   11,al       ; DMA mode

  mov   ax,of      ;Buffer offset LSB
  out   4,al
  mov   al,ah       ;Buffer offset MSB
  out   4,al

  mov   ax,sg      ;Page number
  out   81h,al

  mov   ax,511      ;Data length
  out   5,al
  mov   al,ah
  out   5,al

  mov   al,2        ; channel 2 enabled
  out   10,al

  ; It's now safe to enable ints
;  _enable(); // Is is function really necessary?
  sti
  ret
dma_init  endp

; This routine waits for cnt timer ticks.
; Timer frequency is 18.2 Hz
;void tdelay (int cnt)
tdelay  proc
      sti
tloop:
      cmp [timer],eax  
      jc  tloop
      cli
      mov   ecx,1000000000
tloop5:
      loop  tloop5
      ret
tdelay  endp

displaySector proc
  ; Sample program to fill the screen with '!'
  ;
  push  es      ; Load the screen up with '!'
  push  fs
  push  cx
  mov   bx,0b800h
  mov   es,bx
; mov   ax,1111h
  lea   bx,workarea
  call  htoa        ; Convert hex value in ax to ascii
  sub   di,di
  lea   si,workarea ;pt to work area where data to display is stored
  mov cx,4   ;Setup to display 8 bytes (4 ascii characters)
rep movsw
  pop cx
  pop fs
  pop es
  ret
displaySector endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converts binary value in ax 
; to ascii for display
; and returns the result in bx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 htoa  proc
   push  ebx
   push  ecx
   push  edx
   mov ecx,4
 top:
 ; Process the right nibble
   xor dh,dh
   mov dl,al
   and dl,0fh
   cmp dl,10
   jl  notLetter
   cmp dl,15
   jg  exit  ; Invalid digit
   add dl,7  ; Adjust for characters between A and F
 notLetter:
   add dl,30h
   push  dx
 ; Process the left nibble
   mov dl,al
   shr dl,4
   cmp dl,10
   jl  notLetter2
   cmp dl,15
   jg  exit  ; Invalid digit
   add dl,7  ; Adjust for A to F
 notLetter2:
   add dl,30h
   push  dx
   shr eax,8 ;prepare to process the next byte
 loop  top
   mov ecx,8
 top2:
   pop dx
   mov byte ptr[ebx],dl
   mov byte ptr[ebx+1],0fh  ;high intensity attribute
   inc ebx
   inc ebx  ;skip char and attribute
   loop  top2
 exit:
   pop edx
   pop ecx
   pop ebx
   ret
 htoa  endp

seg386  ends
    end bootstrap

