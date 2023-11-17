

.286
IDEAL
MODEL small
STACK 100h

DATASEG
black_color dw 0
SpaceShip_color dw 0ah
purple_color db 5
SpaceShipX dw 160
SpaceShipY dw 180
BulletX dw 0
BulletY dw 0
IsShooting db 0
;<Opening picture>
filename db 'test.bmp',0
filename1 db 'You_Lose.bmp',0
filename2 db 'You_Win.bmp',0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)
ErrorMsg db 'Picture not found :/', 13, 10 ,'$'
;</Openining picture>
;<First Row Enemies Data>
color db 5
;First_Row_EnemiesX dw 0,60,90,120,150,180,210,240,270

First_Row_EnemiesX dw 0,60,90,120,150,180,210,240,270
First_Row_EnemiesY dw 20
Is_Enemy_Alive db 0,1,1,1,1,1,1,1,1 ; array of "booleans" that indicates if an enemy is alive. decided to remove the first enemy for esthetics
x dw 160
y dw 80
i db 0
j db 0
;Enemy_Color_Map db 0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,1,1,0,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,1,0,0,1,0,0,0
Enemy_Color_Map db 0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,1,1,0,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,1,0,1,1,0,0,0
s dw 0
leftORRight db 0
Win db 0
EnemiesDelay dw 1000

;</First Row Enemies Data>
Clock equ es:6ch
Lost db 0; bool 0=> false 1 => true

CODESEG
;=====================================
;opening a picture
proc OpenFile
mov bp,sp
; Open file
	mov ah, 3Dh
	xor al, al
	mov dx, [bp+2] ; filename
	int 21h
	jc openerror
	mov [filehandle], ax
	ret 2
	
	openerror :
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	ret 2
endp OpenFile


proc ReadHeader
; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader

proc ReadPalette
; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette
proc CopyPal
; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB .
	mov al,[si+2] ; Get red value .
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it .
	mov al,[si+1] ; Get green value .
	shr al,2
	out dx,al ; Send it .
	mov al,[si] ; Get blue value .
	shr al,2
	out dx,al ; Send it .
	add si,4 ; Point to next color .
	; (There is a null chr. after every color.)
	loop PalLoop
	ret
endp CopyPal

proc CopyBitmap
	; BMP graphics are saved upside-down .
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop :
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	 ;rep movsb is same as the following code :
	 ;mov es:di, ds:si
	 ;inc si
	 ;inc di
	 ;dec cx
	 ;loop until cx=0
	pop cx
	loop PrintBMPLoop
	ret
endp CopyBitmap

;This proc Draws the first Row of enemies
proc Draw_First_Enemy_Row
		mov si,0
		mov di,0
	Draw_First_Enemy_Row_loop:
		cmp [Is_Enemy_Alive+di],1; checks if the enemy is dead or alive
		jne Skip_Enemy; skips if dead
		mov cx,[First_Row_EnemiesX+si];X
		mov dx,[First_Row_EnemiesY];Y
		mov ah,0ch
		mov bl,0
		mov al,[color] ;color
		;pushing di to the the stack to use it saperatly to scan Enemy_Color_Map
		push di
		;<Drawing the Enemy>
		mov di,0
		mov [i],0
		Yloop:
			mov [j],0
			mov cx,[First_Row_EnemiesX+si]
			Xloop:
					cmp [Enemy_Color_Map+di],0
					je Skip_Pixel
					int 10h
					Skip_Pixel:
					inc cx
					inc [j]
					inc di
			cmp [j],10
			jle Xloop
			inc dx
			inc [i]
			cmp [i],8
		jle Yloop
		;</Drawing the Enemy>
		;Poping di out to use it again for the Living Status of the enemy
		pop di
		Skip_Enemy:
		add si,2
		inc di
	cmp si,16
	jle Draw_First_Enemy_Row_loop
ret
endp Draw_First_Enemy_Row

;taking X pos then Y pos
; [bp + 2] - Y
; [bp + 4] - X
; [bp + 6] - SpaceShip_color / or black_color
proc DrawSpaceShip
;first pixel
	mov bp,sp
	pusha
	
	mov cx,[bp+4] ;X
	mov dx,[bp+2] ;Y

	mov ah,0ch
	mov bl,0
	mov al,[bp + 6] ;color
	int 10h
	
	;adjast pos
	inc dx
	sub cx,2
	
	;second row
	mov si,0 ;clearing si for the loop

second_row_draw:
	inc cx
	int 10h
	inc si
	cmp si,3
	jne second_row_draw

	;adjast pos
	inc dx
	add cx,2
	;third row
	mov si,0 ;clearing si for the loop
third_row_draw:
	dec cx
	int 10h
	inc si
	cmp si,5
	jne third_row_draw

	;adjast pos
	inc dx
	dec cx
	;forth row
forth_row_draw:
	inc cx
	int 10h
	dec si
	cmp si,0
	jne forth_row_draw

	;adjast pos
	inc dx
	add cx,2
	;fifth row
fifth_row_draw:
	dec cx
	int 10h
	inc si
	cmp si,7
	jne fifth_row_draw
	
	;adjast pos
	inc dx
	dec cx
	;Left booster
	mov si,0 ;clearing si for the loop
fifth:
	inc cx
	int 10h
	inc si
	cmp si,2
	jne fifth
	inc dx
	dec cx
	int 10h
	;adjast pos
	mov si,0 ;clearing si for the loop
six:
	inc cx
	inc si
	cmp si,7
	jne six
	
	;Right booster
	dec cx
	int 10h
	dec dx
	int 10h
	dec cx
	int 10h
;End of proc
	popa
ret 6
endp DrawSpaceShip



;proc that draws the buttom line
proc DrawUnderLine
mov cx, 0
mov dx, 190
mov ah,0ch
mov bl,0
mov al,0fh ;color
int 10h
loop0:
inc cx
int 10h
cmp cx,319
jne loop0
ret
endp DrawUnderLine

;proc that controls the movment
proc handle_input

WaitForData:
	in al,64h
	cmp al,10b
	je intermediate_endmovements
	in al,60h
	cmp al,4bh
	je goLeft
	cmp al,4dh
	je goRight
	cmp al,48h
	je Shooting
intermediate_endmovements:
	jmp endmovments

Shooting:
	cmp [IsShooting],1
	je endmovments
		; init shooting condition
		mov [IsShooting],1
		mov ax,[SpaceShipX]
		mov bx,[SpaceShipY]
		sub bx,2	
		mov [BulletX], ax
		mov [BulletY], bx
	jmp endmovments

goLeft:
	;boundry
	cmp [SpaceShipX],5
	jle endmovments
		;<erasing>
		push [black_color]
		push [SpaceShipX]
		push [SpaceShipY]
		call DrawSpaceShip
		;</erasing>
		;<draw new Space Ship>
		dec [SpaceShipX]
		push [SpaceShip_color]
		push [SpaceShipX]
		push [SpaceShipY]
		call DrawSpaceShip
		;</draw new Space Ship>
	jmp endmovments

goRight:
	;boundry
	cmp [SpaceShipX],314
	jge endmovments
		;<erasing>
		push [black_color]
		push [SpaceShipX]
		push [SpaceShipY]
		call DrawSpaceShip
		;</erasing>
		;<draw new Space Ship>
		inc [SpaceShipX]
		push [SpaceShip_color]
		push [SpaceShipX]
		push [SpaceShipY]
		call DrawSpaceShip
		;</draw new Space Ship>
	
endmovments:
;End of proc
	ret
endp handle_input

;proc that adds delay
proc Delay

	mov cx,11
outer_waitloop:
	push cx

	mov cx,64000
	inner_waitloop:
		loop inner_waitloop

	pop cx
	loop outer_waitloop

;end of proc
	ret
endp Delay

; =================== Shooting related procs ===================
proc Shoot
	;cmp [IsShooting],1
	;jne Finish_Shooting
	mov cx,[BulletX]
	mov dx,[BulletY]
	mov al,4;color
	mov bl,0
	mov ah,0ch
	int 10h
	;cmp dx,1
	;jg Finish_Shooting
	;mov [IsShooting],1
	;Finish_Shooting:
ret
endp Shoot

proc erase_shot
	cmp [IsShooting],1
	jne Stop_erasing_shot
	mov cx,[BulletX]
	mov dx,[BulletY]
	inc dx; 1 delay
	mov al,0;color
	mov bl,0
	mov ah,0ch
	int 10h
	cmp dx,0
	jg Stop_erasing_shot
	mov [IsShooting],0
	Stop_erasing_shot:
ret
endp erase_shot

proc Clearing_Shot
	cmp [IsShooting],1
	jne Stop_erasing_shot2
	mov cx,[BulletX]
	mov dx,[BulletY]
	
	mov al,0
	mov bl,0
	mov ah,0ch
	int 10h
	
	inc dx
	int 10h
	
	;cmp dx,0
	;jg Stop_erasing_shot2
	
	;mov [IsShooting],0
Stop_erasing_shot2:
	ret
endp Clearing_Shot
; =================== /Shooting related procs ===================

;Hitting in first Row
proc Check_For_Hit_First_Row
mov bp,sp

mov cx,[bp+4]
mov dx,[bp+2]
xor bx,bx
mov ah,0dh
dec dx
int 10h
cmp al, [purple_color]
jne EndCheck
	call Clearing_Shot

	; reset shooting status
	mov [IsShooting],0 
	
	mov [color],0
	call Draw_First_Enemy_Row
	call erase_mess
	call Find_Dead_enemy

	mov [color],5
	call Draw_First_Enemy_Row
	call erase_mess

EndCheck:
ret 4
endp Check_For_Hit_First_Row

;finding which enemy got hit
proc Find_Dead_enemy
mov di,0
mov si,0

Not_This:
mov ax,[First_Row_EnemiesX+si]
add si,2
inc di
add ax,11
cmp ax,[BulletX]
jl Not_This
dec di
mov [Is_Enemy_Alive+di],0
add di,di
mov [First_Row_EnemiesX+di],0
mov [BulletY],0

ret
endp Find_Dead_enemy

;proc that moves the enemies
proc Move_Enemies
	cmp [leftORRight],0
	je EnemiesLeft
	cmp [leftORRight],1
	je EnemiesRight

	EnemiesLeft:
	xor si, si
	loop7:
	cmp [Is_Enemy_Alive+si],1
	je end_loop7
	inc si
	jmp loop7
	end_loop7:
	cmp si,9
	jae fakeProcEnd


	add si,si
	;now Si stores the location of the MOST LEFT ENEMY ALIVE
	;saving si to be used later
	mov [s],si
	;updating pos to the left by 10
	mov [color],0
	call Draw_First_Enemy_Row
	call erase_mess
	;moving si back into si from [s]
	mov si,[s]
	cmp [First_Row_EnemiesX+si],10
	jle MovingDownFR
	mov di,si
	mov ax,18

	loop8:
	sub [First_Row_EnemiesX+di],10
	add di,2
	cmp di,ax
	jne loop8

	jmp procEnd
	MovingDownFR:
	add [First_Row_EnemiesY],10
	mov [leftORRight],1
	fakeProcEnd:
	jmp procEnd
	;=====Right=====
	EnemiesRight:
	mov si,8
	loop10:
	cmp [Is_Enemy_Alive+si],1
	je end_loop10
	dec si
	jmp loop10
	end_loop10:

	cmp si,0
	jle ProcEnd

	add si,si
	;now Si stores the location of the MOST Right ENEMY ALIVE
	;saving si to be used later
	mov [s],si
	;updating pos to the Right by 10
	mov [color],0
	call Draw_First_Enemy_Row
	call erase_mess
	;moving si back into si from [s]
	mov si,[s]
	cmp [First_Row_EnemiesX+si],300
	jae MovingDownFR_
	mov di,si
	loop11:
	add [First_Row_EnemiesX+di],10
	sub di,2
	cmp di,0
	jne loop11

	jmp procEnd
	MovingDownFR_:
	add [First_Row_EnemiesY],10
	mov [leftORRight],0
	;==
	ProcEnd:
	mov [color],5
	call Draw_First_Enemy_Row
	call erase_mess
ret
endp Move_Enemies

proc erase_mess
	xor bh,bh
	mov ax,[First_Row_EnemiesY]
	add ax, 8
	mov cx,0 ;X
	mov dx,ax ;Y
	mov ah,0ch
	mov bl,0
	mov al,0  ;color (black)
	cleanMess:
	int 10h
	inc cx
	cmp cx,320
	jle cleanMess

ret
endp erase_mess

proc LosingCondition

cmp [First_Row_EnemiesY],165
jbe DidntLose
mov [Lost],1
DidntLose:

ret
endp LosingCondition

;this proc created the yellow lines that indicates when you lose
proc YellowIndicators
add ax, 8
mov cx,0 ;X
mov dx,175 ;Y
mov ah,0ch
mov bl,0
mov al,0eh ;color
;need to inc cx twice
xor si,si 
yellowLeftLoop:
	int 10h
	inc cx
	inc si
cmp si,2
jl yellowLeftLoop

mov cx,319
xor si,si 
yellowRightLoop:
	int 10h
	dec cx
	inc si
cmp si,2
jl yellowRightLoop

ret
endp YellowIndicators

;this proc scans Is_Enemy_Alive to make sure its all 0
proc DidYouWin
	mov [win],1
	mov si,0
	scanEnemies:
		
		cmp [Is_Enemy_Alive+si],1
		jne dontupdate
		mov [win],0
		dontupdate:
		inc si
		cmp si,9
	jl scanEnemies	

ret
endp DidYouWin

;this proc will reload all the enemy data for the game reset
proc ReloadEnemiesData
mov [lost],0 ; because we didnt lose
mov [First_Row_EnemiesY],20;moving the enemies to the start location
;restarting enemies X location
mov si,2
mov ax,60
resPosLoop:
	mov [First_Row_EnemiesX+si],ax
	add si,2
	add ax,30
	cmp si,16
jle resPosLoop

;reserecting the enemies
mov si,1
resLoop:
	mov[Is_Enemy_Alive+si],1
	inc si
	cmp si,9
jl resLoop
;restaring shooting positions
mov [BulletX],0
mov [BulletY],0

ret
endp ReloadEnemiesData

;==========================
start:

    mov ax, @data
    mov ds, ax
;======Main======
call ReloadEnemiesData
;<grapics mode>
mov ax,13h
int 10h
;</grapics mode>

;<picture>
xor ax,ax
push offset filename
call OpenFile
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap
Nothingpressed:
    mov ah, 1
    int 16h
    jz Nothingpressed
   
    mov ah, 0
    int 16h
   
    cmp ah, 1Ch
    je chck
    jmp Nothingpressed
   
    chck:
;<grapics mode>
mov ax,13h
int 10h
;</grapics mode>
;</picture>
call DrawUnderLine ; draws the Under Line
mov [color], 5 ;selecting enemy color
call Draw_First_Enemy_Row;drawing the enemies
call erase_mess
call YellowIndicators
;<Game loop>
GAME:
	call Delay
	call handle_input

	;cheking if the payer lose
	call LosingCondition
	cmp [lost],1
	je GameLost

	cmp [IsShooting],1
	jne cont
		call Shoot
		call erase_shot

cont:
	push [BulletX]
	push [BulletY]
	call Check_For_Hit_First_Row
	;cheking if the payer won
	call DidYouWin
	cmp [win],1
	je GameWon
	
		cmp [IsShooting],1
		jne DontUpdateY
			dec [BulletY]

DontUpdateY:
	;<moving the enemies & delay>
	mov dx, 0
	mov ax,[EnemiesDelay]
	mov bx,50 ;every 50 times
	div bx
	cmp dx,0
	jne skipEnemyMove

		call Move_Enemies ; moving the enemies

skipEnemyMove:
	dec [EnemiesDelay]
	;</moving the enemies & delay>
jmp GAME
	;</Game loop>
GameWon:
jmp skipjump ;bruh moment
fakeStart: 
jmp start
skipjump:
;what happends if the player won
;<grapics mode>
mov ax,13h
int 10h
;</grapics mode>
;<picture>
push offset filename2
call OpenFile
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap
;</picture>
xor ax,ax
Nothingpressed_:
    mov ah, 1
    int 16h
    jz Nothingpressed_
   
    mov ah, 0
    int 16h
   
    cmp ah, 1Ch
    je fakeStart
	cmp ah,1
	je GameOVER
    jmp Nothingpressed_
   
jmp GameOVER

GameLost:
;what happends if the player loses
;<grapics mode>
mov ax,13h
int 10h
;</grapics mode>
;<picture>
push offset filename1
call OpenFile
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap
;</picture>
xor ax,ax
Nothingpressed2:
    mov ah, 1
    int 16h
    jz Nothingpressed2
   
    mov ah, 0
    int 16h
   
    cmp ah, 1Ch
    je fakeStart
	cmp ah,1
	je GameOVER
    jmp Nothingpressed2
   
jmp GameOVER

GameOVER:
;==============
exit:
    mov ax, 4c00h
    int 21h
END start
