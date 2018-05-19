TITLE Program 5a         (program5a.asm)

; Author: Chelsea Egan
; CS271-400               Date: 8/13/17
; Description: This program prompts the user for 10 integers, reads them in
; as a string, validates their input and converts to digits, converts the
; digits back to strings and displays them to the user, then calculates and 
; displays the sum and average of those digits.

INCLUDE Irvine32.inc

;-------------------------------------------------------------------------
; CONSTANTS
;-------------------------------------------------------------------------
; Size of array and amount of digits entered by user
ARRAY_SIZE = 10

; Size of buffer used to read in digits
BUFFER_SIZE = 256

; ASCII code for enter key
ENTER_KEY = 13

;-------------------------------------------------------------------------
; MACROS
;-------------------------------------------------------------------------
displayString  MACRO digitString:REQ
; Receives a string as a parameter and prints it to the screen
; Receives: offset of a string
; Returns: n/a
; Preconditions: must have a string passed to it
; Registers changed: none
;-------------------------------------------------------------------------
     ; Save registers
     push edx
     push eax

     ; Write string
     mov  edx, digitString
     call WriteString

     ; Restore registers
     pop  eax
     pop  edx
ENDM

;-------------------------------------------------------------------------
getString MACRO prompt:REQ, buffer:REQ
; Prints prompt to screen and reads input
; Receives: offset of a string, offset of buffer to read in input
; Returns: User input stored in EAX
; Preconditions: Offset of string and buffer passed to it
; Registers changed: EAX
;-------------------------------------------------------------------------
     ; Save registers
     push ecx
     push edx

     ; Display prompt
     displayString prompt

     ; Set up buffer for ReadString
     mov  edx, buffer
     mov  ecx, BUFFER_SIZE

     ; Read input
     call ReadString
     call Crlf
     call Crlf

     ; Restore registers
     pop  edx
     pop  ecx
ENDM



.data
;-------------------------------------------------------------------------
; TEXT
;-------------------------------------------------------------------------
programTitle   BYTE "Program 5A",0
myName         BYTE "by Chelsea Egan",0
instructions   BYTE "Please provide 10 unsigned decimal integers.",0dh, 0ah
               BYTE "Each number needs to be small enough to fit "
               BYTE "inside a 32 bit register.",0dh, 0ah
               BYTE "After you have finished inputting the raw "
               BYTE "numbers I will display a list",0dh, 0ah
               BYTE "of the integers, their sum, and their average value.",0              
numPrompt      BYTE "Please enter an unsigned number: ",0
errorMsg       BYTE "ERROR: You did not enter an unsigned number or your "
               BYTE "number was too big.",0
displayMsg     BYTE "You entered the following numbers:",0
sumMsg         BYTE "The sum of these numbers is: ",0
averageMsg     BYTE "The average of these numbers is: ",0
farewell       BYTE "Have a great day!",0

;-------------------------------------------------------------------------
; VARIABLES
;-------------------------------------------------------------------------
digitArray     DWORD ARRAY_SIZE DUP(?)  ; Array to hold user-defined digits
sumOfDigits    DWORD ?                  ; Sum of all digits in arrays
stringBuffer   BYTE BUFFER_SIZE DUP (0) ; Buffer to hold user input

.code
main PROC

     ; Push offset of strings for introduction
     push OFFSET programTitle
     push OFFSET myName
     push OFFSET instructions

     ; Introduction
     call introduction

     ; Push data needed for readVal function
     push OFFSET numPrompt
     push OFFSET errorMsg
     push OFFSET digitArray
     push OFFSET stringBuffer

     ; Get digits
     call readVal

     ; Push data needed for writeVal function
     push OFFSET displayMsg
     push OFFSET digitArray
     push OFFSET stringBuffer

     ; Display digits
     call writeVal

     ; Push data needed for sum function
     push OFFSET stringBuffer
     push OFFSET sumMsg
     push OFFSET digitArray
     push OFFSET sumOfDigits
     
     ; Sum
     call calculateSum

     ; Push data needed for average function
     push OFFSET stringBuffer
     push OFFSET averageMsg
     push sumOfDigits

     ; Average
     call calculateAverage

     ; Push data needed for farewell function
     push OFFSET farewell

     ; Farewell
     call sayGoodbye

	exit	; exit to operating system
main ENDP

;-------------------------------------------------------------------------
     introduction   PROC
; Prints the program title, author's name, and description of program
; Receives: Offsets of strings
; Returns: nothing
; Preconditions: strings pushed on stack
; Registers changed: none
;-------------------------------------------------------------------------
          push ebp
          mov  ebp, esp
          pushad                        ; Preserve registers

          displayString  [ebp+16]       ; Program 5a
          mov  al, TAB
          call WriteChar
          displayString [ebp+12]        ; by Chelsea Egan
          call Crlf
          call Crlf
          displayString [ebp+8]         ; Description
          call Crlf
          call Crlf

          ; Clear screen
          call WaitMsg
          call ClrScr

          popad                         ; Restore registers
          pop  ebp

          ret 12                        ; Clean up stack
     introduction   ENDP

;-------------------------------------------------------------------------
     promptParam    EQU [ebp+20]
     errorParam     EQU [ebp+16]
     arrayParam     EQU [ebp+12]
     bufferParam    EQU [ebp+8]
     readVal PROC
; Gets 10 digits from user, reading them in as a string, converting them
; to digits, and validates
; Receives: Offset of prompt string, error string, digitArray, and buffer
; Returns: nothing
; Preconditions: Messages, array, and buffer pushed on stack
; Registers changed: none
;-------------------------------------------------------------------------
          ; Preserve registers
          push ebp
          mov  ebp, esp
          pushad

          mov  ecx, ARRAY_SIZE               ; Counter=10
          mov  esi, arrayParam               ; Offset of digitArray
     ArrayFillLoop:                          ; Loop until receive 10 digits
          PromptLoop:                        ; Loop until validated digit
               getString [ebp+20], [ebp+8]   ; Prompt user for input
               ; Push buffer and digitArray for convertDigit function
               push bufferParam             
               ; Call function to convert string to digit and validate
               call convertDigit             
               cmp  eax, -1                  ; If eax=-1, invalid
               jne  ValidNum
               displayString errorParam      ; Print error message
               call Crlf
               call Crlf
               jmp  PromptLoop               ; Loop for a new digit
          ; If received a valid digit
          ValidNum:
               mov  [esi], eax               ; Store in array
               add  esi, TYPE DWORD          ; Move to next element in array
               loop ArrayFillLoop            ; Loop until 10 digits

          ; Restore registers
          popad
          pop  ebp

          ret  16                            ; Clean up stack
     readVal ENDP

;-------------------------------------------------------------------------
     stringParam    EQU [ebp+8]
     convertDigit   PROC
; Validates that string is a digit then converts from ASCII
; Receives: Offset of buffer
; Returns: Digit in EAX (-1 if invalid input)
; Preconditions: Buffer pushed on stack
; Registers changed: EAX
;-------------------------------------------------------------------------
          ; Preserve registers
          push ebp
          mov  ebp, esp
          push ebx
          push ecx
          push edx
          push esi

          mov  esi, stringParam    ; Offset of string buffer
          mov  ebx, 0              ; Total result

          xor  eax, eax            ; Zero out EAX

          lodsb                    ; Get first byte
          cmp  ax, 0               ; Check if empty
          je   InvalidInput
          jne  NextNumber          ; Not empty, process
     ConversionLoop:               ; Loop through string
          lodsb                    ; Get next byte
          cmp  eax, 0              ; Check if null
          je   EndOfNumber         ; If null, end of string
     NextNumber:
          cmp  eax, ENTER_KEY      ; Check if user pressed enter
          je   EndOfNumber 
          cmp  eax, '0'            ; Check if in ASCII range
          jl   InvalidInput
          cmp  eax, '9'
          jg   InvalidInput
          sub  eax, '0'            ; Convert character to number
          mov  ecx, eax
          mov  eax, ebx
          mov  edx, 10
          mul  edx                 ; Multiply running total by 10
          cmp  edx, 0              ; Check if overflow into EDX
          jne  InvalidInput        ; If overflow - digit is too big
          mov  ebx, eax
          add  ebx, ecx            ; Add digit to final result

          xor eax, eax             ; Reset EAX

          jmp ConversionLoop

     EndOfNumber:
          mov  eax, ebx            ; Store digit in EAX
          call validateNum         ; Validate digit
          jmp  Quit

     InvalidInput:
          mov  eax, -1             ; Set eax=-1 to indicate invalid input

     Quit:
          ; Restore registers
          pop  esi
          pop  edx
          pop  ecx
          pop  ebx
          pop  ebp
          ret 4                    ; Clean up stack
     convertDigit   ENDP

;-------------------------------------------------------------------------
     validateNum    PROC
; Verifies that the user-defined integer is in range; returns -1 if not in
; range or returns the valid integer
; Receives: integer stored in EAX
; Returns: integer stored in EAX
; Preconditions: an integer from the user must have been stored in EAX
; Registers changed: EAX
;-------------------------------------------------------------------------
          ; Check for overflow flag
	     jo	Invalid

	     ; Check for negative integer
          cmp  eax, 0
          jl   Invalid

          ; In range
          jmp  Valid

     ; Out of range
     Invalid:
          mov  eax, -1                   ; Set eax=-1 to indicate error

     Valid:
          ret
     validateNum    ENDP

;-------------------------------------------------------------------------
     writeVal  PROC
; Loop through digitArray and print each digit
; Receives: Offset of message, array, string buffer
; Returns: n/a
; Preconditions: Offsets must be pushed on stack
; Registers changed: n/a
;-------------------------------------------------------------------------
          ; Preserve registers
          push ebp
          mov  ebp,esp
          pushad

          displayString [ebp+16]        ; Print title of list
          call Crlf

        
          mov  ecx, ARRAY_SIZE          ; Counter = 10
          mov  esi, [ebp+12]            ; Digit array
          

     StringLoop:                        ; Loop through array
          push [ebp+8]                  ; Push string buffer
          push [esi]                    ; Push digit array

          call DisplayNumber            ; Convert digit to string and print
          mov  al, TAB
          call WriteChar
          add  esi, TYPE DWORD          ; MOve to next digit
          loop StringLoop

          call Crlf
          call Crlf

          ; Restore registers
          popad
          pop  ebp
          ret  12                       ; Clean up stack
     writeVal  ENDP

;-------------------------------------------------------------------------
     DisplayNumber  PROC
; Loop through digit, convert to string, and display
; Receives: Offset of digit array and string buffer
; Returns: n/a
; Preconditions: Offsets must be pushed on stack
; Registers changed: n/a
;-------------------------------------------------------------------------
          ; Preserve registers
          push ebp
          mov  ebp, esp
          pushad

          mov  edi, [ebp+12]            ; String buffer
          add  edi, BUFFER_SIZE-1       ; Move to end of buffer

          std                           ; Set direction flag
          mov  eax, [ebp+8]             ; Get digit
          DigitLoop:
               cdq                      ; Divide digit by 10
               mov  ebx, 10
               div  ebx
               cmp  eax, 0              ; If quotient = 0
               je   NextDigit
               add  edx, 48             ; Else, convert remainder to ASCII
               mov  ebx, eax            ; Preserve quotient
               mov  eax, edx            ; Move remainder to EAX
               stosb                    ; Store in string
               mov  eax, ebx            ; Restore quotient
               jmp  DigitLoop           ; Move to next number in digit
          NextDigit:
               mov  eax, edx            ; Get remainder
               add  eax, 48             ; Covert to ASCII
               stosb                    ; Store in string
               inc  edi                 ; Move back one space in buffer
               displayString edi        ; Print string

          ; Restore registers
          popad
          pop  ebp
          ret  8                        ; Clean up stack
     DisplayNumber  ENDP

;-------------------------------------------------------------------------
     calculateSum   PROC
; Loop through digit array, summing all digits together and displaying
; Receives: Offset of digit array, string buffer, message, and variable to
; hold sum
; Returns: n/a
; Preconditions: Offsets must be pushed on stack
; Registers changed: n/a
;-------------------------------------------------------------------------
          ; Preserve registers
          push ebp
          mov  ebp, esp
          pushad

          mov  esi, [ebp+12]       ; Digit array
          mov  eax, 0              ; Sum - initalized to zero
          mov  ecx, ARRAY_SIZE     ; Counter = 10

     SumLoop:                      ; Loop through array
          add  eax, [esi]          ; Add next digit to total sum
          add  esi, TYPE DWORD     ; Move to next digit
          loop SumLoop

          displayString [ebp+16]   ; Print title of sum display

          mov  edi, [ebp+8]        ; Variable to hold total sum
          mov  [edi], eax          ; Store total sum

          push [ebp+20]            ; Push string buffer on stack
          push eax                 ; Push total sum on stack

          call DisplayNumber       ; Print sum

          call Crlf
          call Crlf

          ; Restore registers
          popad
          pop  ebp  
          ret  16                  ; Clean up stack
     calculateSum   ENDP

;-------------------------------------------------------------------------
     calculateAverage    PROC
; Divide sum by size of array to get average and display
; Receives: Total sum, offset of string buffer, and offset of message
; Returns: n/a
; Preconditions: Offsets and sum must be pushed on stack
; Registers changed: n/a
;-------------------------------------------------------------------------
          ; Preserve registers
          push ebp
          mov  ebp, esp
          pushad

          mov  eax, [ebp+8]        ; Sum of digits
          cdq
          mov  ebx, ARRAY_SIZE     ; 10
          div  ebx                 ; Divie sum by size of array

          displayString [ebp+12]   ; Print messsage about average

          push [ebp+16]            ; Push string buffer
          push eax                 ; Push average

          call DisplayNumber       ; Display average

          call Crlf
          call Crlf

          ; Restore registers
          popad
          pop  ebp
          ret  12                  ; Clean up stack
     calculateAverage    ENDP

;-------------------------------------------------------------------------
     sayGoodbye     PROC
; Prints farewell statement to screen and waits for user to enter a key
; before exiting program
; Receives: Offset of farewell message
; Returns: none
; Preconditions: Offset of message pushed on stack
; Registers changed: n/a
;-------------------------------------------------------------------------
          ; Preserve registers
          push ebp
          mov  ebp, esp
          pushad

          displayString [ebp+8]    ; Print message
          call Crlf
          call Crlf

          ; Wait for user to press key before exiting
          call WaitMsg
          call Clrscr

          ; Restore registers
          popad
          pop  ebp
          ret  4                   ; Clean up stack
     sayGoodbye     ENDP

END main
