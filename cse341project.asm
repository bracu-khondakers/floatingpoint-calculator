;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;                   FLOATING-POINT CALCULATOR                     ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
include 'emu8086.inc'

size_buf = 101  

data segment 
    ; the buffer for infix expression
    in_buf db size_buf dup (?)
    ; the buffer for the converted postfix list    
    postfixList db size_buf*2 dup (?)
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
    ; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax

    ; main code starts from here
    
    ; print intro to the calculator, asking for input
    call pthis
    db 'WELCOME TO FLOATING-POINT CALCULATOR!', 13, 10
    db 'Please enter an expression', 0
     
    call pthis
    db 13, 10, '> ', 0
    
    ; take input expression in infix form
    lea di, in_buf
    mov dx, size_buf
    call get_string
    
    ; print newline
    call pthis
    db 13, 10, 0 
               
    ; convert the infix expression into postfix
    lea si, postfixList
    push si                             ; load params
    lea si, in_buf
    push si
    call inToPost
    add sp, 4                           ; clean stack
    
    ; evaluate the postfix expression
    lea si, postfixList
    push si
    call postfixEval
    add sp, 2 
    
    call pthis
    db '= ', 0
    
    ; print the result
    push ax
    call printNum
    add sp, 2 
                       
    mov ax, 4c00h                       ; exit to operating system.
    int 21h    
ends
     
     
; inToPost(inaddr, outaddr)
; input:
; inaddr => the input buffer for infix expression 
; outaddr => the output buffer for postfix expression 
; output: 
; null
; comments:
; the postfix expression will be stored in the output buffer.
; data structure for the postfix expression:
; every token will consist of 3 bytes
; first word(1st and 2nd byte) is the number or operator
; the 3rd byte is the type of token it is
; if it is an operator, 3rd byte should be 1
; if it is an operand, 3rd byte should be 2
; if one such 'token' (3 bytes) is null, the expression has ended
inToPost:
    ; some constants
    isOperator = 1
    isOperand = 2
    ; parameters
    inToPost_inaddr = 4 
    inToPost_outaddr = 6
    ; local variables
    stack_empty = -2
    ; create stack frame
    push bp
    mov bp, sp
    sub sp, 2                           ; keep room for local variables
    ; backup registers 
    push dx
    push cx 
    push bx
    push ax
    push di
    push si
    
    mov [bp+stack_empty], sp            ; mark the position of sp when opStack is empty      
    
    ; si = index in infix expression
    ; di = index in postfix expression
    mov si, [bp+inToPost_inaddr]
    mov di, [bp+inToPost_outaddr]
    
    ; to detect if it's a number,
    ; check if first byte is a digit. (I know it's lame)  
    ; if number, grab the number and append it to postfixList
    inToPost_nextToken:
    cmp [si], '0'
    jl  inToPostNotNum
    cmp [si], '9'
    jg  inToPostNotNum  
    push si
    call nextNum                        ; convert the token to a number
    add sp, 2  
    mov [di], ax                        ; store the number as a word
    mov [di+2], isOperand               ; mark that it is an operand
    add di, 3                           ; move on to next index
    jmp inToPost_consumed               ; token consumed (Y)
                                                
    inToPostNotNum:             
    cmp [si], '('               
    jne inToPostNotOpenPar      
    mov dx, [si]
    xor dh, dh
    push dx                             ; if current token is '(', push it into opStack
    jmp inToPost_consumed               ; token consumed (Y) 
    
    inToPostNotOpenPar:
    mov dx, [si]
    xor dh, dh
    cmp dx, ')'               
    jne inToPostNotClosePar
    pop bx                              ; if current token is ')', pop opStack
    ; while popped token != '(' 
    whileNotOpenPar:
    cmp bx, '('
    je  inToPost_consumed               ; token consumed (Y) 
    mov [di], bx
    mov [di+2], isOperator    
    add di, 3
    pop bx                              ; pop token from opStack
    jmp whileNotOpenPar
    
    inToPostNotClosePar:
    whileNotEmptyAndPrec:
    cmp [bp+stack_empty], sp            ; check if stack is empty
    je  inToPost_justPushToken  
    mov bx, sp
    mov bx, ss:[bx]                     ; peek token from opStack
    push bx
    call checkPrec                      ; get precedence of peeked token
    add sp, 2
    mov cx, ax
    mov dx, [si]
    xor dh, dh
    push dx                   
    call checkPrec                      ; get precedence of current token
    add sp, 2 
    ; if stack is not empty AND 
    ; if precedence of peeked token >= precedence of current token,
    cmp cx, ax                  
    jl  inToPost_justPushToken 
    pop bx                              ; pop token from opStack 
    mov [di], bx                        ; append popped token into postfixList
    mov [di+2], isOperator    
    add di, 3
    jmp whileNotEmptyAndPrec
    inToPost_justPushToken:
    mov dx, [si]
    xor dh, dh
    push dx                             ; push current token into opStack 
    jmp inToPost_consumed               ; token consumed (Y)
    
    inToPost_consumed:
    add si, 2                           ; move to next token   
    cmp [si], 0  
    ; parse next token if end of infix expression is not reached                
    jne inToPost_nextToken      
    
    ; while opStack is not empty
    whileOpStackNotEmpty:
    cmp [bp+stack_empty], sp
    je inToPost_processed               ; if it's empty, we are done parsing
    ; else, pop token from opStack and append it to postfixList
    pop bx
    mov [di], bx                
    mov [di+2], isOperator    
    add di, 3 
    jmp whileOpStackNotEmpty
    
    ; if done parsing, append 3 null bytes to terminate the postfix expression
    inToPost_processed:
    mov word[di], 0
    mov [di+2], 0 
    
    ; restore registers
    pop si 
    pop di 
    pop ax
    pop bx
    pop cx 
    pop dx
    add sp, 2
    pop bp
    ret              
    
    
; postfixEval(addr)
; input:
; addr => address of the postfixList
; output:
; ax => result of the calculation
; operation:
; evaluates a postfix expression  
; comments:
; the postfix expression must be a data structure as specified by inToPost 
postfixEval:
    ; some constants
    isOperator = 1
    isOperand = 2 
    ; parameters
    postfixEval_addr = 4
    push bp
    mov bp, sp
    push bx
    push si
    
    mov si, [bp + postfixEval_addr] 
    
    postfixEval_nextToken:
    cmp [si+2], isOperand               ; check if token is an operand
    jne postfixEval_notOperand
    push [si]                           ; push current token into operandStack
    jmp postfixEval_consumed            ; token consumed (Y)
    postfixEval_notOperand:
    pop ax                              ; pop operand2 from operandStack
    pop bx                              ; pop operand1 from operandStack
    push ax                             ; load params for calc procedures
    push bx
    cmp [si], '*'                       ; check which operand current token is
    je  postfixEval_mul      
    cmp [si], '/'
    je  postfixEval_div
    cmp [si], '+'
    je  postfixEval_add
    cmp [si], '-'
    je  postfixEval_sub
    
    postfixEval_mul:                    ; call the corresponding procedure based on the operator
    call calcMul            
    jmp postfixEval_evaluated
    postfixEval_div:    
    call calcDiv            
    jmp postfixEval_evaluated    
    postfixEval_add:
    call calcAdd            
    jmp postfixEval_evaluated    
    postfixEval_sub:
    call calcSub            
    jmp postfixEval_evaluated
    
    postfixEval_evaluated:
    add sp, 4                           ; clean stack
    push ax                             ; push evaluated result back into operandStack
    
    postfixEval_consumed:
    add si, 3
    cmp word[si], 0
    jne postfixEval_nextToken
    cmp [si+2], 0
    jne postfixEval_nextToken
     
    pop ax                              ; pop final result into ax
    
    pop si
    pop bx
    pop bp
    ret


; nextNum(addr)
; input:
; addr => the address of the number
; output:
; ax => the number returned  
; si => address of the next index
; operation:
; converts the next number in string form from user's expression 
; into its binary form
nextNum:  
    nextNum_addr = 4 
    ten = -2
    tens = -4
    push bp
    mov bp, sp 
    ; declare local variables
    sub sp, 4
    mov word[bp + ten], 10              ; a constant representing 10
    mov word[bp + tens], 1              ; used as a multiplier
    push bx     
    push cx
    push dx
    push di 
    
    xor bx, bx 
    mov si, [bp+nextNum_addr]
    dec si
    
    isNotSpace:                         ; increment si till a space is found
    inc si
    cmp [si], ' '
    je isSpace
    cmp [si], 0                         ; because there is a null byte in the end of the infix expression
    jne isNotSpace
    
    isSpace:
    dec si        
    mov di, si                          ; keep a backup of the last index
    xor cx, cx                          ; the converted number
    xor dx, dx                          ;;;; TODO - detect too big number using dx
    
    ; build up the number from right to left
    nextNum_nextDigit: 
    xor ax, ax
    mov al, byte[si]  
    cmp al, '.'                         ; check if it's a decimal point
    jne nextNum_decimalPointNotFound
    mov bx, cx                          ; if it's a decimal point, backup the current sum to bx
    xor cx, cx                          ; zero out the sum
    mov word[bp+tens], 1                ; restore the factor back to 1
    jmp nextNum_advanceIndex
    nextNum_decimalPointNotFound:
    sub al, '0'                         ; grab the digit and 'de-ascii-fy'
    mul word[bp+tens]                   ; multiply by factors of ten
    add cx, ax                          ; build up the number
    mov ax, word[bp+tens]
    mul word[bp+ten]                    ; multiply the factor by ten
    mov word[bp+tens], ax               ; save back the new factor of ten 
    nextNum_advanceIndex:
    dec si
    cmp si, [bp+nextNum_addr]
    jge nextNum_nextDigit               ; if conversion not completed, grab next digit  
    
    push bx
    call fixFrac                        ; al will contain the fraction part in 2 dp
    add sp, 2
               
    mov ah, cl                          ; copy the number before decimal point to ah
    mov si, di                          ; copy the next address to si
            
    pop di 
    pop dx
    pop cx
    pop bx
    add sp, 4
    pop bp
    ret
       
       
; printNum(num)
; input:
; num => the number to be printed
; output:
; null
; operation:
; prints the number given in 2 decimal places
printNum:  
    ; parameter
    printNum_num = 4
    ; local variable
    mark_empty = -2    
    push bp
    mov bp, sp
    sub sp, 2    
    push dx
    push cx
    push bx  
    
    mov [bp+mark_empty], sp             ; mark where the stack is empty
    mov bl, 10                          ; the base of the number to be printed
    
    mov ax, [bp+printNum_num]
    mov al, ah                          ; get the number before decimal point
    ; the following bit hack gets the absolute value of a number
    ; this was apparently 'patented' by some **** working at Sun Microsystems
    ; not really needed as we are not handling negative numbers :(
    cbw                      
    xor al, ah
    sub al, ah
   
    printNum_parseBeforeDecimal:
    xor ah, ah
    div bl                              ; divide by 10 to get least significant digit
    xor cx, cx 
    mov cl, ah 
    push cx                             ; push the digit into stack (to be reverse printed later)
    cmp al, 0
    jne printNum_parseBeforeDecimal     ; keep grabbing more digits until quotient is 0
    
    printNum_printBeforeDecimal:
    cmp [bp+mark_empty], sp
    je printNum_beforeDecimalParsed
    pop dx
    add dl, 30h                         ; 'ascii-fy' the digit
    mov ah, 2                    
    int 21h                             ; print the digit
    jmp printNum_printBeforeDecimal   
    
    printNum_beforeDecimalParsed:
    mov dl, '.'
    int 21h
    mov ax, [bp+printNum_num]
    cbw                          
    xor al, ah
    sub al, ah
    
    printNum_parseAfterDecimal:
    xor ah, ah
    div bl
    xor cx, cx            
    mov cl, ah 
    push cx 
    cmp al, 0
    jne printNum_parseAfterDecimal      ; keep grabbing more digits until quotient is 0 
    
    printNum_printAfterDecimal:
    cmp [bp+mark_empty], sp
    je  printNum_printed
    pop dx
    add dl, 30h
    mov ah, 2
    int 21h  
    jmp printNum_printAfterDecimal
    
    
    printNum_printed:
    pop bx
    pop cx
    pop dx
    add sp, 2
    pop bp
    
    ret         
                      

; fixFrac(num)
; input:
; num => the fraction to be fixed
; output:
; ax => the fixed fraction
; operation:
; 'fixes' a fraction to 2 decimal places    
fixFrac:
    fixFrac_num = 4
    push bp
    mov bp, sp
    push bx
    
    xor dx, dx
    mov bx, 10 
    
    mov ax, [bp+fixFrac_num]
    cmp ax, 100
    jae fixFrac_reduce
    cmp ax, 10
    jae fixFrac_fixed
    mul bx
    jmp fixFrac_fixed
    
    fixFrac_reduce:
    div bx
    cmp dx, 5
    jl fixFrac_skipCarry
    inc ax
    fixFrac_skipCarry:
    mov dx, 0
    cmp ax, 100
    jae fixFrac_reduce               
                  
    fixFrac_fixed:
    
    pop bx
    pop bp
    ret 


; checkPrec(op)
; input:
; op => the operator to check precedence of
; output:
; ax => the precedence of the operator  
; operation:
; checks precedence of the given operator
; precedence is marked as follows: 
; */ = 3
; +- = 2
; ( = 1
checkPrec:
    checkPrec_op = 4
    push bp
    mov bp, sp
    
    mov ax, [bp+checkPrec_op] 
    cmp ax, '*'
    je  checkPrec_three
    cmp ax, '/'
    je  checkPrec_three
    cmp ax, '+'
    je  checkPrec_two
    cmp ax, '-'
    je  checkPrec_two
    cmp ax, '('
    je  checkPrec_one
    
    checkPrec_three:
    mov ax, 3
    jmp checkPrec_done
    
    checkPrec_two:
    mov ax, 2
    jmp checkPrec_done    
    
    checkPrec_one:
    mov ax, 1
    
    checkPrec_done:
    
    pop bp
    ret 


; calcAdd(op1, op2)
; input:
; op1 => operand1
; op2 => operand2
; output:
; ax => result of the operation
; operation:
; adds operand1 and operand2   
calcAdd:
    calcAdd_op1 = 4
    calcAdd_op2 = 6
    push bp
    mov bp, sp
    push bx
    
    mov ax, [bp+calcAdd_op1]
    mov bx, [bp+calcAdd_op2]
    
    add al, bl                          ; add the parts after decimal point
    cmp al, 100
    jb calcAdd_noCarry
    sub al, 100                     
    inc bh                              ; if there is a carry, add 1 to the part before decimal point
    calcAdd_noCarry:
    add ah, bh                          ; add the parts before decimal point 
    
    pop bx
    pop bp
    ret         
    

; calcSub(op1, op2)
; input:
; op1 => operand1
; op2 => operand2
; output:
; ax => result of the operation
; operation:
; subtracts operand2 from operand1    
calcSub:
    calcSub_op1 = 4
    calcSub_op2 = 6
    push bp
    mov bp, sp
    push bx     
    
    mov ax, [bp+calcSub_op1]
    mov bx, [bp+calcSub_op2]  
                
    cmp al, bl                          ; check if first operand is smaller than second operand
    jnc calcSub_noBorrow    
    add al, 100 
    inc bh                              ; if there is a borrow, add 1 to the part before decimal point in operand 2
    calcSub_noBorrow:  
    sub al, bl                          ; subtract the parts after decimal point
    sub ah, bh                          ; subtract the parts before decimal point
    
    pop bx
    pop bp
    ret
         
         
; calcMul(op1, op2)
; input:
; op1 => operand1
; op2 => operand2
; output:
; ax => result of the operation
; operation:
; multiplies operand1 by operand2    
calcMul:
    calcMul_op1 = 4
    calcMul_op2 = 6
    push bp
    mov bp, sp
    push bx
    push dx
    push cx
    
    mov bl, 100
    
    ; get the value of operand 1 without considering the decimal point                         
    mov ax, [bp+calcMul_op1]
    mov al, ah
    mov ah, 0
    mul bl                              
    mov cx, [bp+calcMul_op1]
    mov ch, 0
    add ax, cx
    mov dx, ax 
    
    ; get the value of operand 2 without considering the decimal point
    mov ax, [bp+calcMul_op2]
    mov al, ah
    mov ah, 0
    mul bl
    mov cx, [bp+calcMul_op2]
    mov ch, 0
    add ax, cx
    
    ; separate the parts before and after decimal point
    mov bx, dx
    mul bx
    mov bx, 10000
    div bx
    
    ; truncate the part after decimal point to 2 dp
    mov cx, ax
    push dx
    call fixFrac
    add sp, 2
                 
    mov ah, cl
    
    pop cx
    pop dx
    pop bx
    pop bp
    ret      
    

; calcDiv(op1, op2)
; input:
; op1 => operand1
; op2 => operand2
; output:
; ax => result of the operation
; operation:
; divides operand1 by operand2  (truncates the result to 1 dp)  
calcDiv:
    calcDiv_op1 = 4
    calcDiv_op2 = 6
    push bp
    mov bp, sp
    push dx
    push bx  
    
    xor dx, dx
    
    mov ax, [bp+calcDiv_op1]
    mov bx, [bp+calcDiv_op2] 
    
    div bx 
    
    push ax                             ; Quotient in AX    
    xor dx, dx 
    mov dl, al
    
    mov ax, [bp+calcDiv_op2] 
    mul dx
    mov bx, ax
    mov ax, [bp+calcDiv_op1]
    push bx
    push ax
    call calcSub                        ; Getting remainder
    add sp, 4
     
    xor dx, dx 
    mov bl, al         
    mov al, ah  
    mov dl, 10
    mul dl 
    mov ah, al
    mov bh, ah
    
    mov al, bl
    mov ah, 0 
    div dl
    add bh, al
    mov al, ah
    mov ah, bh 
                  
    
    mov bx, [bp+calcDiv_op2]  
    xor dx, dx
    div bx   
    
    mov bl, al
    
    pop ax
    mov ah, al
    mov al, bl
                                    
    
    pop bx
    pop dx
    pop bp
    ret                
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;                     MACRO DEFINITIONS                           ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DEFINE_PRINT_NUM
DEFINE_PRINT_NUM_UNS
DEFINE_GET_STRING
DEFINE_PTHIS

end start ; set entry point and stop the assembler.
