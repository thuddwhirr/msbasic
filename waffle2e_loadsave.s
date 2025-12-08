; Waffle2e LOAD/SAVE implementation
; Saves/loads tokenized BASIC programs to/from FAT32 filesystem
;
; LOAD "filename"   - load tokenized binary program
; SAVE "filename"   - save tokenized binary program
;
; Binary format: Memory dump from TXTTAB to VARTAB
; After LOAD, FIX_LINKS rebuilds the line pointers

; Constants (must match simple_fat32.s)
TYPE_FILE = $00

; External kernel/FAT32 routines
.import fat32_init, fat32_parse_filename, fat32_resolve_path
.import fat32_init_file_handle, fat32_read_file_sector, fat32_advance_file_position
.import fat32_create_file
.import fat32_file_size, fat32_file_bytes_remaining
.importzp fat32_ptr
.import sector_buffer
.import cmd_buffer

.segment "CODE"

;-----------------------------------------------------------------------------
; SAVE - Save BASIC program to file
;
; Syntax: SAVE "FILENAME"
; Saves tokenized program from TXTTAB to VARTAB-1
;-----------------------------------------------------------------------------
SAVE:
        ; Evaluate string expression (filename)
        jsr     FRMEVL
        jsr     FRESTR          ; A=length, INDEX=pointer to string

        ; Check for empty filename
        tax
        beq     @save_error

        ; Save critical BASIC zero page before FAT32 operations
        ; INDEX will be clobbered, so save the string info first
        stx     save_len        ; Save length
        lda     INDEX
        sta     save_index
        lda     INDEX+1
        sta     save_index+1

        ; Copy filename to cmd_buffer for FAT32 routines
        ldy     #0
@copy_name:
        lda     (INDEX),y
        sta     cmd_buffer,y
        iny
        dex
        bne     @copy_name
        lda     #0              ; Null terminate
        sta     cmd_buffer,y

        ; Initialize FAT32
        jsr     fat32_init
        bcc     @save_error

        ; Parse filename (X=0, filename at cmd_buffer)
        ldx     #0
        jsr     fat32_parse_filename

        ; Calculate program size: VARTAB - TXTTAB
        sec
        lda     VARTAB
        sbc     TXTTAB
        sta     fat32_file_size
        lda     VARTAB+1
        sbc     TXTTAB+1
        sta     fat32_file_size+1
        lda     #0
        sta     fat32_file_size+2
        sta     fat32_file_size+3

        ; Set source pointer to TXTTAB
        lda     TXTTAB
        sta     fat32_ptr
        lda     TXTTAB+1
        sta     fat32_ptr+1

        ; Create file (fat32_create_file writes from fat32_ptr)
        jsr     fat32_create_file
        bcc     @save_error

        ; Print success message
        lda     #<QT_SAVED
        ldy     #>QT_SAVED
        jsr     STROUT
        rts

@save_error:
        lda     #<QT_SAVE_ERR
        ldy     #>QT_SAVE_ERR
        jsr     STROUT
        rts

QT_SAVED:
        .byte   "SAVED", $0D, $00
QT_SAVE_ERR:
        .byte   "?SAVE ERROR", $0D, $00

;-----------------------------------------------------------------------------
; LOAD - Load BASIC program from file
;
; Syntax: LOAD "FILENAME"
; Loads tokenized program to TXTTAB, updates VARTAB
;-----------------------------------------------------------------------------
LOAD:
        ; Evaluate string expression (filename)
        jsr     FRMEVL
        jsr     FRESTR          ; A=length, INDEX=pointer to string

        ; Check for empty filename
        tax
        beq     load_error

        ; Copy filename to cmd_buffer IMMEDIATELY before any other FRMEVL
        ; (FRMEVL can cause string garbage collection which moves strings)
        ldy     #0
@copy_name:
        lda     (INDEX),y
        sta     cmd_buffer,y
        iny
        dex
        bne     @copy_name
        lda     #0              ; Null terminate
        sta     cmd_buffer,y

        ; Initialize FAT32
        jsr     fat32_init
        bcc     load_error

        ; Resolve path to file (X=0, filename at cmd_buffer)
        ldx     #0
        lda     #TYPE_FILE
        jsr     fat32_resolve_path
        bcc     load_error

        ; Initialize file handle for reading
        jsr     fat32_init_file_handle

        ; Set load pointer to TXTTAB (use DEST as ZP pointer)
        lda     TXTTAB
        sta     DEST
        lda     TXTTAB+1
        sta     DEST+1

        ; Load file sector by sector
@load_sector_loop:
        jsr     fat32_read_file_sector
        bcc     @load_done      ; EOF or error

        ; Copy sector to memory
        jsr     load_copy_sector

        ; Check if more bytes remaining
        lda     fat32_file_bytes_remaining
        ora     fat32_file_bytes_remaining+1
        ora     fat32_file_bytes_remaining+2
        ora     fat32_file_bytes_remaining+3
        beq     @load_done

        ; Advance to next sector
        jsr     fat32_advance_file_position
        bcs     @load_sector_loop

@load_done:
        ; Set VARTAB to end of loaded program
        lda     DEST
        sta     VARTAB
        lda     DEST+1
        sta     VARTAB+1

        ; Print success message before FIX_LINKS (which doesn't return)
        lda     #<QT_LOADED
        ldy     #>QT_LOADED
        jsr     STROUT

        ; Fix line links and reset variables (tail call - doesn't return)
        jmp     FIX_LINKS

load_error:
        lda     #<QT_LOAD_ERR
        ldy     #>QT_LOAD_ERR
        jsr     STROUT
        rts

QT_LOADED:
        .byte   $0D, $0A, "LOADED", $0D, $0A, $00
QT_LOAD_ERR:
        .byte   "?LOAD ERROR", $0D, $0A, $00

;-----------------------------------------------------------------------------
; load_copy_sector - Copy current sector to memory at DEST
;
; Copies up to 512 bytes from sector_buffer, limited by bytes_remaining
;-----------------------------------------------------------------------------
load_copy_sector:
        ; Reset sector offset
        lda     #0
        sta     load_offset
        sta     load_offset+1

@copy_loop:
        ; Check if bytes remaining in file
        lda     fat32_file_bytes_remaining
        ora     fat32_file_bytes_remaining+1
        ora     fat32_file_bytes_remaining+2
        ora     fat32_file_bytes_remaining+3
        beq     @copy_done

        ; Check if we've copied full sector (512 bytes)
        lda     load_offset+1
        cmp     #2              ; High byte = 2 means 512
        bcs     @copy_done

        ; Get byte from sector_buffer
        lda     load_offset+1
        beq     @lower_half
        ; Upper half (256-511)
        ldy     load_offset
        lda     sector_buffer+256,y
        jmp     @store_byte
@lower_half:
        ldy     load_offset
        lda     sector_buffer,y

@store_byte:
        ; Store to memory (DEST is ZP pointer)
        ldy     #0
        sta     (DEST),y

        ; Increment DEST pointer
        inc     DEST
        bne     @no_ptr_carry
        inc     DEST+1
@no_ptr_carry:

        ; Decrement bytes remaining (32-bit)
        lda     fat32_file_bytes_remaining
        sec
        sbc     #1
        sta     fat32_file_bytes_remaining
        lda     fat32_file_bytes_remaining+1
        sbc     #0
        sta     fat32_file_bytes_remaining+1
        lda     fat32_file_bytes_remaining+2
        sbc     #0
        sta     fat32_file_bytes_remaining+2
        lda     fat32_file_bytes_remaining+3
        sbc     #0
        sta     fat32_file_bytes_remaining+3

        ; Increment sector offset
        inc     load_offset
        bne     @copy_loop
        inc     load_offset+1
        jmp     @copy_loop

@copy_done:
        rts

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------
.segment "KERNELDATA"
load_offset:      .res 2        ; Offset within sector (0-511)
save_len:         .res 1        ; Saved filename length
save_index:       .res 2        ; Saved INDEX pointer
