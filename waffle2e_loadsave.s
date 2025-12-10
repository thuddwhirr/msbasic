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
.import fat32_resolve_parent_path, path_working_cluster, current_dir_cluster
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
        bne     @have_filename
        jmp     @save_error
@have_filename:

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
        bcs     @init_ok
        jmp     @save_error
@init_ok:

        ; Save current directory (will restore after file creation)
        lda     current_dir_cluster
        sta     saved_dir_cluster
        lda     current_dir_cluster+1
        sta     saved_dir_cluster+1
        lda     current_dir_cluster+2
        sta     saved_dir_cluster+2
        lda     current_dir_cluster+3
        sta     saved_dir_cluster+3

        ; Resolve parent path (e.g., "basic/colorbar.bas" -> navigate to "basic")
        ldx     #0                      ; Path starts at offset 0 in cmd_buffer
        jsr     fat32_resolve_parent_path
        bcs     @parent_ok
        ; Parent path error - restore directory and fail
        jmp     @save_restore_error

@parent_ok:
        ; X now points to final component (filename) in cmd_buffer
        ; path_working_cluster has parent directory
        ; Set current_dir_cluster to parent for create operation
        lda     path_working_cluster
        sta     current_dir_cluster
        lda     path_working_cluster+1
        sta     current_dir_cluster+1
        lda     path_working_cluster+2
        sta     current_dir_cluster+2
        lda     path_working_cluster+3
        sta     current_dir_cluster+3

        ; Parse final component to 8.3 format
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
        bcc     @save_restore_error

        ; Restore original directory
        lda     saved_dir_cluster
        sta     current_dir_cluster
        lda     saved_dir_cluster+1
        sta     current_dir_cluster+1
        lda     saved_dir_cluster+2
        sta     current_dir_cluster+2
        lda     saved_dir_cluster+3
        sta     current_dir_cluster+3

        ; Print success message
        lda     #<QT_SAVED
        ldy     #>QT_SAVED
        jsr     STROUT
        rts

@save_restore_error:
        ; Restore original directory before error
        lda     saved_dir_cluster
        sta     current_dir_cluster
        lda     saved_dir_cluster+1
        sta     current_dir_cluster+1
        lda     saved_dir_cluster+2
        sta     current_dir_cluster+2
        lda     saved_dir_cluster+3
        sta     current_dir_cluster+3

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
; FEED - Feed text file to BASIC input
;
; Syntax: FEED "FILENAME"
; Opens a text file and redirects BASIC's input to read from it.
; Lines are executed as if typed at the keyboard.
; When file is exhausted, input returns to normal keyboard/serial.
;-----------------------------------------------------------------------------
; Note: chrin_vector, file_getc, and feed_sector_offset are defined in
; msbasic_bios.s which is included later in the same compilation unit

FEED:
        ; Evaluate string expression (filename)
        jsr     FRMEVL
        jsr     FRESTR          ; A=length, INDEX=pointer to string

        ; Check for empty filename
        tax
        beq     @feed_error

        ; Copy filename to cmd_buffer IMMEDIATELY before any other FRMEVL
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
        bcc     @feed_error

        ; Resolve path to file (X=0, filename at cmd_buffer)
        ldx     #0
        lda     #TYPE_FILE
        jsr     fat32_resolve_path
        bcc     @feed_error

        ; Initialize file handle for reading
        jsr     fat32_init_file_handle

        ; Read first sector into buffer
        jsr     fat32_read_file_sector
        bcc     @feed_error

        ; Initialize sector offset to 0
        lda     #0
        sta     feed_sector_offset
        sta     feed_sector_offset+1

        ; Redirect CHRIN to file_getc
        lda     #<file_getc
        sta     chrin_vector
        lda     #>file_getc
        sta     chrin_vector+1

        ; Print message
        lda     #<QT_FEEDING
        ldy     #>QT_FEEDING
        jsr     STROUT

        ; Return to BASIC - subsequent CHRIN calls will read from file
        rts

@feed_error:
        lda     #<QT_FEED_ERR
        ldy     #>QT_FEED_ERR
        jsr     STROUT
        rts

QT_FEEDING:
        .byte   "FEEDING...", $0D, $0A, $00
QT_FEED_ERR:
        .byte   "?FEED ERROR", $0D, $0A, $00

;-----------------------------------------------------------------------------
; QUIT - Exit BASIC and return to kernel shell
;
; Syntax: QUIT
; Jumps directly to the kernel's command loop
;-----------------------------------------------------------------------------
.import cli_main_loop

QUIT:
        jmp     cli_main_loop

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------
.segment "KERNELDATA"
load_offset:        .res 2        ; Offset within sector (0-511)
save_len:           .res 1        ; Saved filename length
save_index:         .res 2        ; Saved INDEX pointer
saved_dir_cluster:  .res 4        ; Saved current directory for SAVE
