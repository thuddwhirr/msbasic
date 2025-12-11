; Waffle2e BASIC Graphics Extensions
; Implements: SCR, CLS, CLR, PSET, LOC, LINE, BOX

; Include hardware definitions for video registers
; Path is relative to msbasic directory where this file lives
.include "../os/src/include/hardware.inc"

; Import kernel video functions
.import video_set_text_position
.import video_wait_ready
.import text_attributes

.segment "BASICBIOS"

; =============================================================================
; Graphics library private storage
; Allocated in kernel data area - NOT shared with BASIC internals!
; =============================================================================
.segment "KERNELDATA"

; Coordinate storage (16-bit each)
gfx_x0:         .res 2      ; Current/start X coordinate
gfx_y0:         .res 2      ; Current/start Y coordinate
gfx_x1:         .res 2      ; End X coordinate
gfx_y1:         .res 2      ; End Y coordinate

; Drawing state
gfx_color:      .res 1      ; Current drawing color (0-255)
gfx_fill:       .res 1      ; Fill color for filled shapes

; Bresenham algorithm working variables
gfx_dx:         .res 2      ; Delta X (16-bit)
gfx_dy:         .res 1      ; Delta Y (8-bit, Y max is 239)
gfx_sx:         .res 1      ; Steep flag / X step direction
gfx_sy:         .res 1      ; Y step direction (+1 or -1)
gfx_err:        .res 2      ; Error accumulator (16-bit)
gfx_has_fill:   .res 1      ; Flag: shape has fill color

; Saved coordinates for BOX outline drawing
gfx_save_x0:    .res 2
gfx_save_y0:    .res 2
gfx_save_x1:    .res 2
gfx_save_y1:    .res 2

.segment "BASICBIOS"

; =============================================================================
; SCR n - Set screen/graphics mode
; Usage: SCR 0 (text mode), SCR 4 (320x240 256-color)
; =============================================================================
SCREEN_CMD:
        jsr     GETBYT              ; Get mode number in X
        txa
        ora     #$60                ; Enable cursor + blink (bits 5,6)
        sta     VIDEO_MODE
        rts

; =============================================================================
; CLS - Clear screen (uses current mode)
; Usage: CLS
; =============================================================================
CLS_CMD:
        lda     VIDEO_MODE
        and     #$07                ; Get mode bits
        cmp     #$04                ; Graphics mode?
        bcs     @clear_graphics
        ; Text mode - clear with current attributes
        lda     #TEXT_CLEAR
        sta     VIDEO_INSTR
        lda     text_attributes
        sta     VIDEO_ARG0
        rts
@clear_graphics:
        ; Graphics mode - clear with color 0
        lda     #CLEAR_SCREEN
        sta     VIDEO_INSTR
        lda     #0
        sta     VIDEO_ARG0
        jsr     video_wait_ready
        rts

; =============================================================================
; CLR n - Set current drawing color (0-255)
; Usage: CLR 15
; =============================================================================
COLOR_CMD:
        jsr     GETBYT              ; Get color in X
        stx     gfx_color
        rts

; =============================================================================
; PSET x,y[,c] - Plot pixel at x,y with optional color
; Usage: PSET 100,50 or PSET 100,50,15
; =============================================================================
PSET_CMD:
        jsr     FRMNUM              ; Get X coordinate
        jsr     GETADR              ; Convert to 16-bit in LINNUM
        lda     LINNUM
        sta     gfx_x0
        lda     LINNUM+1
        sta     gfx_x0+1

        jsr     CHKCOM              ; Skip comma
        jsr     FRMNUM              ; Get Y coordinate
        jsr     GETADR
        lda     LINNUM
        sta     gfx_y0
        lda     LINNUM+1
        sta     gfx_y0+1

        ; Check for optional color
        jsr     CHRGOT              ; Get current character
        cmp     #','                ; Comma?
        bne     @use_current_color
        jsr     CHRGET              ; Skip comma
        jsr     GETBYT              ; Get color in X
        stx     gfx_color

@use_current_color:
        ; Set pixel position
        ; Video controller expects big-endian: ARG0=X_high, ARG1=X_low, ARG2=Y_high, ARG3=Y_low
        lda     #PIXEL_POS
        sta     VIDEO_INSTR
        lda     gfx_x0+1            ; X high byte
        sta     VIDEO_ARG0
        lda     gfx_x0              ; X low byte
        sta     VIDEO_ARG1
        lda     gfx_y0+1            ; Y high byte
        sta     VIDEO_ARG2
        lda     gfx_y0              ; Y low byte
        sta     VIDEO_ARG3          ; Triggers position set

        jsr     video_wait_ready    ; Wait for position to be set

        ; Write pixel
        lda     #WRITE_PIXEL
        sta     VIDEO_INSTR
        lda     gfx_color
        sta     VIDEO_ARG0          ; Triggers pixel write
        rts

; =============================================================================
; LOC row,col - Set text cursor position
; Usage: LOC 10,20
; =============================================================================
LOCATE_CMD:
        jsr     GETBYT              ; Get row in X
        phx                         ; Save row
        jsr     CHKCOM              ; Skip comma
        jsr     GETBYT              ; Get column in X
        txa                         ; Column to A
        ply                         ; Row to Y
        ; A = column (X), Y = row
        jsr     video_set_text_position
        rts

; =============================================================================
; LINE x0,y0,x1,y1[,c] - Draw line from (x0,y0) to (x1,y1)
; If y0=y1, uses fast horizontal line, otherwise Bresenham
; Usage: LINE 0,0,319,239,15
; =============================================================================
LINE_CMD:
        ; Get x0
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_x0
        lda     LINNUM+1
        sta     gfx_x0+1

        ; Get y0
        jsr     CHKCOM
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_y0
        lda     LINNUM+1
        sta     gfx_y0+1

        ; Get x1
        jsr     CHKCOM
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_x1
        lda     LINNUM+1
        sta     gfx_x1+1

        ; Get y1
        jsr     CHKCOM
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_y1
        lda     LINNUM+1
        sta     gfx_y1+1

        ; Check for optional color
        jsr     CHRGOT
        cmp     #','
        bne     @draw_line
        jsr     CHRGET
        jsr     GETBYT
        stx     gfx_color

@draw_line:
        ; Check if horizontal line (y0 == y1)
        lda     gfx_y0
        cmp     gfx_y1
        bne     @bresenham
        lda     gfx_y0+1
        cmp     gfx_y1+1
        bne     @bresenham
        jmp     draw_hline

@bresenham:
        jmp     draw_bresenham

; =============================================================================
; Fast horizontal line drawing
; =============================================================================
draw_hline:
        ; Ensure x0 <= x1 (swap if needed)
        lda     gfx_x1+1
        cmp     gfx_x0+1
        bcc     @swap_x
        bne     @no_swap
        lda     gfx_x1
        cmp     gfx_x0
        bcs     @no_swap
@swap_x:
        ; Swap x0 and x1
        lda     gfx_x0
        ldx     gfx_x1
        stx     gfx_x0
        sta     gfx_x1
        lda     gfx_x0+1
        ldx     gfx_x1+1
        stx     gfx_x0+1
        sta     gfx_x1+1

@no_swap:
        ; Set starting position (big-endian: ARG0=X_high, ARG1=X_low, ARG2=Y_high, ARG3=Y_low)
        lda     #PIXEL_POS
        sta     VIDEO_INSTR
        lda     gfx_x0+1            ; X high byte
        sta     VIDEO_ARG0
        lda     gfx_x0              ; X low byte
        sta     VIDEO_ARG1
        lda     gfx_y0+1            ; Y high byte
        sta     VIDEO_ARG2
        lda     gfx_y0              ; Y low byte
        sta     VIDEO_ARG3

        ; Draw horizontal line using auto-increment
@hloop:
        lda     #WRITE_PIXEL
        sta     VIDEO_INSTR
        lda     gfx_color
        sta     VIDEO_ARG0          ; Write pixel, auto-increments X

        ; Increment current X and check if done
        inc     gfx_x0
        bne     @check_done
        inc     gfx_x0+1
@check_done:
        ; Compare current X with x1+1
        lda     gfx_x0+1
        cmp     gfx_x1+1
        bcc     @hloop              ; x0 < x1, continue
        bne     @done               ; x0 > x1, done
        lda     gfx_x0
        cmp     gfx_x1
        bcc     @hloop              ; x0 < x1, continue
        beq     @hloop              ; x0 == x1, draw last pixel
@done:
        rts

; =============================================================================
; Bresenham line drawing algorithm (16-bit X, 8-bit Y for 320x240)
; Uses the standard integer Bresenham with separate steep/shallow handling
; =============================================================================
draw_bresenham:
        ; Calculate dx = abs(x1 - x0) - 16-bit
        lda     gfx_x1
        sec
        sbc     gfx_x0
        sta     gfx_dx
        lda     gfx_x1+1
        sbc     gfx_x0+1
        sta     gfx_dx+1
        bcs     @dx_pos
        ; Negative - negate 16-bit result
        lda     #0
        sec
        sbc     gfx_dx
        sta     gfx_dx
        lda     #0
        sbc     gfx_dx+1
        sta     gfx_dx+1
@dx_pos:

        ; Calculate dy = abs(y1 - y0) - 8-bit only
        lda     gfx_y1
        sec
        sbc     gfx_y0
        bcs     @dy_pos
        eor     #$FF
        adc     #1
@dy_pos:
        sta     gfx_dy

        ; Determine if steep (dy > dx) - 16-bit compare
        ; If dx high byte is non-zero, dx > 255 > dy, so not steep
        lda     gfx_dx+1
        bne     @not_steep
        ; Both are 8-bit range, compare normally
        lda     gfx_dy
        cmp     gfx_dx
        bcc     @not_steep
        beq     @not_steep
        ; Steep: swap x and y coordinates (full 16-bit swap)
        ; After swap: iterate over Y range (now in gfx_x0/x1), step X (now in gfx_y0/y1)
        lda     gfx_x0
        ldx     gfx_y0
        sta     gfx_y0
        stx     gfx_x0
        lda     gfx_x0+1
        ldx     gfx_y0+1
        sta     gfx_y0+1
        stx     gfx_x0+1
        lda     gfx_x1
        ldx     gfx_y1
        sta     gfx_y1
        stx     gfx_x1
        lda     gfx_x1+1
        ldx     gfx_y1+1
        sta     gfx_y1+1
        stx     gfx_x1+1
        ; Swap dx and dy
        lda     gfx_dx
        ldx     gfx_dy
        sta     gfx_dy
        stx     gfx_dx
        lda     #1
        sta     gfx_sx              ; Use sx as "steep" flag
        bra     @setup_dir
@not_steep:
        lda     #0
        sta     gfx_sx              ; Not steep

@setup_dir:
        ; Ensure x0 < x1 (swap points if needed) - 16-bit compare
        lda     gfx_x1+1
        cmp     gfx_x0+1
        bcc     @do_swap            ; x1 < x0
        bne     @no_swap            ; x1 > x0
        lda     gfx_x1
        cmp     gfx_x0
        bcs     @no_swap            ; x1 >= x0
@do_swap:
        ; Swap start and end points
        lda     gfx_x0
        ldx     gfx_x1
        sta     gfx_x1
        stx     gfx_x0
        lda     gfx_x0+1
        ldx     gfx_x1+1
        sta     gfx_x1+1
        stx     gfx_x0+1
        lda     gfx_y0
        ldx     gfx_y1
        sta     gfx_y1
        stx     gfx_y0
@no_swap:
        ; Determine y step direction
        lda     gfx_y1
        cmp     gfx_y0
        bcs     @ystep_pos
        lda     #$FF                ; ystep = -1
        sta     gfx_sy
        bra     @calc_err
@ystep_pos:
        lda     #1                  ; ystep = 1
        sta     gfx_sy

@calc_err:
        ; Recalculate dx (16-bit) after potential swaps
        lda     gfx_x1
        sec
        sbc     gfx_x0
        sta     gfx_dx
        lda     gfx_x1+1
        sbc     gfx_x0+1
        sta     gfx_dx+1            ; Store high byte

        ; Recalculate dy (8-bit)
        lda     gfx_y1
        sec
        sbc     gfx_y0
        bcs     @dy_pos2
        eor     #$FF
        adc     #1
@dy_pos2:
        sta     gfx_dy

        ; err = dx / 2 (16-bit)
        lda     gfx_dx+1
        lsr
        sta     gfx_err+1
        lda     gfx_dx
        ror
        sta     gfx_err

@bres_loop:
        ; Plot pixel - if steep, swap x,y for actual coordinates
        lda     gfx_sx              ; steep flag
        beq     @plot_normal
        ; Steep: plot (y0, x0) - y0 is actual X, x0 is actual Y
        ; Both coordinates are now 16-bit after swap
        lda     #PIXEL_POS
        sta     VIDEO_INSTR
        lda     gfx_y0+1
        sta     VIDEO_ARG0          ; X high (from swapped Y)
        lda     gfx_y0
        sta     VIDEO_ARG1          ; X low (from swapped Y)
        lda     gfx_x0+1
        sta     VIDEO_ARG2          ; Y high (from swapped X)
        lda     gfx_x0
        sta     VIDEO_ARG3          ; Y low (from swapped X)
        bra     @do_plot
@plot_normal:
        lda     #PIXEL_POS
        sta     VIDEO_INSTR
        lda     gfx_x0+1
        sta     VIDEO_ARG0          ; X high
        lda     gfx_x0
        sta     VIDEO_ARG1          ; X low
        lda     #0
        sta     VIDEO_ARG2          ; Y high = 0
        lda     gfx_y0
        sta     VIDEO_ARG3          ; Y low
@do_plot:
        jsr     video_wait_ready
        lda     #WRITE_PIXEL
        sta     VIDEO_INSTR
        lda     gfx_color
        sta     VIDEO_ARG0

        ; Check if done (x0 == x1) - 16-bit compare
        lda     gfx_x0
        cmp     gfx_x1
        bne     @not_done
        lda     gfx_x0+1
        cmp     gfx_x1+1
        beq     @bres_done
@not_done:

        ; err -= dy (16-bit - 8-bit)
        lda     gfx_err
        sec
        sbc     gfx_dy
        sta     gfx_err
        lda     gfx_err+1
        sbc     #0
        sta     gfx_err+1
        bcs     @no_y_step          ; if err >= 0, skip Y step

        ; err += dx (16-bit)
        lda     gfx_err
        clc
        adc     gfx_dx
        sta     gfx_err
        lda     gfx_err+1
        adc     gfx_dx+1
        sta     gfx_err+1

        ; y0 += ystep (16-bit - could be X coord when steep)
        lda     gfx_sy
        bmi     @dec_y
        ; Increment y0 (16-bit)
        inc     gfx_y0
        bne     @no_y_step
        inc     gfx_y0+1
        bra     @no_y_step
@dec_y:
        ; Decrement y0 (16-bit)
        lda     gfx_y0
        bne     @dec_y_no_borrow
        dec     gfx_y0+1
@dec_y_no_borrow:
        dec     gfx_y0

@no_y_step:
        ; x0 += 1 (16-bit increment)
        inc     gfx_x0
        bne     @x_inc_done
        inc     gfx_x0+1
@x_inc_done:
        jmp     @bres_loop

@bres_done:
        rts

; =============================================================================
; BOX x0,y0,x1,y1[,c[,fill]] - Draw rectangle, optionally filled
; Usage: BOX 10,10,100,100,15 or BOX 10,10,100,100,15,1
; =============================================================================
BOX_CMD:
        ; Initialize fill flag
        lda     #0
        sta     gfx_has_fill

        ; Get x0
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_x0
        lda     LINNUM+1
        sta     gfx_x0+1

        ; Get y0
        jsr     CHKCOM
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_y0
        lda     LINNUM+1
        sta     gfx_y0+1

        ; Get x1
        jsr     CHKCOM
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_x1
        lda     LINNUM+1
        sta     gfx_x1+1

        ; Get y1
        jsr     CHKCOM
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_y1
        lda     LINNUM+1
        sta     gfx_y1+1

        ; Check for optional color
        jsr     CHRGOT
        cmp     #','
        bne     @draw_box
        jsr     CHRGET
        jsr     GETBYT
        stx     gfx_color

        ; Check for optional fill color
        jsr     CHRGOT
        cmp     #','
        bne     @draw_box
        jsr     CHRGET
        jsr     GETBYT
        stx     gfx_fill
        lda     #1
        sta     gfx_has_fill

@draw_box:
        ; Save original coordinates to ZP save area
        lda     gfx_x0
        sta     gfx_save_x0
        lda     gfx_x0+1
        sta     gfx_save_x0+1
        lda     gfx_y0
        sta     gfx_save_y0
        lda     gfx_y0+1
        sta     gfx_save_y0+1
        lda     gfx_x1
        sta     gfx_save_x1
        lda     gfx_x1+1
        sta     gfx_save_x1+1
        lda     gfx_y1
        sta     gfx_save_y1
        lda     gfx_y1+1
        sta     gfx_save_y1+1

        ; If fill specified, draw filled rectangle first
        lda     gfx_has_fill
        beq     @draw_outline
        jsr     draw_filled_box

        ; Restore coordinates (fill may have modified them)
        jsr     box_restore_coords

@draw_outline:
        ; Top line: (x0,y0) to (x1,y0)
        lda     gfx_save_y0
        sta     gfx_y1
        lda     gfx_save_y0+1
        sta     gfx_y1+1
        jsr     draw_hline

        ; Restore coordinates for bottom line
        jsr     box_restore_coords

        ; Bottom line: (x0,y1) to (x1,y1)
        lda     gfx_save_y1
        sta     gfx_y0
        lda     gfx_save_y1+1
        sta     gfx_y0+1
        jsr     draw_hline

        ; Restore coordinates for left line
        jsr     box_restore_coords

        ; Left line: (x0,y0) to (x0,y1) - vertical
        lda     gfx_save_x0
        sta     gfx_x1
        lda     gfx_save_x0+1
        sta     gfx_x1+1
        jsr     draw_bresenham

        ; Restore coordinates for right line
        jsr     box_restore_coords

        ; Right line: (x1,y0) to (x1,y1) - vertical
        lda     gfx_save_x1
        sta     gfx_x0
        lda     gfx_save_x1+1
        sta     gfx_x0+1
        lda     gfx_save_x1
        sta     gfx_x1
        lda     gfx_save_x1+1
        sta     gfx_x1+1
        jsr     draw_bresenham

        rts

; Helper to restore coordinates from save area
box_restore_coords:
        lda     gfx_save_x0
        sta     gfx_x0
        lda     gfx_save_x0+1
        sta     gfx_x0+1
        lda     gfx_save_y0
        sta     gfx_y0
        lda     gfx_save_y0+1
        sta     gfx_y0+1
        lda     gfx_save_x1
        sta     gfx_x1
        lda     gfx_save_x1+1
        sta     gfx_x1+1
        lda     gfx_save_y1
        sta     gfx_y1
        lda     gfx_save_y1+1
        sta     gfx_y1+1
        rts

; =============================================================================
; Draw filled rectangle (called from BOX when fill color specified)
; Uses gfx_save_* for original coordinates since they're set by BOX_CMD
; =============================================================================
draw_filled_box:
        ; Save outline color, use fill color
        lda     gfx_color
        pha
        lda     gfx_fill
        sta     gfx_color

        ; Start at y = save_y0
        lda     gfx_save_y0
        sta     gfx_y0
        lda     gfx_save_y0+1
        sta     gfx_y0+1

        ; Draw horizontal lines from y0 to y1
@fill_loop:
        ; Restore x coordinates (hline modifies gfx_x0)
        lda     gfx_save_x0
        sta     gfx_x0
        lda     gfx_save_x0+1
        sta     gfx_x0+1
        lda     gfx_save_x1
        sta     gfx_x1
        lda     gfx_save_x1+1
        sta     gfx_x1+1

        ; Set y1 = y0 for horizontal line
        lda     gfx_y0
        sta     gfx_y1
        lda     gfx_y0+1
        sta     gfx_y1+1

        jsr     draw_hline

        ; Increment y0
        inc     gfx_y0
        bne     @check_done
        inc     gfx_y0+1

@check_done:
        ; Compare current y0 with original y1 (save_y1)
        ; Continue if y0 <= y1
        lda     gfx_y0+1
        cmp     gfx_save_y1+1
        bcc     @fill_loop          ; y0 high < y1 high, continue
        bne     @fill_done          ; y0 high > y1 high, done
        lda     gfx_y0
        cmp     gfx_save_y1
        bcc     @fill_loop          ; y0 < y1, continue
        beq     @fill_loop          ; y0 == y1, draw last line

@fill_done:
        ; Restore outline color
        pla
        sta     gfx_color
        rts

; =============================================================================
; Stubs for commands not currently in keyword table
; =============================================================================
ATTR_CMD:
        rts

HLINE_CMD:
        rts

BOXF_CMD:
        rts

PALETTE_CMD:
        rts

; Function stubs - return 0
POINT_FN:
        ldy     #0
        jmp     SNGFLT

CSRLIN_FN:
        ldy     #0
        jmp     SNGFLT

CSRCOL_FN:
        ldy     #0
        jmp     SNGFLT
