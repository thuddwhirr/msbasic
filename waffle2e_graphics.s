; Waffle2e BASIC Graphics Extensions
; Implements: SCR, CLS, CLR, PSET, LOC, LINE, BOX, CIRC

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

; Circle algorithm variables
gfx_cx:         .res 2      ; Circle center X (16-bit)
gfx_cy:         .res 1      ; Circle center Y (8-bit, max 239)
gfx_radius:     .res 2      ; Circle radius (16-bit)
gfx_circ_x:     .res 2      ; Current circle X offset (16-bit)
gfx_circ_y:     .res 2      ; Current circle Y offset (16-bit)
gfx_circ_err:   .res 2      ; Circle error term (16-bit, signed)

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
; LOC col,row[,cursor] - Set text cursor position with optional cursor control
; Usage: LOC 10,20 or LOC 10,20,0 (hide) or LOC 10,20,1 (show) or LOC 10,20,2 (blink)
; cursor: 0=off, 1=on (no blink), 2=on with blink
; =============================================================================
LOCATE_CMD:
        jsr     GETBYT              ; Get column in X
        phx                         ; Save column
        jsr     CHKCOM              ; Skip comma
        jsr     GETBYT              ; Get row in X
        txa                         ; Row to A
        tay                         ; Row to Y
        pla                         ; Column to A
        ; A = column, Y = row
        jsr     video_set_text_position

        ; Check for optional cursor parameter
        jsr     CHRGOT
        cmp     #','
        bne     @done
        jsr     CHRGET              ; Skip comma
        jsr     GETBYT              ; Get cursor mode in X
        ; X: 0=off, 1=on, 2=blink
        lda     VIDEO_MODE
        and     #$9F                ; Clear bits 5,6 (cursor enable, blink)
        cpx     #0
        beq     @set_mode           ; cursor off
        ora     #$20                ; Set bit 5 (cursor enable)
        cpx     #2
        bne     @set_mode
        ora     #$40                ; Set bit 6 (blink enable)
@set_mode:
        sta     VIDEO_MODE
@done:
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
; CIR cx,cy,r[,c[,fill]] - Draw circle centered at (cx,cy) with radius r
; Uses midpoint circle algorithm (Bresenham's circle)
; Usage: CIR 160,120,50 or CIR 160,120,50,15 or CIR 160,120,50,15,4
; =============================================================================
CIRCLE_CMD:
        ; Initialize fill flag
        lda     #0
        sta     gfx_has_fill

        ; Get center X (16-bit)
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_cx
        lda     LINNUM+1
        sta     gfx_cx+1

        ; Get center Y (8-bit for 320x240 mode)
        jsr     CHKCOM
        jsr     GETBYT
        stx     gfx_cy

        ; Get radius (16-bit)
        jsr     CHKCOM
        jsr     FRMNUM
        jsr     GETADR
        lda     LINNUM
        sta     gfx_radius
        lda     LINNUM+1
        sta     gfx_radius+1

        ; Check for optional color
        jsr     CHRGOT
        cmp     #','
        bne     @draw_circle
        jsr     CHRGET
        jsr     GETBYT
        stx     gfx_color

        ; Check for optional fill color
        jsr     CHRGOT
        cmp     #','
        bne     @draw_circle
        jsr     CHRGET
        jsr     GETBYT
        stx     gfx_fill
        lda     #1
        sta     gfx_has_fill

@draw_circle:
        ; Initialize: x = radius, y = 0, err = 0
        lda     gfx_radius
        sta     gfx_circ_x
        lda     gfx_radius+1
        sta     gfx_circ_x+1

        lda     #0
        sta     gfx_circ_y
        sta     gfx_circ_y+1
        sta     gfx_circ_err
        sta     gfx_circ_err+1

@circle_loop:
        ; If fill enabled, draw fill lines (outline drawn in second pass)
        lda     gfx_has_fill
        beq     @draw_outline
        jsr     circle_fill_lines
        jmp     @next_iter

@draw_outline:
        ; Plot 8 symmetric points (outline) - only when not filling
        jsr     circle_plot_octants

@next_iter:
        ; Check if done: y > x (16-bit compare)
        lda     gfx_circ_x+1
        cmp     gfx_circ_y+1
        bcc     @exit_circle        ; x_hi < y_hi, done
        bne     @not_done           ; x_hi > y_hi, continue
        lda     gfx_circ_x
        cmp     gfx_circ_y
        bcc     @exit_circle        ; x < y, done
        bra     @not_done

@exit_circle:
        jmp     @circle_done

@not_done:
        ; y++
        inc     gfx_circ_y
        bne     @no_y_carry
        inc     gfx_circ_y+1
@no_y_carry:

        ; err += 2*y + 1 (add y twice plus 1)
        ; First add y (16-bit)
        lda     gfx_circ_err
        clc
        adc     gfx_circ_y
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        adc     gfx_circ_y+1
        sta     gfx_circ_err+1
        ; Add y again
        lda     gfx_circ_err
        clc
        adc     gfx_circ_y
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        adc     gfx_circ_y+1
        sta     gfx_circ_err+1
        ; Add 1
        inc     gfx_circ_err
        bne     @check_err
        inc     gfx_circ_err+1

@check_err:
        ; if err > 0, then x--, err -= 2*x + 1
        ; Check sign of err (16-bit signed)
        lda     gfx_circ_err+1
        bmi     @circle_loop        ; err < 0, continue

        ; err > 0: x--
        lda     gfx_circ_x
        bne     @dec_x_no_borrow
        dec     gfx_circ_x+1
@dec_x_no_borrow:
        dec     gfx_circ_x

        ; err -= 2*x + 1
        ; Subtract x twice and 1
        lda     gfx_circ_err
        sec
        sbc     gfx_circ_x
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        sbc     gfx_circ_x+1
        sta     gfx_circ_err+1
        ; Subtract x again
        lda     gfx_circ_err
        sec
        sbc     gfx_circ_x
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        sbc     gfx_circ_x+1
        sta     gfx_circ_err+1
        ; Subtract 1
        lda     gfx_circ_err
        bne     @sub1_no_borrow
        dec     gfx_circ_err+1
@sub1_no_borrow:
        dec     gfx_circ_err

        jmp     @circle_loop

@circle_done:
        ; If fill was enabled, now draw outline in second pass
        lda     gfx_has_fill
        beq     @jmp_really_done
        jmp     @start_outline_pass

@jmp_really_done:
        jmp     @really_done

@start_outline_pass:
        ; Reset for outline pass
        lda     gfx_radius
        sta     gfx_circ_x
        lda     gfx_radius+1
        sta     gfx_circ_x+1
        lda     #0
        sta     gfx_circ_y
        sta     gfx_circ_y+1
        sta     gfx_circ_err
        sta     gfx_circ_err+1

@outline_loop:
        ; Plot 8 symmetric outline points
        jsr     circle_plot_octants

        ; Check if done: y > x
        lda     gfx_circ_x+1
        cmp     gfx_circ_y+1
        bcc     @exit_outline
        bne     @outline_not_done
        lda     gfx_circ_x
        cmp     gfx_circ_y
        bcs     @outline_not_done
@exit_outline:
        jmp     @really_done

@outline_not_done:
        ; y++
        inc     gfx_circ_y
        bne     @outline_no_y_carry
        inc     gfx_circ_y+1
@outline_no_y_carry:

        ; err += 2*y + 1
        lda     gfx_circ_err
        clc
        adc     gfx_circ_y
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        adc     gfx_circ_y+1
        sta     gfx_circ_err+1
        lda     gfx_circ_err
        clc
        adc     gfx_circ_y
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        adc     gfx_circ_y+1
        sta     gfx_circ_err+1
        inc     gfx_circ_err
        bne     @outline_check_err
        inc     gfx_circ_err+1

@outline_check_err:
        lda     gfx_circ_err+1
        bmi     @outline_loop

        ; x--
        lda     gfx_circ_x
        bne     @outline_dec_x
        dec     gfx_circ_x+1
@outline_dec_x:
        dec     gfx_circ_x

        ; err -= 2*x + 1
        lda     gfx_circ_err
        sec
        sbc     gfx_circ_x
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        sbc     gfx_circ_x+1
        sta     gfx_circ_err+1
        lda     gfx_circ_err
        sec
        sbc     gfx_circ_x
        sta     gfx_circ_err
        lda     gfx_circ_err+1
        sbc     gfx_circ_x+1
        sta     gfx_circ_err+1
        lda     gfx_circ_err
        bne     @outline_sub1
        dec     gfx_circ_err+1
@outline_sub1:
        dec     gfx_circ_err

        jmp     @outline_loop

@really_done:
        rts

; =============================================================================
; Draw horizontal fill lines for filled circle
; Draws 4 lines: at cy+y, cy-y, cy+x, cy-x (from -offset to +offset)
; Uses gfx_fill color, preserves gfx_color for outline
; =============================================================================
circle_fill_lines:
        ; Save outline color, use fill color
        lda     gfx_color
        pha
        lda     gfx_fill
        sta     gfx_color

        ; Line 1: horizontal line at cy + y, from cx-x to cx+x
        lda     gfx_cy
        clc
        adc     gfx_circ_y
        bcs     @skip_line1         ; Y overflow
        sta     gfx_y0
        lda     #0
        sta     gfx_y0+1
        ; x0 = cx - x
        lda     gfx_cx
        sec
        sbc     gfx_circ_x
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_x+1
        sta     gfx_x0+1
        ; x1 = cx + x
        lda     gfx_cx
        clc
        adc     gfx_circ_x
        sta     gfx_x1
        lda     gfx_cx+1
        adc     gfx_circ_x+1
        sta     gfx_x1+1
        ; Set y1 = y0 for horizontal line
        lda     gfx_y0
        sta     gfx_y1
        lda     gfx_y0+1
        sta     gfx_y1+1
        jsr     draw_hline
@skip_line1:

        ; Line 2: horizontal line at cy - y, from cx-x to cx+x
        lda     gfx_cy
        sec
        sbc     gfx_circ_y
        bcc     @skip_line2         ; Y underflow
        sta     gfx_y0
        lda     #0
        sta     gfx_y0+1
        ; x0 = cx - x
        lda     gfx_cx
        sec
        sbc     gfx_circ_x
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_x+1
        sta     gfx_x0+1
        ; x1 = cx + x
        lda     gfx_cx
        clc
        adc     gfx_circ_x
        sta     gfx_x1
        lda     gfx_cx+1
        adc     gfx_circ_x+1
        sta     gfx_x1+1
        ; Set y1 = y0
        lda     gfx_y0
        sta     gfx_y1
        lda     gfx_y0+1
        sta     gfx_y1+1
        jsr     draw_hline
@skip_line2:

        ; Line 3: horizontal line at cy + x, from cx-y to cx+y
        lda     gfx_cy
        clc
        adc     gfx_circ_x          ; x offset low byte
        bcs     @skip_line3         ; Y overflow
        sta     gfx_y0
        lda     #0
        sta     gfx_y0+1
        ; x0 = cx - y
        lda     gfx_cx
        sec
        sbc     gfx_circ_y
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_y+1
        sta     gfx_x0+1
        ; x1 = cx + y
        lda     gfx_cx
        clc
        adc     gfx_circ_y
        sta     gfx_x1
        lda     gfx_cx+1
        adc     gfx_circ_y+1
        sta     gfx_x1+1
        ; Set y1 = y0
        lda     gfx_y0
        sta     gfx_y1
        lda     gfx_y0+1
        sta     gfx_y1+1
        jsr     draw_hline
@skip_line3:

        ; Line 4: horizontal line at cy - x, from cx-y to cx+y
        lda     gfx_cy
        sec
        sbc     gfx_circ_x          ; x offset low byte
        bcc     @skip_line4         ; Y underflow
        sta     gfx_y0
        lda     #0
        sta     gfx_y0+1
        ; x0 = cx - y
        lda     gfx_cx
        sec
        sbc     gfx_circ_y
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_y+1
        sta     gfx_x0+1
        ; x1 = cx + y
        lda     gfx_cx
        clc
        adc     gfx_circ_y
        sta     gfx_x1
        lda     gfx_cx+1
        adc     gfx_circ_y+1
        sta     gfx_x1+1
        ; Set y1 = y0
        lda     gfx_y0
        sta     gfx_y1
        lda     gfx_y0+1
        sta     gfx_y1+1
        jsr     draw_hline
@skip_line4:

        ; Restore outline color
        pla
        sta     gfx_color
        rts

; =============================================================================
; Plot 8 symmetric points for circle at (cx,cy) with offsets (x,y)
; Points: (cx+x,cy+y), (cx-x,cy+y), (cx+x,cy-y), (cx-x,cy-y)
;         (cx+y,cy+x), (cx-y,cy+x), (cx+y,cy-x), (cx-y,cy-x)
; Y subtractions must check for underflow since Y is 8-bit
; =============================================================================
circle_plot_octants:
        ; Point 1: (cx + x, cy + y)
        lda     gfx_cx
        clc
        adc     gfx_circ_x
        sta     gfx_x0
        lda     gfx_cx+1
        adc     gfx_circ_x+1
        sta     gfx_x0+1
        lda     gfx_cy
        clc
        adc     gfx_circ_y
        bcs     @skip1              ; Y overflow, skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip1:

        ; Point 2: (cx - x, cy + y)
        lda     gfx_cx
        sec
        sbc     gfx_circ_x
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_x+1
        sta     gfx_x0+1
        lda     gfx_cy
        clc
        adc     gfx_circ_y
        bcs     @skip2              ; Y overflow, skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip2:

        ; Point 3: (cx + x, cy - y)
        lda     gfx_cx
        clc
        adc     gfx_circ_x
        sta     gfx_x0
        lda     gfx_cx+1
        adc     gfx_circ_x+1
        sta     gfx_x0+1
        lda     gfx_cy
        sec
        sbc     gfx_circ_y
        bcc     @skip3              ; Y underflow (went negative), skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip3:

        ; Point 4: (cx - x, cy - y)
        lda     gfx_cx
        sec
        sbc     gfx_circ_x
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_x+1
        sta     gfx_x0+1
        lda     gfx_cy
        sec
        sbc     gfx_circ_y
        bcc     @skip4              ; Y underflow, skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip4:

        ; Point 5: (cx + y, cy + x) - swap x,y offsets
        lda     gfx_cx
        clc
        adc     gfx_circ_y
        sta     gfx_x0
        lda     gfx_cx+1
        adc     gfx_circ_y+1
        sta     gfx_x0+1
        lda     gfx_cy
        clc
        adc     gfx_circ_x          ; X offset low byte for Y coord
        bcs     @skip5              ; Y overflow, skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip5:

        ; Point 6: (cx - y, cy + x)
        lda     gfx_cx
        sec
        sbc     gfx_circ_y
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_y+1
        sta     gfx_x0+1
        lda     gfx_cy
        clc
        adc     gfx_circ_x
        bcs     @skip6              ; Y overflow, skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip6:

        ; Point 7: (cx + y, cy - x)
        lda     gfx_cx
        clc
        adc     gfx_circ_y
        sta     gfx_x0
        lda     gfx_cx+1
        adc     gfx_circ_y+1
        sta     gfx_x0+1
        lda     gfx_cy
        sec
        sbc     gfx_circ_x
        bcc     @skip7              ; Y underflow, skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip7:

        ; Point 8: (cx - y, cy - x)
        lda     gfx_cx
        sec
        sbc     gfx_circ_y
        sta     gfx_x0
        lda     gfx_cx+1
        sbc     gfx_circ_y+1
        sta     gfx_x0+1
        lda     gfx_cy
        sec
        sbc     gfx_circ_x
        bcc     @skip8              ; Y underflow, skip
        sta     gfx_y0
        jsr     circle_plot_point
@skip8:

        rts

; =============================================================================
; Plot single point for circle (with bounds checking)
; Uses gfx_x0 (16-bit) and gfx_y0 (8-bit low byte only)
; Coordinates may be negative (from subtraction) - high byte $80-$FF = negative
; =============================================================================
circle_plot_point:
        ; Check X bounds: 0 <= x < 320
        ; X is 16-bit signed - high byte >= $80 means negative
        lda     gfx_x0+1
        cmp     #$80
        bcs     @skip               ; X negative (high byte >= $80)
        ; Now check if X < 320
        cmp     #1
        bcc     @x_ok               ; X high byte = 0, X < 256
        bne     @skip               ; X high byte > 1, X >= 512
        lda     gfx_x0
        cmp     #64                 ; Check if X >= 320 (256+64)
        bcs     @skip
@x_ok:
        ; Check Y bounds: 0 <= y < 240
        ; Y could have wrapped negative (e.g., 120-160 = -40 = $D8)
        ; Values $80-$FF are negative, $00-$EF are valid (0-239)
        lda     gfx_y0
        cmp     #240
        bcs     @skip               ; Y >= 240 (includes negative $80-$FF)

        ; Plot the pixel
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

        jsr     video_wait_ready

        lda     #WRITE_PIXEL
        sta     VIDEO_INSTR
        lda     gfx_color
        sta     VIDEO_ARG0

@skip:
        rts

; =============================================================================
; ATT n - Set text attribute (color palette index 0-255)
; =============================================================================
ATTR_CMD:
        jsr     GETBYT              ; Get color value in X
        stx     text_attributes     ; Store directly
        rts

HLINE_CMD:
        rts

BOXF_CMD:
        rts

; =============================================================================
; PL index,r,g,b - Set palette entry (Mode 4 only)
; index: 0-255 (palette slot)
; r,g,b: 0-15 each (4-bit color components)
; Usage: PL 0,15,0,0 (set entry 0 to bright red)
; =============================================================================
PALETTE_CMD:
        ; Get palette index (0-255)
        jsr     GETBYT              ; Index in X
        phx                         ; Save index

        ; Get R (0-15)
        jsr     CHKCOM
        jsr     GETBYT              ; R in X
        cpx     #16
        bcs     @bad_color          ; R > 15
        phx                         ; Save R

        ; Get G (0-15)
        jsr     CHKCOM
        jsr     GETBYT              ; G in X
        cpx     #16
        bcs     @bad_color_pop1     ; G > 15
        phx                         ; Save G

        ; Get B (0-15)
        jsr     CHKCOM
        jsr     GETBYT              ; B in X
        cpx     #16
        bcs     @bad_color_pop2     ; B > 15
        stx     gfx_x0              ; Temp store B

        ; Pack GB byte: G in high nibble, B in low nibble
        pla                         ; Get G
        asl
        asl
        asl
        asl                         ; G << 4
        ora     gfx_x0              ; | B
        sta     gfx_x0              ; GB byte

        ; Get R for 0R byte
        pla                         ; Get R
        sta     gfx_x0+1            ; 0R byte (R in low nibble)

        ; Set palette entry
        jsr     video_wait_ready    ; Wait BEFORE popping index

        lda     #SET_PALETTE_ENTRY
        sta     VIDEO_INSTR
        pla                         ; Get index from stack
        sta     VIDEO_ARG0
        lda     gfx_x0              ; GB byte
        sta     VIDEO_ARG1
        lda     gfx_x0+1            ; 0R byte - triggers
        sta     VIDEO_ARG2
        rts

@bad_color_pop2:
        pla                         ; Discard G
@bad_color_pop1:
        pla                         ; Discard R
@bad_color:
        pla                         ; Discard index
        jmp     IQERR               ; Illegal quantity error

; =============================================================================
; XY x,y - Set read position for GDT function
; x: 0-639 (16-bit), y: 0-479 (16-bit)
; Works for both graphics pixel position and text row,col
; Usage: XY 100,50 : PRINT GDT(1)  ' Read pixel at 100,50
;        XY 20,10 : PRINT GDT(0)   ' Read char at col 20, row 10
; =============================================================================
SETPOS_CMD:
        ; Get X coordinate (16-bit)
        jsr     FRMNUM              ; Evaluate X expression
        jsr     GETADR              ; Convert to 16-bit in LINNUM
        lda     LINNUM
        sta     gfx_x0
        lda     LINNUM+1
        sta     gfx_x0+1

        ; Get Y coordinate (16-bit)
        jsr     CHKCOM              ; Expect ','
        jsr     FRMNUM              ; Evaluate Y expression
        jsr     GETADR              ; Convert to 16-bit in LINNUM
        lda     LINNUM
        sta     gfx_y0
        lda     LINNUM+1
        sta     gfx_y0+1
        rts

; =============================================================================
; GDT(n) - Graphics Data Table lookup
; n=0: Get character at (x=col, y=row) - text mode
; n=1: Get pixel color at (x,y) - graphics mode
; n=2: Get cursor row
; n=3: Get cursor column
; Usage: XY 20,10 : PRINT GDT(0)   ' Char at col 20, row 10
;        XY 100,50 : PRINT GDT(1)  ' Pixel at x=100, y=50
;        PRINT GDT(2)              ' Current cursor row
;        PRINT GDT(3)              ' Current cursor column
; =============================================================================
GETDATA_FN:
        ; PARCHK already called by UNARY, argument in FAC
        jsr     GETADR              ; Convert to integer in LINNUM
        lda     LINNUM
        beq     @get_char           ; 0 = get character
        cmp     #1
        beq     @get_pixel          ; 1 = get pixel
        cmp     #2
        beq     @get_cursor_row     ; 2 = cursor row
        cmp     #3
        beq     @get_cursor_col     ; 3 = cursor column
        jmp     IQERR               ; Invalid argument

@get_char:
        ; GET_TEXT_AT: col in ARG0, row in ARG1
        jsr     video_wait_ready
        lda     #GET_TEXT_AT
        sta     VIDEO_INSTR
        lda     gfx_x0              ; Col (X position)
        sta     VIDEO_ARG0
        lda     gfx_y0              ; Row (Y position) - triggers
        sta     VIDEO_ARG1
        jmp     @read_result

@get_pixel:
        ; GET_PIXEL_AT: X high, X low, Y high, Y low
        jsr     video_wait_ready
        lda     #GET_PIXEL_AT
        sta     VIDEO_INSTR
        lda     gfx_x0+1            ; X high byte
        sta     VIDEO_ARG0
        lda     gfx_x0              ; X low byte
        sta     VIDEO_ARG1
        lda     gfx_y0+1            ; Y high byte
        sta     VIDEO_ARG2
        lda     gfx_y0              ; Y low byte - triggers
        sta     VIDEO_ARG3
        jmp     @read_result

@get_cursor_row:
        ; GET_CURSOR_POS with ARG0=0 returns row
        jsr     video_wait_ready
        lda     #GET_CURSOR_POS
        sta     VIDEO_INSTR
        lda     #0                  ; 0 = row - triggers
        sta     VIDEO_ARG0
        jmp     @read_result

@get_cursor_col:
        ; GET_CURSOR_POS with ARG0=1 returns column
        jsr     video_wait_ready
        lda     #GET_CURSOR_POS
        sta     VIDEO_INSTR
        lda     #1                  ; 1 = column - triggers
        sta     VIDEO_ARG0

@read_result:
        jsr     video_wait_ready
        ldy     VIDEO_RESULT0       ; Result in RESULT0 register
        jmp     SNGFLT              ; Return as float
