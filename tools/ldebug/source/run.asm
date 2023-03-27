
%if 0

lDebug code and commands (P, T, G) to run debuggee code

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

..@run_access_start:

gg_repeat:
	call guard_re
	setopt [internalflags2], dif2_gg_again
	jmp @F

		; G command - go.
gg:
	dec si
	dec si
	mov dx, msg.goto
	call isstring?
	je cmd_goto
	inc si
	lodsb

	call guard_re

	clropt [internalflags2], dif2_gg_again
@@:
	mov word [gg_deferred_message], msg.empty_message
	and word [bb_deferred_message_in_lineout_behind], 0

	mov bx, dmycmd
	testopt [options], gg_no_autorepeat
	jnz @F
	mov bx, gg_repeat
@@:
	mov word [lastcmd], bx

	setopt [internalflags2], dif2_gg_is_gg
	clropt [internalflags2], \
		dif2_gg_is_first | dif2_gg_first_detected \
		| dif2_gg_skip_cseip | dif2_gg_skip_non_cseip

	push word [reg_cs]	; save original CS
	pop word [eqladdr+4]
	call parseql		; process =addr

	testopt [options], gg_do_not_skip_bp
	jnz .do_not_skip_cseip
	setopt [internalflags2], dif2_gg_is_first

	cmp byte [eqflag], 0
	jne .cseip_take_eql

	_386_PM_o32		; xor ecx, ecx
	xor cx, cx
	call get_cseip_ecx_linear
	jmp .got_cseip

.cseip_take_eql:
	mov bx, word [eqladdr + 4]
	_386_PM_o32		; mov edx, dword [eqladdr]
	mov dx, word [eqladdr]
	call getlinear_d_b
.got_cseip:
	jc error
	mov word [gg_first_cseip_linear], ax
	mov word [gg_first_cseip_linear + 2], dx
.do_not_skip_cseip:

%ifn _NUM_G_BP
	call chkeol

	testopt [options3], opt3_gg_no_paging
	jz @F
	clropt [internalflags], pagedcommand
@@:
	call tpg_initialise_empty_auxbuff

%if _BREAKPOINTS
	call bb_writepoints_init_reset
%endif

%else
	dec si
	call skipcomma
	dec si
	mov dx, msg.again
	call isstring?
	jne @F			; (after this, do not dec si!)

gg_again:

%if _AUXBUFFSIZE < (BPSIZE * _NUM_G_BP + 1)
 %error auxbuff not large enough for gg breakpoint list
%endif

	push si
	mov si, g_bplist.used_count
	xor ax, ax
	lodsb			; ax = number of breakpoints set yet
	dec si			; -> gg breakpoint list
	mov cx, ax
	add cx, cx
	add cx, cx		; * 4
%if BPSIZE == 4
%elif BPSIZE == 5
	add cx, ax		; * 5
%elif BPSIZE == 6
	add cx, ax		; * 5
	add cx, ax		; * 6
%elif BPSIZE == 9
	add cx, cx		; * 8
	add cx, ax		; * 9
%else
 %error Unexpected breakpoint size
%endif
	inc cx			; include the count
	call guard_auxbuff
	mov es, word [auxbuff_segorsel]
	xor di, di		; es:di -> auxbuff
	rep movsb		; initialise auxbuff list

	pop si			; si -> separator after "AGAIN" keyword
	mov di, 1		; -> first point
	mov cx, ax
	mov al, 0CCh
	jcxz .end
.loop:
	add di, BPSIZE - 1	; -> point content
	stosb			; initialise breakpoint content
	loop .loop
.end:
				; es:di -> after last breakpoint in array
	jmp gg3			; parse additional points (do not dec si!)

@@:
	testopt [internalflags2], dif2_gg_again
	jnz gg_again


gg_list:
	mov dx, msg.list
	call isstring?
	jne .not

	lodsb
	call chkeol

	setopt [internalflags2], dif2_gg_is_first | dif2_gg_skip_cseip

	mov si, g_bplist.bp
	xor cx, cx
	mov cl, byte [si - 1]
	xor bx, bx
	jcxz .none
.loop:
	inc bx
	push cx
	push bx

	mov ax, bx		; 1-based index
	mov di, line_out
	call ordinalbyte

	push di
	sub di, line_out + 1 + 2
				; 1 = a digit, 2 = ordinal suffix,
				; result = how many additional digits are used
	mov dx, msg.list_bp.first
	add dx, di
	call putsz		; show blanks first
	pop di
	call putsline

	call gg_bb_lods_bp_linear
				; BPSIZE implied

	mov di, msg.list_bp.address1
	xchg ax, dx
	call hexword
	inc di
	; mov di, msg.list_bp.address2
	xchg ax, dx
	call hexword

	call gg_bb_check_is_first
				; we set up the dif2_gg_skip_cseip flag,
	mov cx, msg.list_bp_not_cseip
				;  so if CY (do not skip), initialise this
	jc .not_cseip
				;  if NC (do skip), use other string
%if _PM
	push bx
	mov bx, word [reg_cs]
	cmp byte [eqflag], 0
	je @F
	mov bx, word [eqladdr + 4]
@@:
	call test_d_b_bit
	pop bx
	mov cx, msg.list_bp_cseip_32
	jnz @F			; if 32-bit cs -->
%endif
	mov cx, msg.list_bp_csip_16
@@:
.not_cseip:
%if BPSIZE == 6 || BPSIZE == 9
		; INP:	dx:ax = linear address
		;	si -> (d)word offset
		;	di -> where to store
		; OUT:	cx = length displayed
		;	si -> after offset
		;	di -> after stored string
		; CHG:	ax, dx
	push cx
	mov di, line_out
	call bp_display_offset	; BPSIZE implied
	push di
%endif
	mov di, msg.list_bp.value
	lodsb			; BPSIZE implied
	call hexbyte

	mov dx, msg.list_bp.second
	call putsz

%if BPSIZE == 6 || BPSIZE == 9
	pop di
	call putsline
	pop cx
%endif

	mov dx, msg.list_bp.third
	call putsz

	mov dx, cx
	call putsz

	pop bx
	pop cx
	loop .loop
.end:
	; mov dx, msg.list_bp_first_detected
	; testopt [internalflags2], dif2_gg_first_detected
	; jnz .putsz
	retn

.none:
	mov dx, msg.list_bp_none
.putsz:
	jmp putsz

.not:

		; Store the address of each breakpoint into the buffer. We also
		; make sure that there aren't too many breakpoints. (The user can
		; specify them with 2 byte per breakpoints which gives about 128
		; breakpoints with a full command line.) The breakpoints will only
		; be set later when we have verified that the line contains no
		; syntax errors and that there aren't too many breakpoints.
		;
		; Note:	With "G AGAIN" (or the gg_repeat handler), the user
		;	 can actually specify an arbitrary amount of
		;	 breakpoints. However, we limit the amount.
%if _AUXBUFFSIZE < (BPSIZE * _NUM_G_BP + 1)
 %error auxbuff not large enough for gg breakpoint list
%endif
	call guard_auxbuff
	mov es, word [auxbuff_segorsel]
	xor di, di		; es:di -> auxbuff
	xor ax, ax
	stosb			; counter of saved breakpoints
gg3:
	; dec si		; don't use skipcomm0 instead - need to restore al
	call skipcomma
	call iseol?
	je gg4			; if done -->

	push es
	 push ss
	 pop es			; set STT es = ds = ss
	dec si
	mov dx, msg.remember
	call isstring?
	lodsb
	jne @F

	call chkeol
	mov cx, di		; -> after last point, = size of list
	 push ds
	 pop es
	pop ds			; swap
	xor si, si		; ds:si -> auxbuff
	mov di, g_bplist.used_count
				; es:di -> gg breakpoint list
	rep movsb		; copy list over
	 push ss
	 pop ds			; reset segregs
	clropt [internalflags3], dif3_auxbuff_guarded_1
	retn

@@:
	mov bx, word [eqladdr+4]; default segment
	call getlinearaddr	; get linear address into bx:dx (CHG edx)
	pop es
	jc error
	cmp byte [es:0], _NUM_G_BP
	jae error		; can't store another breakpoint, g_bplist is full -->
	xchg ax, dx		; ax = low word
	stosw
	xchg ax, bx		; to store high byte/word
%if _PM
	stosw
%else
	stosb			; bits 24-31 (dh) always zero in 21-bit addresses
%endif
				; BPSIZE implied
%if BPSIZE == 6
	mov ax, word [bp_offset]
	stosw			; write offset (R86M-only 16-bit)
%elif BPSIZE == 9
	mov ax, word [bp_offset]
	stosw
	mov ax, word [bp_offset + 2]
	stosw			; write offset (PM 32-bit)
%endif
	mov al, 0CCh
	stosb			; later filled with the byte read from this address
	inc byte [es:0]		; increment count
	dec si
	jmp short gg3

gg4:
	mov cx, di		; -> after last point, = size of list
	push es
	 push ds
	 pop es
	pop ds			; swap
	xor si, si		; ds:si -> auxbuff
	mov di, g_bplist.used_count
				; es:di -> gg breakpoint list
	rep movsb		; copy list over
	 push ss
	 pop ds			; reset segregs
	clropt [internalflags3], dif3_auxbuff_guarded_1

	testopt [options3], opt3_gg_no_paging
	jz @F
	clropt [internalflags], pagedcommand
@@:

gg5:
	call tpg_initialise_empty_auxbuff
%if _BREAKPOINTS
	call bb_writepoints_init_reset
				; try to write bb points
				; (detect and write to cseip point too)
		; If this fails, it handles the errors and tries to restore
		; all its own points, then aborts the command.

		; This call might return modeswitched.
%endif
	mov si, g_bplist.used_count
	xor ax, ax
	lodsb			; si-> first point
	mov cx, ax		; cx = number of saved breakpoints
	push cx
	call gg_writepoints	; Store breakpoint bytes in the given locations.
	pop dx
		; dx = number of points tried to write
		; cx = number of points not written
	jnc .points_set		; successful -->


		; Failure to write to a gg breakpoint. Now the fun starts!
	sub dx, cx		; = number of points written
	mov cx, dx
			; We now first have to try restoring all the points we
			; already set because they might be inside the DOS or
			; BIOS handlers we would otherwise call. So instead of
			; displaying errors as we detect them, all the intel is
			; stored first until all points have been taken care of
			; (if possible). We then display error messages.
%if _BREAKPOINTS
	sub sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
				; reserve space for bb error info
%endif
	mov bp, sp		; -> behind gg error info, -> bb error info
	add dx, dx
	sub sp, dx		; reserve space for gg error info
	push ax			; store error info on point that failed to be written

		; The gg points were written last, so restore them first.
	call gg_restorepoints_and_init_error_info
%if _BREAKPOINTS
	push cx
		; Next, restore the bb points.
	mov cx, _NUM_B_BP + _NUM_SYM_BP
				; = index above last one to restore
	call bb_restorepoints_and_init_error_info
	pop cx			; (preserve index of failed gg point)
%endif
	pop ax

	call put_deferred_message_silent
				; CHG: dx

		; ax = info on initially failed point
		; cx = 0-based index of initially failed point
		;    = number of points tried to restore
	mov si, cx
	add si, si
	add si, si		; *4
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, cx		; * 5
%elif BPSIZE == 6
	add si, cx		; * 5
	add si, cx		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, cx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, g_bplist.bp

		; si-> point
		; ax = info (ah = reason, al = new value if reason 3)
		; cx = 0-based index of initially failed point
	push cx
	 push word [si + 2]
	 push word [si]		; stack: linear address
	mov bx, 8000h		; bh = 80h (gg),
				;  bl = what we tried to restore (n/a)
	call display_breakpoint_failure
	pop cx
	call gg_handlefailedrestore
%if _BREAKPOINTS
	call bb_handlefailedrestore
	lea sp, [bp + (_NUM_B_BP + _NUM_SYM_BP) * 2]
%else
	mov sp, bp
%endif
				; (discard bb + gg error info)
	retn


.points_set:
		; All bb and gg points were successfully written.
		;  Next: Handle cseip case, if such a point has been detected.


; old cseip breakpoint handling comment:
; interrupt ? emuint : .isstdtrace (including DPMI hack, pushf handling)

%endif	; _NUM_G_BP


%if _NUM_G_BP || _BREAKPOINTS
	testopt [internalflags2], dif2_gg_first_detected
	jz .only_run		; easy case, no cseip point detected -->


		; Enter special mode: Restore cseip breakpoint content.
	setopt [internalflags2], dif2_gg_skip_non_cseip

	mov cx, dx		; = number of points set
%if _BREAKPOINTS
	sub sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
%endif
	mov bp, sp		; -> behind error info
%if _NUM_G_BP
	mov dx, cx
	add dx, dx
	sub sp, dx
	call gg_restorepoints_and_init_error_info

	jnc .gg_restore_cseip_success


		; Error in gg_restorepoints. Try to restore other gg, all bb.

		; Exit special mode: Handle non-cseip breakpoints again.
	clropt [internalflags2], dif2_gg_skip_non_cseip

		; Enter special mode: Skip cseip breakpoints.
	setopt [internalflags2], dif2_gg_skip_cseip

		; As we already tried to restore all cseip gg points,
		;  here we skip these in the gg_restorepoints call.
	call gg_restorepoints

		; Exit special mode: No longer skip cseip breakpoints.
	clropt [internalflags2], dif2_gg_skip_cseip

		; Any cseip bb points aren't yet restored, so do not skip them.
%if _BREAKPOINTS
	push cx
	mov cx, _NUM_B_BP + _NUM_SYM_BP
	call bb_restorepoints_and_init_error_info
	pop cx
%endif
%else
	jmp .gg_restore_cseip_success
%endif

.gg_bb_cseip_fail_common:
		; The failure that led us here is already noted in the info.
%if _NUM_G_BP
	call gg_handlefailedrestore
%endif
%if _BREAKPOINTS
	call bb_handlefailedrestore
%endif
%if _NUM_G_BP
 %if _BREAKPOINTS
	lea sp, [bp + (_NUM_B_BP + _NUM_SYM_BP) * 2]
 %else
	mov sp, bp
 %endif
%elif _BREAKPOINTS
	add sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
%endif
	jmp cmd3

.gg_restore_cseip_success:
%if _BREAKPOINTS
	mov cx, (_NUM_B_BP + _NUM_SYM_BP)
				; = index above last one to restore
	call bb_restorepoints_and_init_error_info
	jnc @F			; no error ? -->

		; Error in bb_restorepoints. Try to restore other gg, other bb.

		; Exit special mode: Handle non-cseip breakpoints again.
	clropt [internalflags2], dif2_gg_skip_non_cseip

		; Enter special mode: Skip cseip breakpoints.
	setopt [internalflags2], dif2_gg_skip_cseip

		; As we already tried to restore all cseip gg and bb points,
		;  here we skip these in the bb_restorepoints call.
%if _NUM_G_BP
	xor cx, cx
	mov cl, byte [g_bplist.used_count]
	call gg_restorepoints
	push cx
%endif
	mov cx, _NUM_B_BP + _NUM_SYM_BP
	call bb_restorepoints
%if _NUM_G_BP
	pop cx
%endif

		; Exit special mode: No longer skip cseip breakpoints.
	clropt [internalflags2], dif2_gg_skip_cseip

	jmp .gg_bb_cseip_fail_common

@@:
		; Success! Now discard the reserved error info.
	lea sp, [bp + (_NUM_B_BP + _NUM_SYM_BP) * 2]
%else
	mov sp, bp
%endif

		; Special mode restoration handled. Now trace one instruction.
		;  (Proceed if repeated string op or interrupt.)
%if _PM
	call resetmode
%endif
	call seteq		; make the = operand take effect
	mov dx, 15		; DL = number of bytes to go; DH = prefix flags.
	mov bx, word [reg_cs]
	_386_PM_o32	; mov esi, dword [reg_eip]
	mov si, word [reg_eip]
.pp2:
	call pp16		; get next instruction byte into AL
	mov di, ppbytes
	mov cx, PPLEN_ONLY_STRING
%if _SYMBOLIC
	mov byte [pp_instruction], al
%endif
	repne scasb
	jne .not_p		; if not one of these -->
	mov al,byte [di+PPLEN-1]; get corresponding byte in ppinfo
	test al, PP_PREFIX	; prefix ?
	jz .pp3			; no -->
	or dh, al		; set the OSIZE or ASIZE flags if either of these
			; Note:	Multiple OSIZE in a 16-bit cs do not toggle
			;	between decoding as O32 and O16, they're always
			;	decoded as O32. The same is true for A32, and
			;	in a 32-bit cs for O16 and A16.
	dec dl
	jnz .pp2		; if not out of bytes -->
	mov word [gg_deferred_message], msg.warnprefix
	jmp .not_p

		; A repeatable string instruction is to be decoded.
		; Finish the decoding and skip the appropriate number
		; of opcode bytes.
.pp3:
_386_PM	call pp_fix32bitflags
	test al, PP_VARSIZ | PP_SIZ_MASK
	jnz error
%if 0
	test al, PP_VARSIZ	; different opcode length depends on OSIZE ?
	jz .ignoreosize		; no -->
	and dh, 2
	add al, dh
.ignoreosize:
	and ax, PP_SIZ_MASK
_386_PM	movzx eax, ax		; clear high word (in case it counts)
	_386_PM_o32	; add esi, eax
	add si, ax
%endif
; pp10:
%if _SYMBOLIC
	call pp3_check_symhints
	jc .not_p		; trace -->
%endif
	; jmp short pp11	; we have a skippable instruction here
; pp11:
_386_PM	call test_d_b_bit
_386_PM	jnz .32			; full 32-bit offset valid -->
_386_PM	movzx esi, si		; clear high word here
.32:
	call proceedbreakpoint	; run until the breakpoint is hit
		; This call might return modeswitched.
	jmp short @F

.not_p:
	call traceone		; call common code
@@:
	pushf

		; Exit special mode, do not skip non-cseip breakpoints anymore.
	clropt [internalflags2], dif2_gg_skip_non_cseip

		; Enter special mode: Skip matching/restoring cseip breakpoint.
	setopt [internalflags2], dif2_gg_skip_cseip

	test ah, 7Fh		; error happened during proceedbreakpoint ?
	jz @F			; no -->

	pop cx			; (discard flags on stack)

%if _NUM_G_BP
	xor cx, cx
	mov cl, byte [g_bplist.used_count]
%endif

%if _BREAKPOINTS
	sub sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
				; reserve space for bb error info
%endif
	mov bp, sp		; -> behind gg error info, -> bb error info
%if _NUM_G_BP
	mov dx, cx
	add dx, dx
	sub sp, dx		; reserve space for gg error info
%endif
	push ax
%if _NUM_G_BP
	call gg_restorepoints_and_init_error_info
%endif
%if _BREAKPOINTS
	 push cx
	mov cx, _NUM_B_BP + _NUM_SYM_BP
	call bb_restorepoints_and_init_error_info
	 pop cx
%endif
		; Exit special mode: No longer skip cseip breakpoints.
	clropt [internalflags2], dif2_gg_skip_cseip
	pop ax
	 push cx

%if _PM
	call resetmode
%endif
	call put_deferred_message_silent

	 push word [tpg_proceed_bp + 2]
	 push word [tpg_proceed_bp]
	mov bl, [tpg_proceed_bp + BPSIZE - 1]
	mov bh, 0		; proceed breakpoint
	call display_breakpoint_failure
	 pop cx
	jmp .gg_bb_cseip_fail_common


@@:
	popf			; CF

	jc .after_run		; an unexpected interrupt occured -->

	call .after_run_restore	; restore stuff
	call gg_bb_check_hit	; expected interrupt matches our gg or bb ?
	jnc .expectedinterrupt	; yes, handle expected interrupt -->


		; Clear all special modes. Stop specialcasing cseip breakpoint.
	clropt [internalflags2], \
		dif2_gg_is_first | dif2_gg_first_detected \
		| dif2_gg_skip_cseip | dif2_gg_skip_non_cseip
	jmp gg5			; next write all points and run -->
%endif	; _NUM_G_BP || _BREAKPOINTS

.only_run:
		; Clear all special modes. Stop specialcasing cseip breakpoint.
	clropt [internalflags2], \
		dif2_gg_is_first | dif2_gg_first_detected \
		| dif2_gg_skip_cseip | dif2_gg_skip_non_cseip

	call run		; Now run the program.
.after_run:
%if _NUM_G_BP || _BREAKPOINTS
	call .after_run_restore

	call gg_bb_check_hit
.after_gg_bb_check_hit:
	jnc .expectedinterrupt
%endif	; _NUM_G_BP || _BREAKPOINTS
.unexpectedinterrupt:
%if _PM
	call resetmode
%endif
	call put_deferred_message_silent
	jmp unexpectedinterrupt	; print messages for unexpected breakpoint and quit.

%if _NUM_G_BP || _BREAKPOINTS
.expectedinterrupt:
	call adjust_cseip_after_breakpoint
				; it's one of our breakpoints, adjust (e)ip

	mov cx, ax		; handle_bb_* expects flags in cx
	push ax
	push ax			; handle_bb_* expects dword counter on stack
	call handle_bb_hit_pass_match
	pop ax
	pop ax			; discard
	jnc gg5			; if it was a pass non-hit or non-pass non-hit
				;  then jump back to do a subsequent G step
		; If jumping, the function has set up gg_first_cseip_linear
		;  with the current CS:(E)IP so that the next step will start
		;  out with skipping past the breakpoint(s) on that address.
		; Note that gg_bb_check_hit returns ax = 7 if a gg point is
		;  hit, so we always fall through to .actual_hit here.

.actual_hit:
%if _PM
	call resetmode
%endif
	call put_deferred_message_silent
				; (put bb message after gg_bb_check_hit call)
	jmp dumpregs_extended_silent
				; (handles sf_(double_)ctrl_c)
%endif


.after_run_restore:
%if _NUM_G_BP || _BREAKPOINTS
 	sub sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
				; reserve space for bb error info
	mov bp, sp		; -> behind gg error info, -> bb error info
 %if _NUM_G_BP
	xor cx, cx
	mov cl, byte [g_bplist.used_count]
	mov dx, cx
	add dx, dx
	sub sp, dx		; reserve space for gg error info
	call gg_restorepoints_and_init_error_info
				; try restoring gg points, and fill error info
 %endif
 %if _BREAKPOINTS
	push cx
	mov cx, _NUM_B_BP + _NUM_SYM_BP
				; = index above last one to restore
	call bb_restorepoints_and_init_error_info
				; try restoring bb points, and fill error info
	pop cx
 %endif

%if _PM
	call resetmode
%endif
	call put_deferred_message_silent

 %if _NUM_G_BP
	call gg_handlefailedrestore
				; handle gg point restore failures
 %endif
 %if _BREAKPOINTS
	call bb_handlefailedrestore
				; handle bb point restore failures
 %endif
 %if _NUM_G_BP
  %if _BREAKPOINTS
	lea sp, [bp + (_NUM_B_BP + _NUM_SYM_BP) * 2]
  %else
	mov sp, bp		; remove the stack frame
  %endif
 %else
	add sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
 %endif
%endif	; _NUM_G_BP || _BREAKPOINTS
	retn


		; INP:	word [gg_deferred_message]
		;	word [bb_deferred_message_in_lineout_behind]
		; OUT:	variables reset to msg.empty_message
		;	messages displayed; first the gg one then the bb one
		; CHG:	dx
		; STT:	ds = es = ss
		;
		; Note:	Uses putsz_silent and putsline_silent, meaning
		;	 if silent mode is enabled, the messages are
		;	 written to the silent buffer instead of displayed.
put_deferred_message_silent:
	mov dx, msg.empty_message
	xchg dx, word [gg_deferred_message]
	call putsz_silent
	mov dx, putsline_silent

		; INP:	dx = puts function to call, CHG ax, bx, cx, dx, di
		; CHG:	dx
		; STT:	ds = es = ss
put_bb_deferred_message_calling_dx:
	push di
	xor di, di
	xchg di, word [bb_deferred_message_in_lineout_behind]
	test di, di
	jz @F
	push ax
	push bx
	push cx
	call dx
	pop cx
	pop bx
	pop ax
@@:
	pop di
	retn


		; INP:	[internalflags2] & dif2_tpg_adjusted_cseip
		;	[internalflags2] & dif2_tpg_do_not_adjust
		;	word [reg_cs]
		;	(d)word [reg_eip]
		; OUT:	If both flags clear on input,
		;	 set [internalflags2] & dif2_tpg_adjusted_cseip
		;	 cs:(e)ip adjusted by decrementing (e)ip
		;	 (It is only decremented by the first call to this
		;	  function, which sets the flag in dif2.)
		;	Else,
		;	 do nothing
		; CHG:	bx
		; STT:	ds = ss = debugger data selector
adjust_cseip_after_breakpoint:
	testopt [internalflags2], \
		dif2_tpg_adjusted_cseip | dif2_tpg_do_not_adjust
	jnz .retn
	setopt [internalflags2], dif2_tpg_adjusted_cseip
_386_PM	mov bx, word [reg_cs]
_386_PM	call resetmode_and_test_d_b_bit
_386_PM	jz .16			; 16-bit cs -->
_386_PM	o32			; dec dword [reg_eip]
.16:
	dec word [reg_eip]	; re-execute (restored) opcode one byte in front of this
.retn:
	retn


%ifn _BREAKPOINTS
bb_check_hit:
	xor ax, ax
	stc
	retn
%else
		; INP:	word [run_int]
		;	word [reg_cs]
		;	(d)word [reg_eip]
		;	bb breakpoints
		; OUT:	NC if a breakpoint was hit,
		;	 (e)ip must be decremented by one
		;	 word [bb_deferred_message_in_lineout_behind] set
		;	  and line_out written if bb point matched
		;	 (The bb point's index is already written to this msg.)
		;	 ax & 1 set if non-pass match (actual hit),
		;	  else ax & 2 set if pass match	(consider as hit first,
		;		but dump registers next (not to silent buffer)
		;		and then continue execution)
		;	  else ax & 4 always set, indicates any match
		;		(including matches that should merely continue)
		;	 all pass points' counters stepped
		;	CY if no breakpoint was hit,
		;	 ax = 0
		; CHG:	all
		; STT:	es = ds = ss
bb_check_hit:
	lframe near
	lenter
	xor ax, ax
	lequ 1,		flag_trigger
	lequ 2,		flag_pass
	lequ 4,		flag_match
	lvar word,	flags
	 push ax

		; Finish up. Check if it was one of _our_ breakpoints.
	cmp word [run_int], int3msg
	jne @F			; if not interrupt 03h -->

			; Get previous cs:eip (where breakpoint was executed if any at all).
	call get_cseip_of_possible_breakpoint
				; dx:ax = linear address of previous cs:eip
				; bx = reg_cs
	jmp .check

@@:
		; For T/TP/P: if trace interrupt fired just while
		;  pointing at a bb point, do match.
	cmp word [run_int], int1msg
	jne .gg9

	setopt [internalflags2], dif2_tpg_do_not_adjust
				; remember that we should not adjust
	_386_PM_o32
	xor cx, cx
	call get_cseip_ecx_linear
				; get linear of this cs:(e)ip
.check:
	jc .gg9

		; Store the matched address (if any) here in case of non-hit
		;  match. (Ie, non-hit pass match or non-hit non-pass match.)
	mov word [gg_next_cseip_linear], ax
	mov word [gg_next_cseip_linear + 2], dx

	xchg bx, dx
	xchg cx, ax		; bx:cx = linear address of previous cs:eip
	xor ax, ax
.loop:
	push bx
	push ax
	call calcpointbit	; bx = index, ah = value
	test byte [b_bplist.used_mask+bx], ah
				; (NC)
	jz .next
	test byte [b_bplist.disabled_mask+bx], ah
				; (NC)
	jnz .next
	pop ax
	pop bx
	push bx
	push ax
	mov si, ax
	add si, si
	add si, si
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, ax
%elif BPSIZE == 6
	add si, ax		; * 5
	add si, ax		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, ax		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, b_bplist.bp	; -> point

	call gg_bb_lods_bp_linear

	call gg_bb_check_is_first
	jnc .next

	cmp ax, cx
	jne .next
	cmp dx, bx
	jne .next

.hit:
	 pop ax
	 push ax

	or byte [bp + ?flags], ?flag_match

%if _SYMBOLIC
	cmp ax, _NUM_B_BP	; is it a symbol breakpoint ?
	jb @F			; no -->

		; skip WHEN and counter handling for symbol breakpoints
	test byte [bp + ?flags], ?flag_trigger
				; triggered yet ?
	jz .setup_trigger	; no, trigger now -->
	jmp .next		; yes, ignore -->

@@:
%endif
	mov di, ax
	add di, di

	mov si, [b_bplist.when + di]
				; si -> condition
	test si, si		; any ?
	jz @F			; no -->
%if _PM
	call resetmode
%endif
	push word [rc]
	pop word [priorrc]
	lodsb
	call getexpression	; parse stored expression
	call chkeol
	call toboolean		; get boolean
	test dx, dx		; true ?
	jz .next		; no, skip -->
	 pop ax
	 push ax
@@:

	lea bx, [b_bplist.counter + di]
				; word [bx] = this matched point's counter

	test byte [bp + ?flags], ?flag_trigger | ?flag_pass
	jz @F			; none set yet ? -->
	call step_pass_counter	; step counter even if already matched
	jnc .next		; (either is already set, so additional
				;  setting of ?flag_pass is skipped)
	test byte [bp + ?flags], ?flag_trigger
	jnz .next		; (trigger is already set, so skip triggering)
	jmp .setup_trigger	; triggered (after previous pass match)

@@:
	call step_pass_counter	; step counter of matched point, no flag yet
	jnc .check_pass		; not triggered, check for pass match -->

.setup_trigger:
		; Trigger! (And the first detected triggering point.)
	or byte [bp + ?flags], ?flag_trigger

	push cx
	mov di, line_out
	mov si, msg.bb_hit.1
%if _SYMBOLIC
	cmp ax, _NUM_B_BP
	jb @F
	mov si, msg.bb_sym_hit.1
@@:
%endif
	call copy_single_counted_string
		; (If _SYMBOLIC=0) Now si -> msg.bb_hit.2.nocounter
		; (If _SYMBOLIC=1) Now si -> msg.bb_hit.2.nocounter
		;		or si -> msg.bb_sym_hit.2.nocounter


	mov dx, ax
%if _SYMBOLIC
	cmp ax, _NUM_B_BP
	jb @F
	sub ax, _NUM_B_BP
	call hexbyte		; store index of this point
	jmp @FF			; skip counter dump -->

@@:
%endif
		; Store breakpoint index in message.
	call hexbyte		; store index of this point

		; Get counter of this breakpoint.
	mov ax, word [bx]

		; Is it equal to default ?
	cmp ax, 8000h
	je @F			; yes, skip -->

	mov si, msg.bb_hit.2.counter
	call copy_single_counted_string
		; Now si -> msg.bb_hit.3.counter.no_id

		; Store counter in message.
	call hexword

@@:
	testopt [internalflags2], dif2_gg_is_gg
	jz @F
	testopt [options], gg_bb_hit_no_repeat
	jmp @FF
@@:
	testopt [options], tp_bb_hit_no_repeat
@@:
	jz @F
	mov word [lastcmd], dmycmd
@@:
	jmp .trigger_common


.check_pass:
	jz .next		; no pass match ? -->

	or byte [bp + ?flags], ?flag_pass

	push cx
	mov di, line_out
	mov si, msg.bb_pass.1
	call copy_single_counted_string
		; Now si -> msg.bb_pass.2

	mov dx, ax
		; Store breakpoint index in message.
	call hexbyte		; store index of this point

	call copy_single_counted_string
		; Now si -> msg.bb_pass.3.no_id

		; Get counter of this breakpoint.
	mov ax, word [bx]
		; Store counter in message.
	call hexword

.trigger_common:
	push bx
%if _SYMBOLIC
	cmp dx, _NUM_B_BP	; symbol breakpoint ?
	jae @F			; yes, no ID -->
%endif
	mov bx, -1
	call get_set_id_offset_length
	test bh, 63 << 2	; length nonzero ?
	jz @F			; no -->

		; The maximum length of a short ID is based on
		;  how much space there is after the longest message
		;  ("Passed ..., counter=XXXX") assuming 80 columns.
	mov si, msg.bb_hitpass_id.short
	cmp bh, 29 << 2		; long ?
	jb .trigger_short_id
		; This jump MUST be a jb, not jbe. The jbe
		;  would not match ZR for words where the
		;  idbuffer offset is a nonzero value.
	mov si, msg.bb_hitpass_id.long
.trigger_short_id:

	call copy_single_counted_string
	mov cl, bh
	shr cl, 1
	shr cl, 1		; cx = length
	and bx, 1023		; bx = offset
	lea si, [b_bplist.idbuffer + bx]
	rep movsb

	mov si, msg.bb_hitpass_id.after

@@:
	pop bx
	call copy_single_counted_string

	mov word [bb_deferred_message_in_lineout_behind], di
	pop cx

.next:
	pop ax
	pop bx

	inc ax
	cmp ax, _NUM_B_BP + _NUM_SYM_BP
	jb .loop

	mov ax, word [bp + ?flags]
	test al, ?flag_pass | ?flag_trigger | ?flag_match
	jnz .return		; (NC)

.gg9:
	xor ax, ax
	stc
.return:
	lleave
	retn


		; INP:	byte [ds:si] = length of source string
		;	ds:si + 1 -> source string
		;	es:di -> destination buffer
		; OUT:	cx = 0
		;	ds:si -> after source string
		;	es:di -> after written string
		; CHG:	-
		; STT:	UP
copy_single_counted_string:
	xchg ax, cx
	xor ax, ax
	lodsb
	xchg ax, cx
	rep movsb
	retn


		; INP:	word [bx] = pass counter of this breakpoint
		; OUT:	NC if to proceed (no trigger),
		;	 ZR if no pass message display
		;	 NZ if pass message display
		;	CY if to trigger
step_pass_counter:
	test word [bx], 3FFFh		; is it already at a terminal state ?
	jz .no_decrement		; yes, do not further decrement -->
	dec word [bx]			; decrement (to 0/4000h/8000h/C000h)
	jz .trigger			; case for decrementing 1 to 0 -->
	cmp word [bx], 4000h
	je .trigger			; case for decrementing 4001h to 4000h
.no_decrement:
	cmp word [bx], 8000h		; decrement resulted in 8000h
	je .trigger			;  or was already in that state? -->
	cmp word [bx], 0_C000h
	je .trigger			; or C000h -->
.proceed:
	test byte [bx + 1], 40h		; (NC) ZR if no pass message
	retn

.trigger:
	stc
	retn
%endif


		; INP:	si -> linear address of breakpoint
		;	 (32 bits if _PM, else 24 bits)
		; OUT:	dx:ax = linear address of breakpoint
		;	si -> behind linear address
gg_bb_lods_bp_linear:
	lodsw
	xchg ax, dx
%if _PM
	lodsw
%else
	xor ax, ax
	lodsb
%endif
	xchg ax, dx
	retn


		; INP:	word [run_int]
		;	word [reg_cs]
		;	(d)word [reg_eip]
		;	gg/bb breakpoints
		; OUT:	NC if a breakpoint was hit,
		;	 (e)ip must be decremented by one
		;	 word [bb_deferred_message_in_lineout_behind] set
		;	  and line_out written if bb point matched
		;	 (The bb point's index is already written to this msg.)
		;	 ax = 7 if non-bb match, else
		;	 ax & 1 set if non-pass match (actual hit),
		;	  else ax & 2 set if pass match (consider as hit first,
		;		but dump registers next (not to silent buffer)
		;		and then continue execution)
		;	  else ax & 4 always set, indicates any match
		;		(including matches that should merely continue)
		;	 all pass points' counters stepped
		;	CY if no breakpoint was hit,
		;	 ax = 0
		; CHG:	all
		; STT:	es = ds = ss
gg_bb_check_hit:
	call bb_check_hit
	jc .gg_check_hit

	test al, 1		; actual bb hit ?
	jnz .ret_NC		; yes, return as hit

	push ax			; bb is pass match or any other match,
	call gg_check_hit	;  is gg a match ?
	pop ax
	jnc gg_check_hit.hit	; yes --> (set NC, ax = 7)

		; Here, we return the flags 2 (set if pass match) and
		; 4 (always set, indicating any match).

.ret_NC:
	clc
	retn

.gg_check_hit:
	; (fall through)

%ifn _NUM_G_BP
gg_check_hit:
	stc
	retn
%else
		; INP:	word [run_int]
		;	word [reg_cs]
		;	(d)word [reg_eip]
		;	bb breakpoints
		; OUT:	NC if a breakpoint was hit,
		;	 (e)ip must be decremented by one
		;	 ax = 7
		;	CY if no breakpoint was hit,
		;	 ax = 0
		; CHG:	all
		; STT:	es = ds = ss
gg_check_hit:
		; Finish up. Check if it was one of _our_ breakpoints.
	cmp word [run_int], int3msg
	jne .gg9		; if not interrupt 03h -->

			; Get previous cs:eip (where breakpoint was executed if any at all).
	call get_cseip_of_possible_breakpoint
				; dx:ax = linear address of previous cs:eip
	jc .gg9
	mov si, g_bplist.bp
	xor cx, cx
	mov cl, byte [si-1]	; number of saved breakpoints
	jcxz .gg9		; none, so always unexpected -->

	mov di, ax
	mov bx, dx		; bx:di = linear address of previous cs:(e)ip

.loop_gg6:
	call gg_bb_lods_bp_linear

	call gg_bb_check_is_first
	jnc .next

	cmp dx, bx
	jne .next
	cmp ax, di
	jne .next

.hit:
	mov ax, 7
	clc
	retn

.next:

%if BPSIZE == 4 || BPSIZE == 5
	inc si			; skip saved (actually CCh) byte
%elif BPSIZE == 6
	add si, 3		; skip word offset and byte content
%elif BPSIZE == 9
	add si, 5		; skip dword offset and byte content
%endif
				; BPSIZE implied
	loop .loop_gg6		; try next if there's any

.gg9:
	xor ax, ax
	stc
	retn
%endif


%if _DELAY_BEFORE_BP
delay_before_bp:
	testopt [options3], opt3_delay_before_bp
	jz .ret
	testopt [internalflags3], dif3_delayed
	jnz .ret
	setopt [internalflags3], dif3_delayed
	push es
	push di
	push ax
	mov di, 40h		; dual mode segment/selector
	mov es, di
	mov di, word [es:6Ch]
@@:
	cmp di, word [es:6Ch]
	jne @F
	call idle
	jmp @B
@@:
	pop ax
	pop di
	pop es
.ret:
	retn
%endif


%if _BREAKPOINTS
bb_writepoints_init_reset:
	mov di, b_bplist.bp
	mov al, 0CCh
	mov cx, _NUM_B_BP + _NUM_SYM_BP
.loop:
	add di, BPSIZE - 1
	stosb
	loop .loop

		; This is called first by gg before writing any of the
		;  gg points. So, if it fails, it only needs to restore
		;  its own points, not any of the gg points.
		; This is also called deep down in run_with_bb when called
		;  from tt or pp. In this case, there may be a proceed
		;  breakpoint already written. On failure, after having
		;  restored all yet-written bb points, this proceed
		;  breakpoint is restored too.
		; Symbolic branch: This initialises symbol breakpoints.
		;
		; INP:	bb breakpoints
		;	tpg_proceed_bp
		; OUT:	does not return if an error occurred,
		;	 instead jumps to cmd3
		; STT:	might return modeswitched
bb_writepoints_init:
%if _SYMBOLIC
	nearcall zz_detect_xms	; re-detect XMS if used after run

	mov dx, word [sym_storage.main.bb.first]
	mov bx, b_bplist.bp + _NUM_B_BP * BPSIZE
	mov cx, _NUM_B_BP
	jmp .sym_condition

.sym_loop:
	cmp cx, _NUM_B_BP + _NUM_SYM_BP
	jb .sym_enough
	mov dx, msg.bb_sym_too_many
.sym_error_putsz:
	call putsz_error
	mov cx, (_NUM_B_BP + _NUM_SYM_BP)
	xor ax, ax
@@:
	push ax
	loop @B			; dummy bb error info
	mov bp, sp
	dec cx			; no bb write/restore failure
	push cx
	push cx
	jmp .sym_error_done	; handle error (including pp restore) -->

.sym_enough:
	 push dx
	 push ax
	dualcall getfarpointer.main
	 pop di
	 pop es

	mov ax, word [es:di + smLinear]
	mov word [bx], ax	; store low word of linear
	inc bx
	inc bx
	mov ax, word [es:di + smLinear + 2]
%if BPSIZE == 5 || BPSIZE == 9
	mov word [bx], ax	; store high word of linear
	inc bx
	inc bx
%else
	mov byte [bx], al	; store high byte of linear
	inc bx
	mov dx, msg.bb_sym_beyond_linear
	test ah, ah
	jnz .sym_error_putsz
%endif
%if BPSIZE == 6 || BPSIZE == 9
	mov ax, word [es:di + smOffset]
	mov word [bx], ax	; store low word of offset
	inc bx
	inc bx
	mov ax, word [es:di + smOffset + 2]
 %if BPSIZE == 9
	mov word [bx], ax	; store high word of offset
	inc bx
	inc bx
 %else
	mov dx, msg.bb_sym_beyond_offset
	test ax, ax
	jnz .sym_error_putsz
 %endif
%endif
	inc bx			; skip content byte
	mov ax, cx
	push bx
	call calcpointbit
	or byte [b_bplist.used_mask + bx], ah
	pop bx
	inc cx
	mov dx, word [es:di + smSpecialNext]

.sym_condition:
	cmp dx, -1
	jne .sym_loop

	jmp @FF
@@:
	mov ax, cx
	call calcpointbit
	not ah
	and byte [b_bplist.used_mask + bx], ah
	inc cx
@@:
	cmp cx, _NUM_B_BP + _NUM_SYM_BP
	jb @BB
%endif

	call bb_writepoints
	jnc .retn

	sub sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
				; reserve space for bb error info
	mov bp, sp
		; cx = index of failed point
		;    = index above last one to restore
		; ax = error info of failed point
	push ax
	push cx
	call bb_restorepoints_and_init_error_info
.sym_error_done:

bb_restorepoints_exit: equ $
		; If this is not gg and T/P wrote a proceed breakpoint,
		;  restore it here (after having restored bb points).
	call proceed_writepoint_restore
		; This call might return modeswitched.
	mov bx, 0		; (preserve CF)
	jnc @F
	or ah, 80h		; mark error during restoration
	mov bx, ax		; bx & 80h set: error restoring pp
@@:

	pop cx
	pop ax			; error info + index of failed point

	mov si, cx
	add si, si
	add si, si		; * 4
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, cx		; * 5
%elif BPSIZE == 6
	add si, cx		; * 5
	add si, cx		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, cx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, b_bplist.bp	; -> point

	push bx

	call put_deferred_message_silent
				; CHG: dx

	cmp cx, -1
	je @F

		; si-> point
		; ax = info (ah = reason, al = new value if reason 3)
		; cx = 0-based index of initially failed point
	 push word [si + 2]
	 push word [si]		; stack: linear address
	mov bx, 4000h		; bh = 40h (bb),
				;  bl = what we tried to restore (n/a)
	call display_breakpoint_failure
		; This function calls resetmode.

@@:
	call bb_handlefailedrestore
		; This function calls resetmode.

	pop ax
	test ah, 80h		; pp failed to restore ?
	jz @F

	 push word [tpg_proceed_bp + 2]
	 push word [tpg_proceed_bp]
	mov bl, [tpg_proceed_bp + BPSIZE - 1]
	mov bh, 0		; proceed breakpoint
	call display_breakpoint_failure
@@:
	add sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
				; discard bb error info
	jmp cmd3

.retn:
	retn
%endif


%if _NUM_G_BP
		; INP:	ss:bp -> behind gg error info space
		;	cx = number of error info words on stack
		; OUT:	error info space initialised to all zeros
		; CHG:	ax, di, es
		; STT:	sets es to ss
gg_restorepoints_init_error_info:
	push ss
	pop es
	mov di, bp		; es:di -> behind error info
	push cx
	dec di
	dec di
	xor ax, ax
	std			; _AMD_ERRATUM_109_WORKAROUND does not apply
	rep stosw		; initialize error info
	cld
	pop cx
	retn


gg_restorepoints_and_init_error_info:
	call gg_restorepoints_init_error_info

		; Restore gg breakpoints.
		; On errors remember failures but restore all remaining anyway.
		;
		; INP:	cx = number of breakpoints to restore (<= 255),
		;		assumed at beginning of g_bplist.bp
		;	ss:bp -> behind cx words for error info
		; OUT:	NC if all points restored successfully
		;	CY if at least one point couldn't be restored,
		;	 error info filled, high byte:
		;	  reason =	0 = no error (this point didn't fail),
		;			1 = couldn't write,
		;			2 = unreachable,
		;			3 = overwritten),
		;	  low byte: new byte value (if reason 3)
		; CHG:	ax, bx, (e)dx, si, di, es
		; STT:	sets es to ss
		;	might return modeswitched
		;
		; Note:	The points are restored in reverse, from the last back
		;	 to the front. The first point is handled last.
gg_restorepoints:
	mov si, cx
	add si, si
	add si, si		; * 4
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, cx		; * 5
%elif BPSIZE == 6
	add si, cx		; * 5
	add si, cx		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, cx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, g_bplist.bp - BPSIZE	; -> last point in list (first to restore)

	clc			; assume success
	push cx
	pushf
	jcxz .done		; nothing to do -->
.loop:
	call gg_writepoints_restore	; Restore breakpoint bytes.
	jnc .done		; successful -->
	pop dx			; discard flags
	pop bx
	push bx
	pushf			; store (CY) flags

	push ax
	mov di, si
	sub di, BPSIZE+g_bplist.bp
%if BPSIZE == 5 || BPSIZE == 6 || BPSIZE == 9
	mov ax, di
	xor dx, dx
	mov di, BPSIZE
	div di
	mov di, ax		; di = 0-based point index
	shl di, 1		; di = 0-based error info offset
%elif BPSIZE == 4
	shr di, 1		; di = 0-based error info offset
%else
 %error "Unexpected BPSIZE"
%endif
	sub di, bx
	sub di, bx
	pop word [bp+di]	; store error info
	dec cx			; exclude the failed point
	sub si, 2*BPSIZE	; -> the point _before_ failed one
	jmp short .loop
.done:
	popf			; get flags. CY if any couldn't be restored
	pop cx			; restore cx
	retn
%endif


%if _BREAKPOINTS
		; INP:	ss:bp -> error info space (one word per bb breakpoint)
		; OUT:	error info space initialised to all zeros
		; CHG:	ax, di, es
		; STT:	sets es to ss
		;
		; Note:	This initialises words for all bb points.
		;	 While the value in cx is preserved, it is
		;	 assumed that space for all points is allocated.
bb_restorepoints_init_error_info:
	push ss
	pop es
	mov di, bp
	xor ax, ax
	push cx
	mov cx, _NUM_B_BP + _NUM_SYM_BP
	rep stosw
	pop cx
	retn


bb_restorepoints_and_init_error_info:
	call bb_restorepoints_init_error_info

		; Restore bb breakpoints.
		; On errors remember failures but restore all remaining anyway.
		;
		; INP:	cx = index above last one to restore
		;	ss:bp -> error info space (one word per bb breakpoint)
		; OUT:	NC if all points restored successfully
		;	CY if at least one point couldn't be restored,
		;	 error info filled, high byte:
		;	  reason =	0 = no error (this point didn't fail),
		;			1 = couldn't write,
		;			2 = unreachable,
		;			3 = overwritten),
		;	  low byte: new byte value (if reason 3)
		; CHG:	ax, bx, cx, (e)dx, si, di, es
		; STT:	sets es to ss
		;	might return modeswitched
		;
		; Note:	The points are restored in reverse, from the list back
		;	 to the front. The first point is handled last.
bb_restorepoints:

	clc			; assume success
	pushf
	jcxz .done
.loop:
	call bb_writepoints_restore
	jnc .done
	pop dx			; (discard flags)
	pushf			; store (CY) flags
				; cx = index of point that failed to write
				; ah = reason, al = new byte value (reason 3)
	mov di, cx
	add di, di
	mov word [bp + di], ax	; store error info
	jmp .loop

.done:
	popf			; CF
	retn


		; Loop through bb breakpoints and exchange the saved
		; byte with that one at the actual address. Used to write
		; the breakpoints.
		;
		; INP:	-
		; OUT:	NC if successful
		;	CY if error writing a point,
		;	 cx = index of point that failed to write
		;	 (all PRIOR points were processed successfully,
		;	  either written successfully or skipped)
		; CHG:	ax, bx, (e)dx, si, cx, di
		; STT:	might return modeswitched
bb_writepoints:
	xor cx, cx
	mov di, 1
	db __TEST_IMM16		; (skip xor, NC)

		; Same, but go through the breakpoints in reverse order
		; and check that what we overwrite is a 0CCh byte. If so,
		; restore the original value. (The 0CCh is discarded.)
		;
		; INP:	cx = index *above* last to write
		;	 (_NUM_B_BP + _NUM_SYM_BP for all)
		; OUT:	NC if successful
		;	CY if error writing a point,
		;	 cx = index of point that failed to write
		;	 ah = 1 if error because point could not be written
		;	 ah = 2 if error because address is unreachable
		;	 ah = 3 if error because point contained non-0CCh value,
		;	  al = new byte
		; CHG:	ax, bx, (e)dx, si, cx, di
		; STT:	might return modeswitched
bb_writepoints_restore:
	xor di, di		; (NC)
bb_wp:
	lframe near
	lenter
	lvar	word, is_write
	 push di

	test byte [bp + ?is_write], 1
				; (NC) is it writing ?
	jz .next		; no, is restoring, first decrement cx -->

.loop:
	mov ax, cx
	call calcpointbit	; bx = index, ah = value
	test byte [b_bplist.used_mask+bx], ah
				; (NC)
	jz .next
	test byte [b_bplist.disabled_mask+bx], ah
				; (NC)
	jnz .next

	mov si, cx
	add si, si
	add si, si		; * 4
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, cx		; * 5
%elif BPSIZE == 6
	add si, cx		; * 5
	add si, cx		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, cx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, b_bplist.bp	; -> point

	call gg_bb_lods_bp_linear
				; dx:ax = linear address

	call gg_bb_check_is_first
	jnc .next		; (NC)

	call getsegmented	; bx:(e)dx = segmented address
%if BPSIZE == 6
	lodsw			; skip word offset
%elif BPSIZE == 9
	lodsw
	lodsw			; skip dword offset
%endif
	lodsb			; get byte to write
	mov ah, 2
	jc .return		; not in PM anymore/address not available --> (CY)
	test byte [bp + ?is_write], 1
				; writing?
	jnz .forward_nocheck	; yes -->

.backward_check:
	push ax
	call readmem		; read current byte
	cmp al, 0CCh		; is this still what we wrote?
	mov ah, 83h		; (80h = error occurred while restoring)
	stc
	jne .return_discard	; nope --> (CY)
	pop ax
	call writemem		; return the byte to its original value
	jc .next		; failed --> (CY, handled there)
	mov byte [si-1], 0CCh	; reset stored point
	jmp short .next

.forward_nocheck:
%if _DELAY_BEFORE_BP
	call delay_before_bp
%endif
	call writemem
	jc .next
	mov byte [si-1], al	; save the previous byte there
.next:
	mov ah, 1		; (in case of error)
	jc .return		; failed to write --> (CY)

	test byte [bp + ?is_write], 1
	jnz .is_write_next
	dec cx			; restore: decrement index
	jns .loop		; decremented to 0FFFFh ?  no, loop -->
	jmp .return_NC

.is_write_next:
	inc cx			; write: increment index
	cmp cx, _NUM_B_BP + _NUM_SYM_BP
				; above last ?
	jb .loop		; no, loop -->
.return_NC:
	clc
.return:
.return_discard:
	jnc .ret

	test byte [bp + ?is_write], 1
				; restoring ?
	jnz .ret_CY		; no -->
	or ah, 80h		; error occurred while restoring
.ret_CY:
	stc
.ret:
	lleave
	lret
%endif


%if _NUM_G_BP
		; Loop through saved breakpoints and exchange the saved
		; byte with that one at the actual address. Used to write
		; the breakpoints.
		;
		; INP:	si-> current point
		;	cx = number of points to write (might be zero)
		; OUT:	NC if successful
		;	CY if error writing a point,
		;	 cx = number of points still to write (including failed one)
		;	 (si-BPSIZE)-> point that failed
		; CHG:	ax, bx, (e)dx, si, cx, di
gg_writepoints:
	mov di, 1
	db __TEST_IMM16		; (skip xor, NC)

		; Same, but go through the breakpoints in reverse order
		; and check that what we overwrite is a 0CCh byte. If so,
		; restore the original value. (The 0CCh is discarded.)
		;
		; Additionally:
		; OUT:	CY if error writing a point,
		;	 ah = 1 if error because point could not be written
		;	 ah = 2 if error because address is unreachable
		;	 ah = 3 if error because point contained non-0CCh value,
		;	  al = new byte
gg_writepoints_restore:
	xor di, di		; (NC)
gg_wp:
	jcxz .return		;if nothing to do --> (still NC from xor/test)
.loop:
	call gg_bb_lods_bp_linear
				; dx:ax = linear address

	call gg_bb_check_is_first
	jc @F			; if to handle this breakpoint -->

		; Skip to next breakpoint.
%if BPSIZE == 4 || BPSIZE == 5
	inc si			; -> after point
%elif BPSIZE == 6
	add si, 3		; skip word offset and byte content
%elif BPSIZE == 9
	add si, 5		; skip dword offset and byte content
%endif
				; BPSIZE implied

	test di, di		; (NC)
	jnz .next		; going forward -->
	jmp .next_lea_si	; (NC)

		; Handle this breakpoint.
@@:
	call getsegmented	; bx:(e)dx = segmented address
%if BPSIZE == 6
	lodsw			; skip word offset
%elif BPSIZE == 9
	lodsw
	lodsw			; skip dword offset
%endif
	lodsb			; get byte to write
	mov ah, 2
	jc .return		; not in PM anymore/address not available --> (CY)
	test di, di		; writing?
	jnz .forward_nocheck	; yes -->

.backward_check:
	push ax
	call readmem		; read current byte
	cmp al, 0CCh		; is this still what we wrote?
	mov ah, 83h		; (80h = error occurred while restoring)
	stc
	jne .return_discard	; nope --> (CY)
	pop ax
	call writemem		; return the byte to its original value
	jc .next		; failed --> (CY, handled there)
	mov byte [si-1], 0CCh	; reset stored point
.next_lea_si:
	lea si, [si-2*BPSIZE]	; adjust (for reverse writing)
	jmp short .next

.forward_nocheck:
%if _DELAY_BEFORE_BP
	call delay_before_bp
%endif
	call writemem
	mov byte [si-1], al	; save the previous byte there
.next:
	mov ah, 1		; (in case of error)
	jc .return		; failed to write --> (CY)
	loop .loop
.return:			; (NC)
	jnc .ret

	test di, di		; restoring ?
	jnz .ret_CY		; no -->
	or ah, 80h		; error occurred while restoring
.ret_CY:
	stc
	push dx			; (counteract effect of pop)
.return_discard:
	pop dx
.ret:
	retn
%endif


		; INP:	dx:ax = linear address of point to be (re)set
		;	dword [internalflags2]
		;	dword [gg_first_cseip_linear]
		; OUT:	CY if to (re)set breakpoint
		;	 if: no flag set
		;	 or: dx:ax doesn't match, dif2_gg_skip_non_cseip clear
		;		(setting non-cseip point)
		;	 or: dx:ax matches, dif2_gg_skip_cseip clear
		;		(setting cseip point afterwards)
		;	NC if to not (re)set breakpoint
		;	 if: dx:ax doesn't match, dif2_gg_skip_non_cseip set
		;		(not setting non-cseip point afterwards)
		;	 or: dx:ax matches, dif2_gg_skip_cseip set
		;		(not setting cseip point)
gg_bb_check_is_first:
	testopt [internalflags2], \
		dif2_gg_is_first | dif2_gg_skip_cseip | dif2_gg_skip_non_cseip
	jz .continue

	cmp [gg_first_cseip_linear], ax
	jne .is_not_first
	cmp [gg_first_cseip_linear + 2], dx
	jne .is_not_first

.is_first:
	setopt [internalflags2], dif2_gg_first_detected
	testopt [internalflags2], dif2_gg_skip_cseip
	jnz .skip
.continue:
	stc
	retn

.is_not_first:
	testopt [internalflags2], dif2_gg_skip_non_cseip
	jz .continue
.skip:
	clc
	retn


		; INP:	al = number to put
		; OUT:	putsline called with ordinal string
		; CHG:	di, [line_out], ax
putordinalbyte:
	push bx
	push cx
	push dx

		; Note that trim_overflow in front of line_out is
		;  initialised to '0', which we depend on here.
		;  With the output from decbyte, at least two decimal
		;  digits are always valid in the buffer.
	mov di, line_out
	call ordinalbyte
	call putsline
	pop dx
	pop cx
	pop bx
	retn


ordinalbyte:
	call decbyte
	mov ax, [di - 2]	; al = lower address, higher decimal digit
				; ah = higher address, lower decimal digit
	cmp al, '1'
	je .gotsuf_th		; 11th, 12th, 13th or other teen -->
	mov dx, "st"
	cmp ah, '1'
	je .gotsuf		; if low digit was one -->
	mov dx, "nd"
	cmp ah, '2'
	je .gotsuf		; if low digit was two -->
	mov dl, 'r'
	cmp ah, '3'
	je .gotsuf		; if low digit was three -->
.gotsuf_th:
	mov dx, "th"
.gotsuf:
	mov ax, dx
	stosw
	retn


%if _BREAKPOINTS
		; INP:	ss:bp -> error info (points at first info word)
		;	 Each word provides information for one point we tried
		;	 to restore. The lowest info word corresponds to the
		;	 first bb breakpoint (bb 00), followed by the info word
		;	 for the second bb breakpoint (bb 01), and so on.
		;	 There are always as many info words as there are
		;	 bb breakpoints.
		; CHG:	ax, bx, cx, dx, di, si
		; STT:	sets es to ss
bb_handlefailedrestore:
%if _PM
	call resetmode
%endif
	xor di, di
	xor cx, cx
	push ss
	pop es
	mov si, b_bplist.bp
.loop:
	mov ax, word [bp + di]
	test ah, 7Fh		; failed ?
	jz .next
	push cx
	push di
				; cx = 0-based index
	 push word [si + 2]
	 push word [si]		; stack: linear address
	mov bl, byte [si + BPSIZE - 1]
				; bl = what we tried to restore
	mov bh, 40h		; bh = 40h (bb)
	or ah, 80h		; ah & 80h = set (is restore)
	call display_breakpoint_failure
	pop di
	pop cx
.next:
	add si, BPSIZE
	scasw			; di += 2
	inc cx
	cmp cx, _NUM_B_BP + _NUM_SYM_BP
	jb .loop
.end:
	retn
%endif


%if _NUM_G_BP
		; INP:	cx = number of restored points (number of info words)
		;	ss:bp-> error info (points *behind* last info word)
		;	 Each word provides information for one point we tried
		;	 to restore. The lowest info word corresponds to the
		;	 first gg breakpoint, followed by the info word for
		;	 the second gg breakpoint (if any), and so on.
		; CHG:	ax, bx, cx, dx, di, si
		; STT:	sets es to ss
gg_handlefailedrestore:
%if _PM
	call resetmode
%endif
	mov di, cx
	add di, di
	neg di
	push ss
	pop es
	mov bx, 0
	mov si, g_bplist.bp
	jcxz .end
.loop:
	mov ax, word [bp+di]
	test ah, 7Fh		; failed?
	jz .next		; no -->

		; si-> point
		; ax = info (ah = reason, al = new value if reason 3)
		; bx = point's 0-based index
.display:
	push bx
	push di
	mov cx, bx		; cx = 0-based index
	 push word [si + 2]
	 push word [si]		; stack: linear address
	mov bl, byte [si + BPSIZE - 1]
				; bl = what we tried to restore
	mov bh, 80h		; bh = 80h (gg)
	or ah, 80h		; ah & 80h = set (is restore)
	call display_breakpoint_failure
	pop di
	pop bx
.next:
	inc bx			; increment counter
	add si, BPSIZE		; -> next point
	inc di
	inc di			; di+bp-> next error info
	jnz .loop		; not yet at end -->
.end:
	retn
%endif	; _NUM_G_BP


		; INP:	word [reg_cs]
		;	(d)word [reg_eip]
		; OUT:	bx = word [reg_cs]
		;	CY if invalid address
		;	NC if address is valid,
		;	 dx:ax = linear address
		; CHG:	edx, ax, ecx
get_cseip_minus_1_linear:
	_386_PM_o32		; or ecx, byte -1
	or cx, byte -1

		; INP:	word [reg_cs]
		;	(d)word [reg_eip]
		;	(e)cx = adjustment to (e)ip
		; OUT:	bx = word [reg_cs]
		;	CY if invalid address
		;	NC if address is valid,
		;	 dx:ax = linear address
		; CHG:	edx, ax
get_cseip_ecx_linear:
	mov bx, [reg_cs]	; bx = cs
	_386_PM_o32
	mov dx, [reg_eip]	; (e)dx = (e)ip
	_386_PM_o32
	add dx, cx		; bx:(e)dx = adjusted cs:(e)ip
			; (getlinear doesn't use the high word of edx if it's a 16-bit cs)
	; call getlinear	; dx:ax = linear address of this cs:eip
	; (fall through to getlinear_d_b)


		; INP:	If currently in RM,
		;	 bx:dx = segment:offset of address
		;	If currently in PM,
		;	 bx:(e)dx = selector:offset of address
		; OUT:	bx = unchanged (selector/segment)
		;	CY if address is invalid,
		;	 because Int31.0006 failed for bx or
		;	 because 32-bit address overflowed or
		;	 because A20 could not be switched on
		;	NC if address is valid,
		;	 dx:ax = linear address
		;	 dh is always zero if the address is in RM address space
		; CHG:	dx, ax
%if _PM
getlinear_d_b: section_of_function
	push di
	mov di, test_d_b_bit
	call getlinear_common
	pop di
	retn

getlinear_high_limit: section_of_function
	push di
	mov di, test_high_limit
	call getlinear_common
	pop di
	retn

getlinear_32bit: section_of_function
	push di
	mov di, .always_NZ
	call getlinear_common
	pop di
	retn

.always_NZ:
	test di, di
	retn

section_of getlinear_16bit
getlinear_16bit: section_of_function
	push di
	mov di, .always_ZR
	call getlinear_common
	pop di
	retn

.always_ZR:
	cmp di, di
	retn

 %if _DEBUG1
getlinear_high_limit.do_not_use_test: section_of_function
		; DO NOT use resetmode, called by readmem/writemem
	push di
	mov di, test_d_b_bit
	call getlinear_common.do_not_use_test
	pop di
	retn
 %endif
%else
check_section_of getlinear_d_b
check_section_of getlinear_high_limit
check_section_of getlinear_32bit
getlinear_d_b: equ getlinear_common
getlinear_high_limit: equ getlinear_common
getlinear_32bit: equ getlinear_common
getlinear_16bit: equ getlinear_common
 %if _DEBUG1
check_section_of getlinear_high_limit.do_not_use_test
getlinear_high_limit.do_not_use_test: equ getlinear_common.do_not_use_test
 %endif
%endif


		; Internal entry: Call function di to determine use
		;  of a 32-bit offset, INP: bx = seg/sel, OUT: NZ iff 32-bit
getlinear_common: section_of_function
%if _PM
	call resetmode
				; This must execute in the correct mode,
				;  because we get the input from whatever
				;  mode we were originally entered in.
%endif
%if _DEBUG1
	call .do_not_use_test	; get linear address
	jc @F			; already an error ?  then return -->
	push bx
	push cx
	mov bx, test_records_getLinear
	call handle_test_case_multiple_16
				; check whether this should testcase the error
				; CY to indicate error from this call
	pop cx
	pop bx
@@:
	retn

%endif
		; DOES NOT use resetmode
.do_not_use_test:
	 push bx
%if _PM
	call ispm
	jnz .rm
	mov ax, 0006h
	 push cx
	push dx
	int 31h			; get selector base address into cx:dx
	pop ax			; (edxh:)ax = offset, cx:dx = base
	jc .return_cx_bx

_386	call di
_386	jz .16
_386	push edx
_386	pop bx
_386	pop bx			; bx = high word edx (in 32-bit PM segment)
_386	jmp .32
.16:				; bx:ax = offset
	xor bx, bx		; 16-bit PM segment, clear offset high word
.32:
	add ax, dx
	adc bx, cx		; add the base
	xchg bx, dx		; dx:ax = 32-bit linear address
	clc

.return_cx_bx:
	 pop cx
	 pop bx
	retn
.rm:
%endif
	mov ax, bx
	push cx
	mov cl, 4
	rol ax, cl
	pop cx
	mov bx, 0FFF0h
	and bx, ax		; bx = low word adjustment
	and ax, byte 0Fh	; ax = higher bits which don't fit in the low word
	add dx, bx
	adc ax, byte 0
	xchg ax, dx		; dx:ax = 21-bit linear address
	testopt [internalflags], debuggeeA20
	jnz .return_bx		; A20 line enabled, no need to adjust (NC)
	and dl, ~10h		; clear corresponding bit of the address if it was set (NC)
.return_bx:
	 pop bx
	retn


		; INP:	bx = segment/selector to use by default
		;	al = next character in input
		;	si -> following character in input
		; OUT:	NC if successful,
		;	 bx:dx = linear address
		;		(if not _PM, this always fits in 24 bits)
		;	 al = next character
		;	 si -> following character
		;	 (d)word [bp_offset] = preferred offset, -1 if none
		;	CY if error
		; CHG:	edx, bx, ax, si, (d)word [bp_offset]
getlinearaddr: section_of_function
%if _PM
	call resetmode
%endif
	_386_PM_o32	; or dword
	or word [bp_offset], strict byte -1
	call skipcomm0
	cmp al, '@'
	jne .at_not
	lodsb			; insure we have a blank or opening parens
	cmp al, '('
	je .at
	cmp al, 9
	je .at
	cmp al, 32
	je .at
	jmp .at_not_reload	; assume the @ is part of a symbol

.at:
	call skipwh0
	cmp al, '('
	jne .at_not_paren

	call getdword
	call skipwh0
	cmp al, ')'
	jne .error
	lodsb

	jmp .at_got

.at_not_paren:
	call getdword
.at_got:
%ifn _PM
	; test bh, bh
	; jnz .error
	cmp bx, 11h
	jae .error
%endif
	clc
	retn

.at_not_reload:
	dec si
	dec si
	lodsb
.at_not:
	call getaddrX
%if _PM
_no386	and word [bp_offset + 2], 0
%endif
	_386_PM_o32		; mov dword [bp_offset], edx
	mov word [bp_offset], dx
	push ax
	call getlinear_high_limit
	xchg bx, ax	; dx:bx = linear
	xchg dx, bx	; bx:dx = linear
	pop ax		; al = next character
	retn

.error:
	stc
	retn


	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
bp_offset:	dw 0
%if _PM
		dw 0
%endif
	usesection lDEBUG_CODE


		; INP:	word [reg_cs]
		;	(d)word [reg_eip]
		;	dword [tpg_possible_breakpoint]
		;	[internalflags2] & dif2_tpg_have_bp
		; OUT:	CY if invalid address
		;	NC if address is valid,
		;	 dx:ax = linear address
		; CHG:	edx, ax, ecx, bx
		; STT:	ds = ss = debugger data selector
get_cseip_of_possible_breakpoint:
	mov ax, [tpg_possible_breakpoint]
	mov dx, [tpg_possible_breakpoint + 2]
			; dx:ax = stored possible breakpoint address
	testopt [internalflags2], dif2_tpg_have_bp
	jnz .retn	; (NC) if already stored -->
	call get_cseip_minus_1_linear
			; dx:ax = linear address at cs:(e)ip - 1
	jc .retn	; (CY) if getlinear failed -->
	mov [tpg_possible_breakpoint], ax
	mov [tpg_possible_breakpoint + 2], dx
			; store for later use
	setopt [internalflags2], dif2_tpg_have_bp
			; (NC)
.retn:
	retn


%if _PM

; DEBUGX: when a mode switch from PM to RM did occur in the debuggee,
; breakpoint addresses in non-RM address space are unreachable.
; To enable the debugger to reset the breakpoints, it has to switch
; temporarily to PM. (In DEBUG/X 1.19, a switch from RM to PM in the
; debuggee will temporarily switch the debugger back to RM too. We
; handle this case without a switch as the linear RM address can be
; accessed directly in PM, assuming that the DPMI hosts linear-to-
; physical mapping for the RM address space is the same as the one
; we get in PM.)

; (To use mode switching in any case, the format the breakpoints are
; stored in needs to be modified. Additionally, the mode switching
; needs to be done elsewhere instead of in getsegmented. resetmode
; can be used as is, even if a mode switch from PM to RM occured.)

switchmode:
;--- raw switch:
;--- si:e/di: new cs:e/ip
;--- dx:e/bx: new ss:e/sp
;--- ax:      new ds
;--- cx:      new es
_386	xor ebx, ebx		; clear ebxh
	mov bx, sp
_386	xor edi, edi		; clear edih
	mov di, .back_after_switch
	call ispm
	jz .pm
.r86m:
d4	call d4message
d4	asciz "In switchmode.r86m",13,10
	mov ax, [dssel]		; switch rm -> pm
	mov si, [code_sel]
	mov dx, ax
	mov cx, ax
	jmp far [dpmi_rm2pm]
.pm:
d4	call d4message
d4	asciz "In switchmode.pm",13,10
	mov ax, [pspdbg]	; switch pm -> rm
	mov si, [code_seg]
	mov dx, ax
	mov cx, ax
	_386_o32	; jmp far dword [dpmi_pm2rm]
	jmp far [dpmi_pm2rm]

.back_after_switch:
	xoropt [internalflags], protectedmode
d4	call d4message
d4	asciz "In switchmode.back_after_switch",13,10
	retn


		; INP:	al = direction, 0 = save state, 1 = restore state
		;	[auxbuff] = state buffer
		;	ES = debugger data selector
		; CHG:	edi
sr_state:
	testopt [internalflags], switchbuffer
	jz .return		; no need to call -->
	_386_o32	; xor edi, edi
	xor di, di
	call ispm
	jz .pm
.r86m:
d4	call d4message
d4	asciz "In sr_state.r86m",13,10
	mov es, word [auxbuff_segorsel + soaSegment]
	call far [dpmi_rmsav]
	jmp .return

.pm:
d4	call d4message
d4	asciz "In sr_state.pm",13,10
	mov es, word [auxbuff_segorsel + soaSelector]
	_386_o32	; call far dword [dpmi_pmsav]
	call far [dpmi_pmsav]
.return:
	push ss
	pop es
	retn


		; INP:	[internalflags] & modeswitched
		; OUT:	flag cleared, mode switched back if had been switched
		;	EI
		; CHG:	- (not even flags!)
		; STT:	es = ds = ss = gs = fs = debugger data segment/selector
		;
		; Switches back to RM if a mode switch to PM was done by getsegmented.
		; Does nothing otherwise. (Can be called multiple times.)
resetmode:
	pushf
	testopt [internalflags], modeswitched	; switched mode previously ?
	jz .return		; no -->
d4	call d4message
d4	asciz "In resetmode (actually resetting)",13,10
subcpu 286
	_386_o32
	pusha
	call switchmode 	; switch back to the mode we were entered in (RM)
	mov al, 1
	call sr_state		; restore state
	call handle_mode_changed		; ! called with flag set
	clropt [internalflags], modeswitched	; reset the flag
_386	push ss
_386	pop gs
_386	push ss
_386	pop fs
	_386_o32
	popa
subcpureset
.return:
	popf
	sti
	retn
%endif

		; INP:	dx:ax = linear address
		; OUT:	CY if inaccessible
		;	NC if accessible,
		;	 bx:(e)dx = segment:offset or selector:offset address
		;	 A temporary mode switch from RM to PM might have occured.
		;	 This has to be reset by calling resetmode.
		; CHG:	(e)ax, (e)bx, (e)dx, fs, gs
		; STT:	es = ds = ss = our segment/selector
		;
		; It is assumed that only one byte needs to be accessed.
		; (A workaround to access multiple bytes would be to access each
		;  byte separately and call this function for each of the bytes.)
		;
		; The TSR command code assumes that getsegmented.pm does not switch
		; modes and that it will return an offset in (e)dx that allows
		; 16-bit access. Adjust that code if either is no longer true.
		;
		; This is often used in conjunction with writemem or
		;  readmem. Refer to debug.asm for those. Note that
		;  they will decide whether to use edx or dx depending
		;  on the segment limit of the selector returned in
		;  bx. Currently edxh is always zero so it doesn't
		;  matter whether writemem or readmem choose a32 or
		;  a16 addressing.
getsegmented:
%if _DEBUG1
	push bx
	push cx
	mov bx, test_records_getSegmented
	call handle_test_case_multiple_16
	pop cx
	pop bx
	jnc @F
	; stc			; (already CY if the conditional didn't jump)
	retn

@@:
%endif
.do_not_use_test:

%if _PM
	call ispm
	jnz .rm
.pm:
	push cx
	mov bx, word [scratchsel]
	xchg ax, dx
	xchg ax, cx		; cx:dx = linear address
	mov ax, 0007h
	int 31h			; set base to the address
	pop cx
	jc .invalid
 %if 1
	push cx
	mov ax, 0008h
	xor cx, cx
	xor dx, dx
	int 31h			; set limit to zero (access to one byte only)
	pop cx
	jc .invalid
 %endif
	_386_o32	; xor edx, edx
	xor dx, dx		; bx:(e)dx = selector:offset (using scratchsel with the address as base)
	retn			; (NC)
%endif
.rm:
	cmp dx, byte 10h	; check for address space
	jb .nothma		; below HMA, normal access -->
	je .checkhma		; possibly in HMA -->
.nonrmspace:			; above HMA
%if _PM
		; It's a PM address but we are in RM.
	testopt [internalflags], canswitchmode
	jz .invalid		; can't switch to PM -->
.switchmodes:
	_386_o32
	push si
	_386_o32
	push di
	_386_o32
	push cx
	_386_o32
	push bp
	push dx
	push ax
d4	call d4message
d4	asciz "In getsegmented.switchmodes (switching to access memory beyond 1088 KiB)",13,10
	call remember_mode
	setopt [internalflags], modeswitched	; set flag for resetmode
	mov al, 0
	call sr_state		; save state
	call switchmode 	; switch to PM
	call handle_mode_changed		; ! called with flag set
	pop ax
	pop dx
	_386_o32
	pop bp
	_386_o32
	pop cx
	_386_o32
	pop di
	_386_o32
	pop si
	jmp .pm

.invalid:			; the address is inaccessible
%endif
	stc
	retn

.checkhma:
	cmp ax, -10h		; valid offset for HMA ?
	jae .nonrmspace		; no, above HMA -->
	testopt [internalflags], debuggerA20	; A20 on, HMA accessible ?
	jz .nonrmspace		; no, treat as above HMA (DEBUGX) -->
	mov bx, 0FFFFh		; the HMA must always be addressed by segment FFFFh
	add ax, byte 10h	; and the offset is always at least 0010h (FFFFh:0010h = 00100000h)
		; (NC because we checked that this won't overflow)
	xchg ax, dx		; bx:dx = segment:offset
	jmp .zero_edxh

.nothma:
	push cx
	mov cl, 4
	ror dx, cl		; dx (high 4 bits) = high 4 bits of segment
	mov bx, ax
	shr bx, cl		; bx = low 12 bits of segment
	or dx, bx		; dx = segment
	and ax, byte 0Fh	; ax = low 4 bits of linear, used as offset (NC)
	xchg ax, dx
	xchg ax, bx		; bx:dx = segment:offset
	pop cx
.zero_edxh:
_386_PM	movzx edx, dx
	retn


		; T command - Trace.
tt:
	mov ah, al
	and ah, TOUPPER
	cmp ah, 'M'
	jnz isnotmodeset
	call skipcomma
	call iseol?
	je ismodeget
	call getword
	call chkeol		; expect end of line here
	cmp dx, 1
	ja error
	je .set			; selected 1 -->
.clear:				; selected 0
	clropt [options], traceints
	jmp short .get
.set:
	setopt [options], traceints
.get:
ismodeget:
	mov al, '0'
	mov si, tmode0
	testopt [options], traceints
	jz .zero
	inc ax
	mov si, tmode1
.zero:
	mov byte [tmodev], al
	mov di, line_out
	push si
	mov si, tmodes
	call showstring
	pop si
	call showstring
	jmp putsline_crlf

isnotmodeset:
%if _TTEST
	push ax
	cmp ah, 'T'
	jne @F
	mov ax, word [si]
	and ax, TOUPPER_W
	cmp ax, "ES"
	jne @F
	mov al, byte [si + 2]
	and al, TOUPPER
	cmp al, 'T'
	je ttestcmd
@@:
	pop ax
%endif
%if _TSR
	cmp ah, 'S'
	jne @F
	push ax
	mov al, byte [si]
	and al, TOUPPER
	cmp al, 'R'
	pop ax
	je tsr
%endif
@@:
	clropt [internalflags], tt_p
	cmp ah, 'P'
	jne @F
	lodsb
	setopt [internalflags], tt_p
@@:
tt0:
	mov word [lastcmd], tt0
	clropt [internalflags2], dif2_is_pp
	call parse_pt		; process arguments

		; Do it <BX:CX=count> times.
tt1:
	push bx
	push cx

	testopt [internalflags], tt_p
	jz .not_p

%if _PM
	call resetmode
%endif
	mov dx, 15		; DL = number of bytes to go; DH = prefix flags.
	mov bx, word [reg_cs]
	_386_PM_o32	; mov esi, dword [reg_eip]
	mov si, word [reg_eip]
.pp2:
	call pp16		; get next instruction byte into AL
	mov di, ppbytes
	mov cx, PPLEN_ONLY_STRING
%if _SYMBOLIC
	mov byte [pp_instruction], al
%endif
	repne scasb
	jne .not_p		; if not one of these -->
	mov al,byte [di+PPLEN-1]; get corresponding byte in ppinfo
	test al, PP_PREFIX	; prefix ?
	jz .pp3			; no -->
	or dh, al		; set the OSIZE or ASIZE flags if either of these
			; Note:	Multiple OSIZE in a 16-bit cs do not toggle
			;	between decoding as O32 and O16, they're always
			;	decoded as O32. The same is true for A32, and
			;	in a 32-bit cs for O16 and A16.
	dec dl
	jnz .pp2		; if not out of bytes -->
	mov dx, msg.warnprefix
	call putsz
	jmp .not_p

		; A repeatable string instruction is to be decoded.
		; Finish the decoding and skip the appropriate number
		; of opcode bytes.
.pp3:
_386_PM	call pp_fix32bitflags
	test al, PP_VARSIZ | PP_SIZ_MASK
	jnz error
%if 0
	test al, PP_VARSIZ	; different opcode length depends on OSIZE ?
	jz .ignoreosize		; no -->
	and dh, 2
	add al, dh
.ignoreosize:
	and ax, PP_SIZ_MASK
_386_PM	movzx eax, ax		; clear high word (in case it counts)
	_386_PM_o32	; add esi, eax
	add si, ax
%endif
; pp10:
%if _SYMBOLIC
	call pp3_check_symhints
	jc .not_p		; trace -->
%endif
	; jmp short pp11	; we have a skippable instruction here
; pp11:
_386_PM	call resetmode_and_test_d_b_bit
_386_PM	jnz .32			; full 32-bit offset valid -->
_386_PM	movzx esi, si		; clear high word here
.32:
	call proceedbreakpoint	; run until the breakpoint is hit
		; This call might return modeswitched.
	jmp short @F

.not_p:
	call traceone		; call common code
@@:
	jc unexpectedinterrupt	; an unexpected interrupt occured -->
%if _BREAKPOINTS
	mov dx, 0		; do not skip WHILE
	jz @F			; (breakpoint after instruction was hit)

		; bb breakpoint was hit. dumpregs, then return
	call handle_bb_hit_pass_match
	jc .actual_hit		; actual match ? -->
	jz tt2.nodump		; non-silent mode ? -->
	jmp tt2			; silent mode -->

.actual_hit:
%if _PM
	call resetmode
%endif
	call put_deferred_message_silent
	call dumpregs_extended_silent
	pop cx
	pop bx			; (discard counter)
	jmp @FF
@@:
%endif
tt2:
	push dx
%if _PM
	call resetmode
%endif
	call put_deferred_message_silent
	call dumpregs_extended_silent
				; dump registers
	pop dx			; preserve skip WHILE flag
.nodump:

	call tt_handle_while
	pop cx
	pop bx

	sub cx, 1
	sbb bx, 0		; decrement loop counter

	test bx, bx
	jnz tt1_jmp		; loop -->
	test cx, cx
	jnz tt1_jmp		; loop -->

@@:
%if _PM
	call resetmode
%endif
	call silence_dump

	retn

tt1_jmp:
	jmp tt1


%if _TTEST
ttestcmd:
	pop ax
	inc si
	inc si
	inc si
	call skipwhite

.tt0:
	mov word [lastcmd], .tt0
	clropt [internalflags2], dif2_is_pp
	call parse_pt		; process arguments

		; Do it <BX:CX=count> times.
.tt1:
	push bx
	push cx

.tt2:
%if _PM
	call resetmode
%endif
	call put_deferred_message_silent
	call dumpregs_extended_silent
				; dump registers
.nodump:

	xor dx, dx
	call tt_handle_while
	pop cx
	pop bx

	sub cx, 1
	sbb bx, 0		; decrement loop counter

	test bx, bx
	jnz .tt1_jmp		; loop -->
	test cx, cx
	jnz .tt1_jmp		; loop -->

@@:
%if _PM
	call resetmode
%endif
	call silence_dump

	retn

.tt1_jmp:
	jmp .tt1
%endif


		; INP:	dx = nonzero if to bypass while condition
tt_handle_while:
	test dx, dx
	jnz @F
	testopt [internalflags], tt_while
	jz @F

%if _PM
	call resetmode
%endif
	call .copy_condition_to_line_out
				; dx = si -> line_out with condition
	push word [rc]
	pop word [priorrc]
	lodsb
	call getexpression	; parse stored expression
	call chkeol
	call toboolean		; get boolean
	test dx, dx		; true ?
	jnz @F			; yes, continue -->

	call silence_dump

	mov dx, msg.while_terminated_before
	call putsz
	call .copy_condition_to_line_out
	call putsz		; display condition
	mov dx, msg.while_terminated_after
	call putsz
	jmp cmd3

@@:
	retn


.copy_condition_to_line_out:
	mov es, word [auxbuff_segorsel]
	xor di, di		; -> stored expression (if not PM)
%if _PM
	add di, word [auxbuff_switchbuffer_size]
				; -> stored expression
%endif
	mov si, di
	mov cx, -1
	mov al, 0
	repne scasb
	not cx			; = length, including zero terminator
	push es
	pop ds			; ds:si -> auxbuff stored expression
	 push ss
	 pop es
	mov di, line_out	; -> line_out
	push di
	rep movsb		; move over
	pop si

	mov dx, si
	 push ss
	 pop ds			; -> line_out
	retn


		; INP:	cl = flags indicating what kind of bb match occurred
		;	 cl & 1 set if actual hit,
		;	 else cl & 2 set if pass match,
		;	 else it is a non-pass non-hit match
		;	(cl & 4 always set (indicates any bb match))
		;	 cl & 8 set if pp/tt breakpoint hit
		;	dword [ss:sp] = command repetition counter
		; OUT:	NC if pass match or non-pass non-hit match,
		;	 if pass match: deferred message output, dumpregs output
		;	 if cl & 8 set (pass/nonpass bb, tt/pp hit):
		;	  dx = 0
		;	  ZR if [internalflags] & tt_silent_mode clear
		;	  NZ if [internalflags] & tt_silent_mode set
		;	 if cl & 8 clear (pass/nonpass bb, no tt/pp hit):
		;	  ZR
		;	  dword [ss:sp] increased by 1
		;	  dx = 1 (indicates to skip WHILE condition)
		;	CY if actual hit (hit bb),
		;	 dx = 0
		;	ax = INP:cx
		; CHG:	dx, bx, cx, si, di, all high words, fs, gs
		; STT:	ds = es = ss
handle_bb_hit_pass_match:
d5	call d5message
d5	asciz "in handle_bb_hit_pass_match",13,10

	xchg ax, cx

	testopt [internalflags2], dif2_bp_failure
	jnz .actual_hit		; after failure, handle as actual hit

	test al, 1		; actual hit ?  else: non-hit, pass or non-pass
	jnz .actual_hit		; yes -->

	test al, 2		; at least pass match ?
	jz .nonpassnonhit	; no -->

.passnonhit:

d5	call d5message
d5	asciz "bb pass non-hit",13,10

%if _PM
	call resetmode
%endif
	; call put_deferred_message_loud
	mov dx, msg.empty_message
	xchg dx, word [gg_deferred_message]
	call putsz
	mov dx, putsline
	call put_bb_deferred_message_calling_dx

	push ax
	; call dumpregs_extended_loud
				; (includes handle_serial_flags_ctrl_c)
	call dumpregs_extended
	pop ax			; (preserve so the test al, 8 can use it)
.nonpassnonhit:
	call handle_serial_flags_ctrl_c

	; al & 4 always set

	clropt [internalflags2], \
		dif2_gg_skip_non_cseip | dif2_gg_skip_cseip | dif2_gg_first_detected
	setopt [internalflags2], dif2_gg_is_first

	push word [gg_next_cseip_linear + 2]
	push word [gg_next_cseip_linear]
	pop word [gg_first_cseip_linear]
	pop word [gg_first_cseip_linear + 2]

	test al, 8		; not bb hit, is tt/pp hit ?
	jnz @FF			; yes -->

	testopt [internalflags2], dif2_is_pp | dif2_tpg_keep_proceed_bp, 1
	jnz @F
	jmp @FF

@@:
	lframe near
	lpar dword,	counter
	lpar_return
	lenter
	add word [bp + ?counter], 1
	adc word [bp + ?counter + 2], 0
	lleave
	mov dx, 1		; skip WHILE
	cmp al, al		; (NC, ZR)
	retn

@@:
	xor dx, dx		; no skip WHILE
	test al, 2		; displayed pass point ?
	jnz @F			; yes -->
	test al, -1		; NC, NZ (do not skip dump)
		; al & 0Ch is set so this test is NZ
	retn

@@:
	testopt [internalflags], tt_silent_mode
				; (NC, ZF is ZR if to skip dump)
	retn

.actual_hit:
d5	call d5message
d5	asciz "bb hit",13,10

	xor dx, dx		; no skip WHILE
	stc
	retn


%if _TSR
tsr:
	call guard_re
	inc si
	lodsb
	call chkeol
	testopt [internalflags], tsrmode
	jz .try			; not yet resident -->
	mov dx, msg.alreadytsr
	jmp short .putsz
.try:
	mov dx, word [pspdbe]
	mov bx, word [pspdbg]
	mov di, 0Ah
.loop:
%if _PM
	mov cx, dx		; = original segment address
	call ispm
	jnz .rm
	push bx
	mov ax, dx
[cpu 286]
	shr dx, 12
	shl ax, 4		; shift to get a 32-bit linear address
__CPU__
	call getsegmented.pm	; set up selector for access
		; This call makes some assumptions:
		; - No mode switch occurs; we are still in PM.
		;    Currently none is performed from PM.
		; - (e)dx isn't larger than about FFD0h.
		;    Currently scratchsel with an offset of zero is returned.
	call setrmlimit
	mov di, dx		; -> PSP
	mov dx, bx
	pop bx
	add di, byte 0Ah	; -> PSP termination vector
.rm:
%endif
	mov es, dx
	mov ax, word [es:(di-0Ah)+16h]
	inc ax
	jz .pspnotfound
	dec ax
	jz .pspnotfound		; parent is invalid -->
%if _PM
	cmp ax, cx
%else
	cmp ax, dx
%endif
	je .pspnotfound		; parent is the process itself -->
	mov dx, ax
	cmp ax, bx
	jne .loop		; parent is not us -->
	cmp word [es:di], int22
	jne .psphooked
	cmp word [es:di+2], bx
	je .found		; correct vector --> (otherwise: hooked)
.psphooked:
	mov dx, msg.psphooked
	jmp short .putsz_es
.pspnotfound:
	mov dx, msg.pspnotfound
.putsz_es:
	push ss
	pop es
.putsz:
	jmp putsz

.found:
	mov si, psp22
	movsw
	movsw			; write our parent's vector
	add di, 16h-(0Ah+4)
	movsw			; write our parent
	setopt [internalflags], tsrmode	; note that we are resident
%if _PM
	push cx
%else
	push es
%endif
	mov dx, msg.nowtsr1
	call .putsz_es
	pop ax
	mov di, line_out
	call hexword
	call putsline
	mov di, psp22
	xor ax, ax
	stosw
	stosw
	stosw			; clear our parent/int22 fields
	mov dx, msg.nowtsr2
	jmp short .putsz
%endif


		; INP:	dx -> message, zero-terminated
		; CHG:	-
		; OUT:	message displayed or put into silent buffer
		; STT:	ds = es = ss = debugger data selector
putsz_silent:
	call silence_init
	call putsz		; print string

silence_exit:
	testopt [internalflags], tt_silent_mode
	jz @F
	clropt [internalflags], tt_silence
@@:
	retn


silence_init:
	testopt [internalflags], tt_silent_mode
	jz @F
	setopt [internalflags], tt_silence
@@:
	retn


		; INP:	es:di -> behind message in line_out
		; CHG:	ax, bx, cx, dx
		; OUT:	message displayed or put into silent buffer
		; STT:	ds = es = ss = debugger data selector
putsline_silent:
	call silence_init
	call putsline
	jmp silence_exit


		; INP:	word [run_int]
		;	InDOS status
		; STT:	es = ds = ss
		; OUT:	dx -> last message
		;	message displayed
		; CHG:	ax, di
putrunint:
%if _DEBUG && _DEBUG_COND
	testopt [options6], opt6_debug_putrunint
	jz @F
	testopt [internalflags6], dif6_debug_mode
	jnz @F
	call reset_interrupts
	setopt [internalflags6], dif6_debug_mode
	setopt [options6], opt6_debug_mode
@@:
%endif

%if _DEBUG
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jz @F
 %endif
	testopt [options6], opt6_debug_putrunint_early
	jz @F
	int3
	nop
@@:
%endif

%if _AREAS_HOOK_SERVER
	_386_PM_o32		; push ecx
	push cx
	push bx
	_386_PM_o32		; push esi
	push si

	mov ax, word [run_int]
	cmp ax, int1msg
	je .end_j
	cmp ax, int3msg
	je .end_j
%if _CATCHINT19
	cmp ax, int19msg
	je .end_j
%endif
	cmp ax, progtrm
	jne @F
.end_j:
	jmp .end

@@:
	_386_PM_o32		; xor ecx, ecx
	xor cx, cx
	call get_cseip_ecx_linear
	mov bx, dx		; bx:ax = linear cs:eip
	push ss
	pop es
	mov di, ddebugareas
	xor cx, cx

.loop:
%if _PM
	mov dx, word [es:di + areastrucNext + 2]
	mov di, word [es:di + areastrucNext]
	call setes2dx
%else
	les di, [es:di + areastrucNext]
	mov dx, es
%endif
	cmp dx, word [pspdbg]
	je .end

	push dx
	push di
	push cx
	push bx
	push ax

	push di
	mov cx, word [es:di + areastrucFunAmount]
	mov di, word [es:di + areastrucFunOffset]
	jcxz .noareafun
.loopareafun:
	cmp bx, word [es:di + areastrucfunLinear + 2]
	jne @F
	cmp ax, word [es:di + areastrucfunLinear]
@@:
	jb .nextareafun

	cmp bx, word [es:di + areastrucfunLinearEnd + 2]
	jne @F
	cmp ax, word [es:di + areastrucfunLinearEnd]
@@:
	jae .nextareafun

	push bx
	push ax
	push cx
	push di

	sub ax, word [es:di + areastrucfunLinear]
	sbb bx, word [es:di + areastrucfunLinear + 2]
	jnz .nextareafun_pop

	mov cx, word [es:di + areastrucfunListAmount]
	push cx
	mov di, word [es:di + areastrucfunListOffset]
	repne scasw
	pop cx
	jne .nextareafun_pop
	add cx, cx
	dec di
	dec di
	add di, cx
_386_PM	xor ecx, ecx
	mov cx, word [es:di]
	_386_PM_o32
	mov si, word [reg_esp]
	_386_PM_o32
	add si, cx
	push ds
%if _PM
	mov bx, word [reg_ss]
	call test_d_b_bit
	mov ds, bx
	jz .16
.32:
	_386_PM_a32
%else
	mov ds, word [reg_ss]
%endif
.16:
	lodsw
	pop ds
.gotfunction:
	pop di
	pop cx
	pop bx			; (discard)
	pop bx

	xchg dx, ax
	mov bx, word [reg_cs]	; bx:dx = cs:ip
	call getlinear_16bit	; dx:ax = linear
	mov bx, dx		; bx:ax = linear
	jmp .noareafun

.nextareafun_pop:
	pop di
	pop cx
	pop ax
	pop bx
.nextareafun:
	add di, AREASTRUCFUN_size
	loop .loopareafun
.noareafun:
	pop di

	mov cx, word [es:di + areastrucSubAmount]
	mov di, word [es:di + areastrucSubOffset]
	jcxz .noareasub
.loopareasub:
	cmp bx, word [es:di + areastrucsubLinear + 2]
	jne @F
	cmp ax, word [es:di + areastrucsubLinear]
@@:
	jb .nextareasub

	cmp bx, word [es:di + areastrucsubLinearEnd + 2]
	jne @F
	cmp ax, word [es:di + areastrucsubLinearEnd]
@@:
	jae .nextareasub

	push bx
	push ax
	push cx
	push di

	sub ax, word [es:di + areastrucsubLinear]
	sbb bx, word [es:di + areastrucsubLinear + 2]
	jnz .nextareasub_pop

	mov dx, ax		; = ip
	mov si, word [es:di + areastrucsubListOffset]
	mov cx, word [es:di + areastrucsubListAmount]
.looparea:
	es lodsw
	cmp dx, ax
	es lodsw
	jb .nextarea
	cmp dx, ax
	jae .nextarea
	es lodsw
	mov bx, ax		; es:bx -> area message
	xchg ax, dx		; es:dx -> area message

		; If we are displaying to a serial port then
		;  we assume that we are not sharing the output
		;  device with an areas hook client. Therefore
		;  we want to skip the initial linebreal in the
		;  areas message if there is one.
		; The linebreaks are intended to separate (shared)
		;  output from prior output of the code that caused
		;  the fault, eg interactive E mode prompt or
		;  partial disassembly output.
	cmp word [es:bx], 13 | (10 << 8)
				; is it with an initial linebreak ?
	jne @F			; no -->
	testopt [options], enable_serial
	jz @F			; if not serial -->
	inc dx
	inc dx			; skip the linebreak
@@:
	 push es
	 pop ds			; => our area hook client's data selector
	call putsz		; display ds:dx -> ASCIZ message
	 push ss
	 pop ds			; reset ds
	pop di
	pop cx
	pop ax
	pop bx

	pop ax
	pop bx
	pop cx
	pop di
	pop dx

	jmp .end

.nextarea:
	add si, 4
	loop .looparea

.nextareasub_pop:
	pop di
	pop cx
	pop ax
	pop bx
.nextareasub:
	add di, AREASTRUCSUB_size
	loop .loopareasub
.noareasub:

.next:
	pop ax
	pop bx
	pop cx
	pop di
%if _PM
	pop dx
	call setes2dx
%else
	pop es
%endif
	loop .loop_j
	jmp .end

.loop_j:
	jmp .loop

.end:
	_386_PM_o32		; pop esi
	pop si
	pop bx
	_386_PM_o32		; pop ecx
	pop cx
%endif
	 push ss
	 pop es

.noarea:
	mov dx, word [run_int]
	cmp dx, progtrm
	jne .done

	mov ax, -1
	call InDos
	jnz .no_int21_4D

	mov ah, 4Dh
	int 21h
.no_int21_4D:
	mov word [usertermcode], ax
	mov di, progexit
	call hexword
.done:
	jmp putsz


	usesection lDEBUG_DATA_ENTRY
	align 2, db 0
usertermcode:	dw 0
	usesection lDEBUG_CODE


		; Print message about unexpected interrupt, dump registers, and
		; end command. This code is also used by the G and P commands.
unexpectedinterrupt:
%if _PM
	call resetmode
%endif
	call silence_init
	call putrunint
	call silence_exit
%if _CATCHINT19
	cmp dx, int19msg
	je .noregs		; if it terminated, skip the registers
%endif
	cmp dx, progtrm
	je .noregs		; if it terminated, skip the registers
	call dumpregs_extended_silent
.noregs:
	call silence_dump

	testopt [internalflags2], dif2_gg_is_gg
	jz @F
	testopt [options], gg_unexpected_no_repeat
	jmp @FF
@@:
	testopt [options], tp_unexpected_no_repeat
@@:
	jz @F
	mov word [lastcmd], dmycmd
@@:

	jmp cmd3		; back to the start


		; Trace an instruction.
		; INP:	word [reg_cs], dword [reg_eip], other register values
		; OUT:	NC if the breakpoint or trace interrupt was hit,
		;	 ah = 0
		;	 ch = 0
		;	 d[reg_eip] adjusted if a breakpoint (bb) hit
		;	 cx & 8 set if trace interrupt hit
		;		(assumed to be the expected trace hit)
		;	 (ZF only set if _BREAKPOINTS)
		;	 ZR if trace interrupt was hit,
		;	  cx & 7 = 0
		;	 NZ if a breakpoint (bb) was hit (or both were hit),
		;	  cx & 1 set if non-pass match (actual hit),
		;	   else cx & 2 set if pass match (consider as hit first,
		;		but dump registers next (not to silent buffer)
		;		and then continue execution)
		;	   else cx & 4 always set, indicates any match
		;		(including matches that should merely continue)
		;	  all pass points' counters stepped
		;	CY if no breakpoint and no trace interrupt was hit,
		;	 cx = 0
		;	If [internalflags2] & dif2_gg_is_gg is set:
		;	 ah & 7Fh = status =	0 = no error,
		;				1 = couldn't write,
		;				2 = unreachable,
		;				3 = overwritten, al = new value
		;	 ah & 80h = set if error restoring point,
		;			else error writing point to begin with
		;	If that flag is clear:
		;	Does not return if a breakpoint cannot be written
		;	 or cannot be restored, jumps to cmd3 instead.
		; CHG:	all
		; STT:	ds = es = ss
		;	might return modeswitched (if dif2_gg_is_gg)
		;	might be called while modeswitched
traceone:
	testopt [internalflags2], dif2_tpg_keep_proceed_bp
	jnz .proceedbreakpoint

%if _PM
	call resetmode
%endif
	xor cx, cx
	call getcseipbyte
	cmp al, 0CDh		; int opcode?
	jne .isstdtrace		; no -->
	inc cx
	call getcseipbyte
	cmp al, 3
	je .isstdtrace		; int 3 opcode -->
	testopt [options], traceints	; TM=1?
	jz isstdtraceX
%if _IMMASM
	testopt [internalflags6], dif6_immasm
	jnz isstdtraceX
%endif
	cmp al, 1
	je .isstdtrace		; int 1 opcode -->

		; TM==1, single-step into the INT
	mov bl, al
%if _PM
	call ispm
	jz .singlestep_into_interrupt_pm
%endif
	mov bh, 0
	push ds
	xor ax, ax
	mov ds, ax
	shl bx, 1
	shl bx, 1
	lds si, [bx]
	cli
	mov al, byte [si]
	inc byte [si]
	cmp byte [si], al
	mov byte [si], al
	sti
	mov bx, ds		; bx:si-> interrupt handler (RM, 16 bit)
	mov ax, bx
	pop ds
	jne .singlestep_into_interrupt_setbp

		; The interrupt handler is in a ROM.
%if 0
	jmp short isstdtraceX
%else
	xchg si, word [reg_eip]
	xchg ax, word [reg_cs]	; get cs:ip, set interrupt handler address
	mov cx, word [reg_efl]	; get flags
	push ds
	mov bx, word [reg_esp]
	mov ds, word [reg_ss]	; ds:bx-> debuggee stack
	sub bx, byte 6		; reserve enough space for flags, cs, ip
	inc si
	inc si			; skip CDh xxh opcode
	mov word [bx+4], cx
	mov word [bx+2], ax
	mov word [bx+0], si	; save flags, cs, ip on stack
	pop ds
	mov word [reg_esp], bx	; update sp
	and byte [reg_efl+1], ~(2|1)	; clear IF and TF (NC)
			; Note: If invalid flag values were previously set by the user
			; by directly accessing the FL or EFL register, these won't be
			; fixed by us now. This could be worked around by executing a
			; NOP in debuggee's environment (or only with debuggee's flags)
			; first, but I don't think it's much of an issue.
	mov word [run_int], int1msg
	clropt [internalflags2], \
		dif2_tpg_have_bp | dif2_tpg_adjusted_cseip \
		| dif2_tpg_do_not_adjust | dif2_tpg_keep_proceed_bp, 1
	mov cx, 8
	xor ax, ax		; NC, ZR
	retn

%endif
%if _PM
.singlestep_into_interrupt_pm:
	mov ax, 0204h
	int 31h
	mov bx, cx
	_386_o32	; mov esi, edx
	mov si, dx		; bx:(e)si-> interrupt handler
	test bl, 4		; is it a LDT selector? (NC)
	jz isstdtraceX		; no -->
%endif
.singlestep_into_interrupt_setbp:
.proceedbreakpoint:
	jmp proceedbreakpoint

.isstdtrace:
%if _PM
	call ispm
	jz .notdpmientry	; already in PM -->
	mov ax, w[reg_eip]	; is this a switch to protected mode ?
	cmp ax, w[dpmiwatch+0]
	jne .notdpmientry
	mov ax, w[reg_cs]
	cmp ax, w[dpmiwatch+2]
	je isdpmientry		; yes, catch it --> (this means really "go")
.notdpmientry:
%endif
	or byte [reg_efl+1], 1	; set single-step mode (cleared when returning into DEBUG)
	xor cx, cx
	call skipprefixes
	cmp al, 9Ch		; opcode "PUSHF"?
	jnz .notpushf
%if _BREAKPOINTS
	call run_with_bb
	mov ah, 0
	pushf
%else
	call run
	xor cx, cx
	xor ax, ax
%endif
		; Clear TF in the fl word or efl dword
		; pointed to by debuggee's ss:(e)sp
	push es
%if _PM
	mov bx, word [reg_ss]	; get ss selector into bx
	mov es, bx
_386	call test_d_b_bit	; check whether a 32-bit ss
%else
	mov es, word [reg_ss]
%endif
	_386_PM_o32	; mov ebx, dword [reg_esp]
	mov bx, word [reg_esp]	; es:(e)bx-> debuggee's stack
_386_PM	jz .pushf_16
_386_PM	and byte [es:ebx+1], ~1	; clear TF
_386_PM	jmp short .pushf_common
.pushf_16:
	and byte [es:bx+1], ~1	; clear TF
.pushf_common:
	pop es
	jmp short .checkreturn
.notpushf:
%if _BREAKPOINTS
	call run_with_bb
	mov ah, 0
	pushf
%else
	call run
	xor cx, cx
	xor ax, ax
%endif
.checkreturn:
	cmp word [run_int], int1msg
	jne .nomatch
	or cl, 8
%if _BREAKPOINTS
	popf			; CF
	jnc .ret_NZ_NC
%endif
	cmp al, al		; if correct interrupt (ZR, NC)
	retn

.nomatch:
%if _BREAKPOINTS
	popf			; CF
	jnc .ret_NZ_NC
%endif
	stc
	retn

%if _BREAKPOINTS
.ret_NZ_NC:
	or cl, 4		; (NZ, NC)
	retn
%endif


; an INT is to be processed (TM is 0)
; to avoid the nasty x86 bug which makes IRET
; cause a debug exception 1 instruction too late
; a breakpoint is set behind the INT

isstdtraceX:
	mov cx, 2
	call iswriteablecseip	; is it ROM ?
	jc traceone.isstdtrace	; is read-only -->
	mov bx, word [reg_cs]

		; (e)si = (e)ip + 2
		; We don't test whether it's a 32-bit code segment here.
		; The previous code would leave the high word of esi uninitialized then.
	_386_PM_o32	; mov esi, dword [reg_eip]
	mov si, word [reg_eip]
	_386_PM_o32	; add esi, byte 2
	add si, byte 2		; ! do not remove the byte override, else o32 won't work
	jmp proceedbreakpoint	; set BP at BX:(E)SI and run debuggee

		; Call getcseipbyte and loop while increasing cx if the returned
		; byte was a prefix. Returns the first non-prefix byte (an opcode)
		; in al. (WAIT or FWAIT is not considered a prefix because it's
		; really an opcode and we also trace it without executing a
		; following FPU opcode.)
skipprefixes:
.:
	call getcseipbyte
	cmp al, 26h
	je .prefix		; ES
	cmp al, 2Eh
	je .prefix		; CS
	cmp al, 36h
	je .prefix		; SS
	cmp al, 3Eh
	je .prefix		; DS
	cmp al, 0F0h
	je .prefix		; LOCK
	cmp al, 0F3h
	je .prefix		; REPZ
	cmp al, 0F2h
	je .prefix		; REPNZ
	_no386_jmps .noprefix	; no 386, following aren't prefixes (invalid opcodes on 186+) -->
	cmp al, 64h
	je .prefix		; FS
	cmp al, 65h
	je .prefix		; GS
	cmp al, 66h
	je .prefix		; o32/o16
	cmp al, 67h
	jne .noprefix		; not a32/a16
.prefix:
	inc cx
	; jmp short .
	jns .			; this is not correct but serves as hack to avoid an infinite loop
				; (note that getcseipbyte really uses cx as signed number)
	dec cx			; back to 07FFFh
.noprefix:
	retn


;--- test if memory at CS:E/IP can be written to
;--- return C if not

iswriteablecseip:
	call getcseipbyte	; get byte at CS:EIP+CX
	mov ah, al
	xor al, 0FFh
	call setcseipbyte
	jc .return
	call getcseipbyte
	cmp ah, al     		; is it ROM?
	stc
	jz .return
	mov al, ah
	call setcseipbyte
	clc
.return:
	retn


%if _CATCHINT07 || _CATCHINT0C || _CATCHINT0D
	usesection lDEBUG_DATA_ENTRY

r86m_debugger_exception:
	cli
	mov ax, cs
	mov ds, ax			; => debugger data segment
	pop word [exception_csip]	; ip
	pop word [exception_csip + 2]	; cs
	pop dx				; get rid of flags
 %if _AREAS
	cld
	mov es, ax
	mov si, sp
	mov di, exception_stack
	mov cx, 8
	rep movsb
 %endif
	mov ss, ax
%ifn _ONLY386
..@patch_no386_ds_6_DATA_ENTRY:
	o32			; mov esp, dword [savesp]
%endif
	mov sp, word [savesp]
	times 1 - (($ - $$) & 1) nop	; align in-code parameter
	call entry_to_code_seg
	dw .code

	usesection lDEBUG_CODE
.code:
	sti
%endif
%if _PM || _CATCHINT07 || _CATCHINT0C || _CATCHINT0D
debuggerexception:
	cld
	push ss
	pop ds
%if _DEBUG && _DEBUG_COND
	testopt [options6], opt6_debug_exception
	jz @F
	testopt [internalflags6], dif6_debug_mode
	jnz @F
	call reset_interrupts
	setopt [internalflags6], dif6_debug_mode
	setopt [options6], opt6_debug_mode
@@:
%endif

%if _DEBUG
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jz @F
 %endif
	testopt [options6], opt6_debug_exception_early
	jz @F
	int3
	nop
@@:
%endif
	call unhack		; sets es to ss

%if _AREAS
	mov bx, cs
	cmp bx, word [exception_csip + 2]
	jne .unknownarea
	mov ax, word [exception_csip]
	mov di, areafunctions
	mov cx, areafunctions.amount
	repne scasw
	jne .unknownfunction
	mov bx, exception_stack
	add bx, word [di - areafunctions - 2 + areafunctions.skip]
	mov ax, [bx]
.gotfunction:

.unknownfunction:
	mov dx, ax		; = ip
	mov si, areas
	mov cx, areas.amount
.looparea:
	lodsw
	cmp dx, ax
	lodsw
	jb .nextarea
	cmp dx, ax
	jae .nextarea
	lodsw
	xchg ax, dx
	call putsz
	jmp .donearea
.nextarea:
	lodsw
	lodsw
	loop .looparea

.donearea:
.unknownarea:
%endif
	call putrunint.noarea
%if _EXCCSIP
	mov di, exccsip
	mov ax, word [exception_csip + 2]
	call hexword
	inc di
	mov ax, word [exception_csip]
	call hexword

	mov dx, excloc
	call putsz
%endif
%if _DEBUG
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jz @F
 %endif
	testopt [options6], opt6_debug_exception_late
	jz @F
	int3
@@:
%endif
	jmp cmd3
%endif	; _PM || _CATCHINT07 || _CATCHINT0C || _CATCHINT0D


%if _AREAS
	usesection lDEBUG_DATA_ENTRY

%imacro areadefine 1-2.nolist
	dw ..@%1_start
	dw ..@%1_end
%ifempty %2
	dw msg.area_%1
%else
	dw msg.area_%2
%endif
	dw 0
%endmacro

%define AREAFUNCTIONS ""
%define AREAFUNCTIONSSKIP ""
%imacro areafunctiondefine 2.nolist
%xdefine AREAFUNCTIONS AREAFUNCTIONS,%1
%xdefine AREAFUNCTIONSSKIP AREAFUNCTIONSSKIP,%2
%endmacro

	align 8, db 0
areas:
.:
		; More specific areas first!
		;  (The first range match wins.)
	areadefine ee_interactive_access
	areadefine hh_indirection_memory_access
	areadefine rr_variable_read_access
	areadefine rr_variable_write_access
	areadefine uu_referenced_memory_access
	areadefine uu_simulate_scas
	areadefine uu_simulate_cmps
	areadefine aa_access
	areadefine dd_access
	areadefine ee_access
	areadefine rr_access
	areadefine run_access
	areadefine uu_access
.end:
.amount: equ (.end - .) / 8

	areafunctiondefine ..@readmem_fault_skip_4_near_call, 4
	areafunctiondefine ..@writemem_fault_skip_4_near_call_a, 4
	areafunctiondefine ..@writemem_fault_skip_4_near_call_b, 4
	areafunctiondefine ..@getcseipbyte_fault_skip_2_near_call, 2
	areafunctiondefine ..@setcseipbyte_fault_skip_2_near_call, 2
%if _PM
	areafunctiondefine ..@readmem_fault_skip_2_near_call, 2
	areafunctiondefine ..@writemem_fault_skip_2_near_call_a, 2
	areafunctiondefine ..@writemem_fault_skip_2_near_call_b, 2
	areafunctiondefine ..@getcseipbyte_fault_skip_6_near_call, 6
	areafunctiondefine ..@setcseipbyte_fault_skip_6_near_call, 6
%endif


	align 2, db 0
areafunctions:
.:
	dw AREAFUNCTIONS
.end:
.amount: equ (.end - .) / 2
.skip:
	dw AREAFUNCTIONSSKIP


%if _AREAS_HOOK_CLIENT
areas_struc:
	istruc AREASTRUC
at areastrucEntry
	stc
	retf
at areastrucNext,	dd -1
at areastrucPrev,	dd -1
at areastrucSubAmount,	dw 1
at areastrucSubOffset,	dw areas_sub
at areastrucFunAmount,	dw 1
at areastrucFunOffset,	dw areas_fun
	iend

areas_sub:
	istruc AREASTRUCSUB
at areastrucsubLinear,		dd 0
at areastrucsubLinearEnd,	dd ldebug_code_size
at areastrucsubListOffset,	dw areas
at areastrucsubListAmount,	dw areas.amount
	iend

areas_fun:
	istruc AREASTRUCFUN
at areastrucfunLinear,		dd 0
at areastrucfunLinearEnd,	dd ldebug_code_size
at areastrucfunListOffset,	dw areafunctions
at areastrucfunListAmount,	dw areafunctions.amount
	iend
%endif

	usesection lDEBUG_CODE
%endif


%if _PM
[cpu 386]
		; INP:	dh = flags as for pp2,pp3,pp5 (1 = ASIZE, 2 = OSIZE)
		; OUT:	dh = flags as used by pp3,pp5 (1 = A32, 2 = O32)
pp_fix32bitflags:
	call test_d_b_bit
	jz .16
	xor dh, 1|2		; toggle OSIZE and ASIZE (once)
.16:
	retn
__CPU__
%endif

		; P command - proceed (i.e., skip over call/int/loop/string instruction).
pp:
	mov word [lastcmd], pp
	setopt [internalflags2], dif2_is_pp
	call parse_pt		; process arguments

		; Do it <BX:CX=count> times.  First check the type of instruction.
pp1:
	push bx
	push cx			; save bx:cx
%if _PM
	call resetmode
%endif
	mov dx, 15		; DL = number of bytes to go; DH = prefix flags.
	mov bx, word [reg_cs]
	_386_PM_o32	; mov esi, dword [reg_eip]
	mov si, word [reg_eip]
pp2:
	call pp16		; get next instruction byte into AL
	mov di, ppbytes
	mov cx, PPLEN
	mov byte [pp_instruction], al
	repne scasb
	jne pp5			; if not one of these -->
	mov al,byte [di+PPLEN-1]; get corresponding byte in ppinfo
	test al, PP_PREFIX	; prefix ?
	jz pp3			; no -->
	or dh, al		; set the OSIZE or ASIZE flags if either of these
			; Note:	Multiple OSIZE in a 16-bit cs do not toggle
			;	between decoding as O32 and O16, they're always
			;	decoded as O32. The same is true for A32, and
			;	in a 32-bit cs for O16 and A16.
	dec dl
	jnz pp2			; if not out of bytes -->
	mov dx, msg.warnprefix
	call putsz
	jmp pp12

		; A repeatable string, interrupt, call immediate or loop
		; instruction is to be decoded. Finish the decoding and
		; skip the appropriate number of opcode bytes.
pp3:
_386_PM	call pp_fix32bitflags
	_386_PM_o32
	mov word [pp_operand], si
	test al, PP_VARSIZ	; different opcode length depends on OSIZE ?
	jz .ignoreosize		; no -->
	and dh, 2
	add al, dh
.ignoreosize:
	and ax, PP_SIZ_MASK
_386_PM	movzx eax, ax		; clear high word (in case it counts)
	_386_PM_o32	; add esi, eax
	add si, ax
	jmp pp10


pp5:
_386_PM	call pp_fix32bitflags
	cmp al, 0FFh		; FF/2 or FF/3 indirect NEAR or FAR call ?
	jne pp12		; no, just an ordinary instruction -->
	call pp16		; get MOD REG R/M byte
	and al, ~ (1 << 3)	; clear lowest bit of REG field (/3 to /2)
	xor al, 2 << 3		; /3 or /2 to /0
	test al, 7 << 3
	jnz pp12		; if not FF/2 or FF/3 -->
	cmp al, 0C0h		; mod = 3 ?
	jae .adjust0		; if just a register -->
	test dh, 1
	jnz .a32		; if 32-bit addressing -->
	cmp al, 6		; mod = 0 r/m = 6 would encode bp ?
	je .adjust2		; if just plain disp16 -->
	cmp al, 40h
	jb .adjust0		; if indirect register -->
	cmp al, 80h
	jb .adjust1		; if disp8[reg(s)]
	jmp short .adjust2	; it's disp16[reg(s)]

		; Handle 32-bit addressing (A32 ModR/M referencing memory)
.a32:
	cmp al, 5		; mod = 0 and r/m = 5 (would encode ebp) ?
	je .adjust4		; if just plain disp32 -->
	xor al, 4		; 4 to 0 (r/m 4 would encode esp)
	test al, 7		; 0 if r/m would encode esp
	jnz .a32_nosib		; if no SIB byte -->
	xchg al, ah
	call pp16
	xchg al, ah		; load and skip the SIB byte
		; The SIB byte is only used here to detect the
		;  special case encoding of disp32 with mod=0
		;  and base=5. index=4 is also special but it
		;  does not alter the size of the displacement
		;  that we have to skip.
	test al, 0C0h		; is it mod = 0 ?
	jnz @F			; no, not a special case -->
	and ah, 7
	cmp ah, 5		; is it base = 5 ?
	je .adjust4		; yes, special case encoding of disp32 -->
@@:
.a32_nosib:
	cmp al, 40h
	jb .adjust0		; if indirect register -->
	cmp al, 80h
	jb .adjust1		; if disp8[reg(s)] -->
				; otherwise, it's disp32[reg(s)]
.adjust4:
	_386_PM_o32	; inc esi
	inc si			; skip an instruction byte
	_386_PM_o32	; inc esi
	inc si			; skip an instruction byte
.adjust2:
	_386_PM_o32	; inc esi
	inc si			; skip an instruction byte
.adjust1:
	_386_PM_o32	; inc esi
	inc si			; skip an instruction byte
.adjust0:

pp10:
	_386_PM_o32
	push si

	cmp byte [pp_instruction], 0E8h
	jne .done

	_386_PM_o32
	mov si, word [pp_operand]

_386_PM	xor eax, eax
	call pp16
	xchg al, ah
	call pp16
	xchg al, ah
_386_PM	jmp @F
	test dh, 2
	jnz .notcallrel16
@@:
_386_PM	test dh, 2
_386_PM	jz @F

_386_PM	rol eax, 16
_386_PM	call pp16
_386_PM	xchg al, ah
_386_PM	call pp16
_386_PM	xchg al, ah
_386_PM	rol eax, 16
@@:
	_386_PM_o32
	add si, ax
	mov dl, 15		; number of bytes to go
.loop:
	call pp16
	cmp al, 0CBh		; retf ?
	je .trace
	cmp al, 0CAh		; retf imm16 ?
	je .trace
	cmp al, 0CFh		; iret ?
	je .trace
	mov di, ppbytes
	mov cx, PPLEN_ONLY_PREFIXES
	repne scasb
	jne .done
				; if one of the prefixes
	dec dl
	jnz .loop		; if not out of bytes -->

				; fall through: do not trace
.notcallrel16:
.done:
	db __TEST_IMM8		; (skip stc, NC)
.trace:
	stc			; trace
	_386_PM_o32
	pop si

	jc pp12			; trace -->
%if _SYMBOLIC
	call pp3_check_symhints
	jc pp12			; trace -->
%endif
	; jmp pp11		; we have a skippable instruction here
pp11:
_386_PM	call test_d_b_bit
_386_PM	jnz .32			; full 32-bit offset valid -->
_386_PM	movzx esi, si		; clear high word here
.32:
@@:
	call proceedbreakpoint	; run until the breakpoint is hit
	jc pp15			; unexpected -->
%if _BREAKPOINTS
	jnz pp12.bb_hit
%endif
	xor dx, dx		; do not skip WHILE
	jmp short pp13

pp12:
	testopt [internalflags2], dif2_tpg_keep_proceed_bp
	jnz @B

	call traceone
	jc pp15
	; jc unexpectedinterrupt

%if _BREAKPOINTS
	mov dx, 0		; do not skip WHILE
	jz @F

.bb_hit:
	call handle_bb_hit_pass_match
	jc .actual_hit
d5	call d5message
d5	asciz "in pp12.bb_hit after non-hit",13,10
	jz pp13.nodump
	jmp pp13


.actual_hit:
%if _PM
	call resetmode
%endif
	call put_deferred_message_silent
	call dumpregs_extended_silent
	pop cx
	pop bx			; (discard counter)
	jmp @FF

@@:
%endif

		; We could check here for the correct address too, but that
		; would require disassembling the instruction and correctly so.
		; (Disassembling it incorrectly would only result in spurious
		; "Unexpected single-step interrupt" messages aborting multi-
		; traces though, so it won't be fatal.)
		; Wouldn't really be useful though: Only the "int1" or "int 01h"
		; instructions should cause this, and their operation means we
		; might as well behave as if the breakpoint was expected.
	cmp word [run_int], int1msg
	jne pp15		; if some other interrupt (unexpected) -->

pp13:
	push dx
%if _PM
	call resetmode
%endif
		; An expected breakpoint. Dump registers, then loop.
	call put_deferred_message_silent
	call dumpregs_extended_silent
	pop dx			; preserve skip WHILE flag

.nodump:
	call tt_handle_while
	pop cx
	pop bx

	sub cx, 1
	sbb bx, 0

	test bx, bx
	jnz pp14
	test cx, cx
	jnz pp14		; back for more -->

@@:
%if _PM
	call resetmode
%endif
	call silence_dump

	retn

%if ($ - pp1 - 1) < 128
pp14: equ pp1
%else
pp14:	jmp pp1
%endif

pp15:
	jmp unexpectedinterrupt	; print message about unexpected interrupt
				; and quit


%if _SYMBOLIC
		; INP:	bx:(e)si -> where to place breakpoint by default
		;	byte [pp_instruction] = E8h if near immediate call,
		;	 bx:(d)word [pp_operand] -> rel16/rel32
		;	word [reg_cs]:(d)word [reg_eip] = next CS:(E)IP
		; OUT:	NC if no symhint detected or only skip symhints,
		;	 bx:(e)si -> where to place breakpoint
		;		(will be modified if skip symhints occurred)
		;	CY if trace symhints detected
pp3_check_symhints:
	nearcall zz_detect_xms		; re-detect XMS if used after run

		; Check for ..@symhint_trace|skip_caller_* hint at called address.
pp3_check_trace_caller_or_skip_caller:
	xor ax, ax
	lframe
	lenter
	lvar word,	segment
	 push bx
	lvar dword,	offset
%ifn _PM
	 push ax
%else
_no386	 push ax
%endif
	_386_PM_o32
	 push si
	lvar word,	skip
	 push ax

	mov al, byte [pp_instruction]
	cmp al, 0E8h
	jne .notcallrel16
	_386_PM_o32
	mov si, word [pp_operand]

_386_PM	movzx eax, ax
	call pp16
	xchg al, ah
	call pp16
	xchg al, ah
_386_PM	jmp @F
	test dh, 2
	jnz .notcallrel16
@@:
_386_PM	test dh, 2
_386_PM	jz @F

_386_PM	rol eax, 16
_386_PM	call pp16
_386_PM	xchg al, ah
_386_PM	call pp16
_386_PM	xchg al, ah
_386_PM	rol eax, 16
@@:
	_386_PM_o32
	add si, ax
	_386_PM_o32
	mov dx, si
	call getlinear_d_b
	jc @F
	mov cx, dx
	mov bx, ax
	nearcall binsearchmain		; search for matching symbol
	jcxz @F
.loop_symbol:
	 push bx			; main index
	 push ax			; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es

	test byte [es:di + smFlags + 1], smfSymHint >> 8
	jz .next_symbol

	nearcall zz_copy_strings_to_str_buffer

	push cx
	push ss
	pop es
	mov di, str_buffer + msg.symhint_size
	mov si, msg.trace_caller
	mov cx, msg.trace_caller_size
	push di
	repe cmpsb
	pop di
	pop cx
	je pp3_trace

	push cx
	mov si, msg.skip_caller
	mov cx, msg.skip_caller_size
	repe cmpsb
	pop cx
	jne .next_symbol

	call pp3_handle_skip_di

.next_symbol:
	inc bx
	loop .loop_symbol

@@:
.notcallrel16:


		; Check for ..@symhint_trace|skip_here_* hint at cs:eip address.
pp3_check_trace_here_or_skip_here:
	mov bx, word [reg_cs]
	_386_PM_o32
	mov dx, word [reg_eip]
	call getlinear_d_b
	jc @F
	mov cx, dx
	mov bx, ax
	nearcall binsearchmain		; search for matching symbol
	jcxz @F
.loop_symbol:
	 push bx			; main index
	 push ax			; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es

	test byte [es:di + smFlags + 1], smfSymHint >> 8
	jz .next_symbol

	nearcall zz_copy_strings_to_str_buffer

	push cx
	push ss
	pop es
	mov di, str_buffer + msg.symhint_size
	mov si, msg.trace_here
	mov cx, msg.trace_here_size
	push di
	repe cmpsb
	pop di
	pop cx
	je pp3_trace

	push cx
	mov si, msg.skip_here
	mov cx, msg.skip_here_size
	repe cmpsb
	pop cx
	jne .next_symbol

	call pp3_handle_skip_di

.next_symbol:
	inc bx
	loop .loop_symbol

@@:
	db __TEST_IMM8		; skip stc, NC
pp3_trace:
	stc

	push ss
	pop es
	push ss
	pop ds

	mov word [stack_low_address], str_buffer

	pop dx			; ?skip
	_386_PM_o32
	pop si			; ?offset
%ifn _PM
	pop ax			; ?offset high word
%else
_no386	pop ax			; ?offset high word (if it was not pop esi)
%endif
	pop bx			; ?segment
				; restore bx:(e)si if proceeding, else discard
	lleave code
	lahf
_386_PM	movzx edx, dx
	_386_PM_o32
	add si, dx
	sahf
	retn


		; INP:	ds:di -> symbol with ASCIZ keyword or expression
		;	word [bp + ?segment] = segment/selector of breakpoint
		;	(d)word [bp + ?offset] = offset of breakpoint
		; OUT:	word [bp + ?skip] = how far to skip
		; CHG:	esi, dx, ax
		; STT:	es = ds = ss
pp3_handle_skip_di:
	push bx
	push cx

	mov si, di
@@:
	lodsb
	cmp al, 0
	je .expr_have
	cmp al, '_'
	jne @B
	mov byte [si - 1], 0
.expr_have:
	mov si, di
	mov dx, msg.asciz
	call isstring?
	jne .expr_num
	xor cx, cx
	mov bx, word [bp + ?segment]
	_386_PM_o32
	mov si, word [bp + ?offset]
.expr_asciz_loop:
	call pp16
	test al, al
	loopnz .expr_asciz_loop
	jcxz .error
	neg cx
	mov dx, cx
	jmp .expr_got

.error:
	mov si, line_in + 2
	jmp error

.expr_num:
	lodsb
	call getword
.expr_got:
	mov word [bp + ?skip], dx

	pop cx
	pop bx
	retn

	lleave ctx


		; INP:	bx = SYMMAIN index
		; CHG:	es, di, si, dx, ax
		; STT:	ss = ds
		;
		; Note:	This invalidates the access slice.
zz_copy_strings_to_str_buffer: section_of_function
	mov word [stack_low_address], stack

	push cx

	 push bx			; main index
	 push ax			; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es
	push ds
		; Here we depend on str_buffer being 512-bytes long,
		;  to allow storing both smName1 and smName2 contents
		;  one after another in str_buffer.
	nearcall getstring1
	 push ss
	 pop es
	mov si, dx
	mov di, str_buffer
	rep movsb
	mov si, di
	pop ds
	 push bx			; main index
	 push ax			; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es
	push ds
	nearcall getstring2
	 push ss
	 pop es
	mov di, si
	mov si, dx
	rep movsb
	mov al, 0
	stosb
	pop ds

	pop cx
	retn
%endif


terminate_silent_dump.if_nonnull:
	testopt [internalflags], tt_silent_mode
	jz terminate_silent_dump.ret
	push es
	push di
	push ax
	mov es, word [auxbuff_segorsel]
	mov di, word [auxbuff_behind_last_silent]
	cmp di, word [auxbuff_behind_while_condition]
	je terminate_silent_dump.done
	dec di
	mov al, 0
	scasb
	je terminate_silent_dump.done
	jmp @F

terminate_silent_dump:
	testopt [internalflags], tt_silent_mode
	jz .ret
.is_silent:
	push es
	push di
	push ax
	mov es, word [auxbuff_segorsel]
	mov di, word [auxbuff_behind_last_silent]
	mov al, 0
@@:
	stosb
	inc word [auxbuff_behind_last_silent]
				; -> point past the NUL
.done:
	pop ax
	pop di
	pop es
.ret:
	retn


dumpregs_extended_silent:
%if _PM
	call resetmode
%endif
	call silence_init
		; Call dumpregs (trimputs, puts, putsline, disasm) with
		;  "silence" flag (writes to auxbuff instead of terminal).
	call dumpregs_extended

		; Terminate the last dump's output with a NUL byte.
	call terminate_silent_dump
	call silence_exit

handle_serial_flags_ctrl_c:
	push ds
	push ax
	 push ss
	 pop ds
	testopt [options3], opt3_check_ctrlc_0bh
	jz @FF
		; The following DOS call originated in sleepcmd.
		;  However, it is useful for all callers of this
		;  function so put it here.
	call InDos
	jnz @F
	mov ah, 0Bh
	doscall			; allow to break with Ctrl-C
@@:
@@:
	testopt [serial_flags], sf_ctrl_c
	jnz handle_ctrl_c
	testopt [options], enable_serial
	jnz .ret
	call InDOS_or_BIOS_IO
	jz .ret
	testopt [options3], opt3_check_ctrlc_keyb
	jz .ret

.check_rombios_buffer:
	push bx
	push dx
	mov ax, 40h		; dual mode segment/selector
	 push ax
	mov ax, word [io_end_buffer]
	mov dx, word [io_start_buffer]
	 pop ds
	test ax, ax
	jnz @F
	mov ax, word [82h]	; end of circular keypress buffer
@@:
	test dx, dx
	jnz @F
	mov dx, word [80h]	; start of circular buffer
@@:
	mov bx, ax
	sub bx, dx		; cmp end, start
	jbe .ret_dx_bx		; invalid -->
	test bl, 1		; even amount of bytes ?
	jnz .ret_dx_bx		; invalid -->
	mov bx, word [1Ah]	; current head of circular buffer
.loop:
	cmp bx, word [1Ch]	; equal to current tail ?
	je .ret_dx_bx		; yes, all entries checked -->
	cmp byte [bx], 3	; is it Ctrl-C ?
	je handle_ctrl_c	; yes, handle -->
	inc bx
	inc bx			; -> next entry
	cmp bx, ax		; at end of buffer ?
	jb .loop		; no, loop -->
	ja .ret_dx_bx		; invalid -->
	mov bx, dx		; reset to start of buffer
	jmp .loop		; then loop -->

.ret_dx_bx:
	pop dx
	pop bx

.ret:
	pop ax
	pop ds
	retn


handle_ctrl_c:
	 push ss
	 pop ds
	 push ss
	 pop es
	clropt [serial_flags], sf_ctrl_c | sf_double_ctrl_c
%if _PM
	call resetmode
%endif
	call terminate_silent_dump.if_nonnull
	call silence_dump
	mov dx, msg.ctrlc
	call putsz

	setopt [internalflags3], dif3_input_terminal_override
				; make sure we drain terminal input
@@:
	call getc_if_any	; drain the buffer
	jnz @B			; if any was available -->

	clropt [internalflags3], dif3_input_terminal_override
	jmp cmd2_reset_re


		; INP:	bp [tpg_proceed_bp],
		;	 linear address and point content to write
		;	opt [internalflags2] & dif2_tpg_proceed_bp_set
		;	 set if a breakpoint was written
		; OUT:	CY if error,
		;	 bp [tpg_proceed_bp] = has point content to restore
		;	 ah = reason =	0 = no error (never),
		;			1 = couldn't write,
		;			2 = unreachable,
		;			3 = overwritten, al = new byte value
		;	NC if no error,
		;	 ah = 0
		;	 opt [internalflags2] & dif2_tpg_proceed_bp_set
		;	  cleared
		; CHG:	di, (e)dx, ax, bx
		; STT:	might switch modes due to getsegmented call
proceed_writepoint_restore:
	mov ah, 0
	testopt [internalflags2], dif2_tpg_proceed_bp_set
	jz proceed_wp.retn	; (NC)
	mov di, 1
	jmp proceed_wp

		; INP:	bp [tpg_proceed_bp],
		;	 linear address and point content to write (0CCh)
		; OUT:	CY if error,
		;	 bp [tpg_proceed_bp] = has point content 0CCh
		;	 ah = reason =	0 = no error (never),
		;			1 = couldn't write,
		;			2 = unreachable,
		;			3 = overwritten (never)
		;	NC if no error (either flag not set or point restored),
		;	 ah = 0
		;	 opt [internalflags2] & dif2_tpg_proceed_bp_set
		;	  set
		; CHG:	di, (e)dx, ax, bx
		; STT:	might switch modes due to getsegmented call
proceed_writepoint:
%if _DELAY_BEFORE_BP
	call delay_before_bp
%endif
	xor di, di
proceed_wp:
	lframe near
	lenter
	xor di, 1
	lvar	word, is_write
	 push di

	mov ax, word [tpg_proceed_bp]
%if _PM
	mov dx, word [tpg_proceed_bp + 2]
%else
	xor dx, dx
	mov dl, byte [tpg_proceed_bp + 2]
%endif
	call getsegmented
	mov al, byte [tpg_proceed_bp + BPSIZE - 1]
				; al = byte to restore
	mov ah, 2		; error reason: unreachable
	jc .return

	test byte [bp + ?is_write], 1
				; (NC) is it writing ?
	jnz .write
.restore:
	push ax
	call readmem		; read current byte
	cmp al, 0CCh		; is this still what we wrote?
	mov ah, 3		; error reason: overwritten, al = new value
	stc
	jne .return_discard	; nope --> (CY)
	pop ax
	call writemem		; return the byte to its original value
	jc .next		; failed --> (CY, handled there)
	mov byte [tpg_proceed_bp + BPSIZE - 1], 0CCh
				; reset stored point
	jmp short .next

.write:
	call writemem
	mov byte [tpg_proceed_bp + BPSIZE - 1], al
				; save the previous byte there
.next:
	mov ah, 1		; (in case of error) error reason: cannot write
	jc .return		; failed to write --> (CY)
	mov ah, 0		; (no error)

        setopt [internalflags2], dif2_tpg_proceed_bp_set
				; set flag in case of successful writing
	test byte [bp + ?is_write], 1
				; (NC) is it writing ?
	jnz .return		; yes, leave flag set -->
        clropt [internalflags2], dif2_tpg_proceed_bp_set
				; (NC) clear flag in case of successful restoring
.return:
.return_discard:
	lleave
.retn:
	retn


		; INP:	ah & 7Fh = status =	0 = no error,
		;				1 = couldn't write,
		;				2 = unreachable,
		;				3 = overwritten, al = new value
		;	ah & 80h = set if error restoring point,
		;			else error writing point
		;	bh & 80h = set if gg breakpoint,
		;	 cx = index (0-based)
		;	bh & 40h = set if bb breakpoint,
		;	 cx = index (0-based)
		;	bh & C0h = clear if proceed breakpoint
		;	bl = what we tried to restore, only set if ah & 80h set
		;	dword [ss:sp] = linear address (24 bit if non-_PM)
		; CHG:	ax, bx, cx, dx, di
		; STT:	ds = es = ss = debugger data selector
display_breakpoint_failure:
%if _PM
	call resetmode
%endif

	push ax
	push bx
	push cx
	push si
	call silence_dump	; do away with silent mode
	pop si
	pop cx
	pop bx
	pop ax

	setopt [internalflags2], dif2_bp_failure

	lframe near
	lpar dword, linear
	lenter
	lvar word, input_ax
	 push ax

	mov dx, msg.cant_bp_the
	call putsz
	test bh, 80h
	jz @F

	mov ax, cx
	inc ax			; make it 1-based
	call putordinalbyte

	mov dx, msg.cant_bp_type_gg
	jmp .got_type

@@:
	test bh, 40h
	jz @F

	mov ax, cx
	mov di, msg.cant_bp_type_permanent.index
	call hexbyte		; (0-based index)

	mov dx, msg.cant_bp_type_permanent
%if _SYMBOLIC
	sub ax, _NUM_B_BP
	jb .got_type

	mov di, msg.cant_bp_type_symbol.index
	call hexbyte		; (0-based index)

	mov dx, msg.cant_bp_type_symbol
%endif
	jmp .got_type

@@:
	mov dx, msg.cant_bp_type_proceed

.got_type:
	call putsz

	mov di, msg.cant_bp_linear.address1
	mov ax, word [bp + ?linear + 2]
%ifn _PM
	mov ah, 0
%endif
	call hexword
	inc di
	; mov di, msg.cant_bp_linear.address2
	mov ax, word [bp + ?linear]
	call hexword

	mov dx, msg.cant_bp_linear
	call putsz

	mov di, msg.cant_bp_restore.value
	mov ax, bx
	call hexbyte

	mov dx, msg.cant_bp_write
	mov ax, word [bp + ?input_ax]
	test ah, 80h
	jz @F
	and ah, ~80h
	mov dx, msg.cant_bp_restore
@@:
	call putsz

	mov di, msg.cant_bp_reason3.value
	call hexbyte

	mov dx, msg.cant_bp_reason
	call putsz

	mov dx, msg.cant_bp_reason0
	cmp ah, 1
	jb @F
	mov dx, msg.cant_bp_reason1
	je @F
	mov dx, msg.cant_bp_reason2
	cmp ah, 3
	jb @F
	mov dx, msg.cant_bp_reason3
	je @F
	mov dx, msg.cant_bp_reasonu
@@:
	call putsz

	lleave
	lret


%if _PM
isdpmientry:
	testopt [internalflags4], dif4_int_2F_hooked
	jz @F
	testopt [internalflags], nohook2F
	jz .stdhook
@@:
	mov word [reg_eip], mydpmientry
	mov word [reg_cs], ds	; if Int2F not hooked, point to the hook here
				; ds => lDEBUG_DATA_ENTRY
.stdhook:
		; Run code until it returned far.
	mov bx, word [reg_esp]
	push ds
	mov ds, word [reg_ss]	; ds:bx-> (16-bit) stack
	mov si, word [bx+0]
	mov bx, word [bx+2]	; get (16-bit) far return address
	pop ds
%endif
		; Proceed over an instruction
		; INP:	bx:(e)si-> where to write the breakpoint
		; OUT:	NC if the breakpoint was hit,
		;	 ah = 0
		;	 ch = 0
		;	 d[reg_eip] adjusted
		;	 cx & 8 set if proceed point hit
		;	 (ZF only set if _BREAKPOINTS)
		;	 ZR if breakpoint after instruction was hit,
		;	  cx & 7 = 0
		;	 NZ if another breakpoint (bb) was hit (or both),
		;	  cx & 1 set if non-pass match (actual hit),
		;	   else cx & 2 set if pass match (consider as hit first,
		;		but dump registers next (not to silent buffer)
		;		and then continue execution)
		;	   else cx & 4 always set, indicates any match
		;		(including matches that should merely continue)
		;	  all pass points' counters stepped
		;	CY if the breakpoint was not hit,
		;	 cx = 0
		;	If [internalflags2] & dif2_gg_is_gg is set:
		;	 ah & 7Fh = status =	0 = no error,
		;				1 = couldn't write,
		;				2 = unreachable,
		;				3 = overwritten, al = new value
		;	 ah & 80h = set if error restoring point,
		;			else error writing point to begin with
		;	If that flag is clear:
		;	Does not return if a breakpoint cannot be written
		;	 or cannot be restored, jumps to cmd3 instead.
		; CHG:	all
		; STT:	ds = es = ss
		;	might return modeswitched (if dif2_gg_is_gg)
		;	might be called while modeswitched
proceedbreakpoint:
	testopt [internalflags2], dif2_tpg_keep_proceed_bp
	jnz @F

	_386_PM_o32		; mov edx, esi
	mov dx, si		; bx:(e)dx = segmented
	call getlinear_d_b	; dx:ax = linear

	mov word [tpg_proceed_bp], ax
		; The following two instructions must be in this order.
		;  For the non-_PM build, writing to the second word
		;  of the breakpoint also writes to the content byte.
	mov word [tpg_proceed_bp + 2], dx
				; store in point
@@:
	mov byte [tpg_proceed_bp + BPSIZE - 1], 0CCh
				; initialise content
	call proceed_writepoint
		; This call might return modeswitched.
	jnc @F

	and ah, ~80h		; mark error during writing

	xor cx, cx		; cx = 0 in case of branching
	push cx			; put the zero on the stack

	jmp .failure

@@:
		; The run functions call resetmode.
%if _BREAKPOINTS
	call run_with_bb
	mov ax, cx
%else
	call run
	xor ax, ax
%endif
	push ax

		; It's important to keep the linear address saved inbetween,
		; even though we save by value (as opposed to DEBUG/X G's saving
		; of the reference) because the selector that we used for the
		; access might now be invalid or (worse) point elsewhere, or
		; a mode change might have occured. (The latter is sometimes
		; handled by a specific kludge in DEBUG/X, but not always.)

	call proceed_writepoint_restore
		; This call might return modeswitched.
	jnc @F

	or ah, 80h		; mark error during restoration

.failure:
		; Here we may be modeswitched.
	testopt [internalflags2], dif2_gg_is_gg
				; is it from gg ?
	jnz .return_CY_pop_cx	; (CY) yes, return to gg
		; This branch may be taken while modeswitched.

%if _PM
	call resetmode
%endif

	call put_deferred_message_silent

	push ax
	call silence_dump
	pop ax

	 push word [tpg_proceed_bp + 2]
	 push word [tpg_proceed_bp]
	mov bl, [tpg_proceed_bp + BPSIZE - 1]
	mov bh, 0
	call display_breakpoint_failure
		; This function calls resetmode.
	jmp cmd3

@@:
	call get_cseip_of_possible_breakpoint
				; dx:ax = linear address 1 before cs:(e)ip
	jc .return_CY_pop_cx_ax_zero

	pop cx
%if _PM
	cmp word [tpg_proceed_bp + 2], dx
%else
	test dh, dh		; (bits 24 to 31 set. shouldn't happen)
	jnz .not_pp
	cmp byte [tpg_proceed_bp + 2], dl
%endif
	jne .not_pp
	cmp word [tpg_proceed_bp], ax
	jne .not_pp		; is unexpected (not behind the breakpoint) -->

		; Need to check this here, because we have to
		; decrement (e)ip if the breakpoint was hit.
	cmp word [run_int], int3msg
	jne .not_pp		; is unexpected (not returned by interrupt 03h) -->
	or cl, 8		; set flag: pp hit

.not_pp:
	mov ah, 0		; set error to "no error"

	jcxz .return_CY		; bb hit/pass/nonpass or pp hit ?  if no -->

	call adjust_cseip_after_breakpoint
				; decrement (e)ip to point at the instruction

	test cl, 1		; bb hit ?
	jnz .return		; yes, return (NC, NZ) -->

	test cl, 8		; proceed point matched ?
	jnz @F			; yes -->

	setopt [internalflags2], dif2_tpg_keep_proceed_bp
				; flag that we should keep this proceed point
				; (NC, NZ)
.return:
	retn

@@:
		; return with ax = 0, NC, ZR
		;
		; (hit proceed point, no hit bb (possibly pass/non-pass bb)
	xor ax, ax		; ah = 0 (NC, ZR)
	retn

		; set ax = 0, pop cx, CY
.return_CY_pop_cx_ax_zero:
	xor ax, ax

		; pop cx, CY (preserve ax)
.return_CY_pop_cx:
	pop cx
.return_CY:
	stc
	retn


		; PPX - Get next byte in instruction stream.
		; INP:	bx:(e)si-> next byte
		; OUT:	al = next byte
		;	(e)si incremented
pp16:
%if _PM
	call resetmode_and_test_d_b_bit
%endif
	push ds
	mov ds, bx
%if _PM
	jz .16
	a32			; use esi for lodsb
.16:
%endif
	lodsb
	pop ds
	retn
		; begin loop over instruction bytes.


 %if _BREAKPOINTS
		; Run with bb breakpoints
		;
		; OUT:	CY if another breakpoint (not a bb one) or trace hit,
		;	 cx = 0
		;	NC if a bb breakpoint hit,
		;	 cx & 1 set if it is an actual hit
		;	 else cx & 2 set if it is a pass match,
		;	 else it is a non-pass non-match
		;	 (cx & 4 always set)
		; STT:	might return modeswitched
run_with_bb:
	testopt [internalflags2], dif2_gg_is_gg
	jnz .no_bb

	testopt [internalflags], tt_no_bb | tt_no_bb_first
	jz @F

	clropt [internalflags], tt_no_bb_first

.no_bb:
	call run
	xor cx, cx
	stc
	retn

@@:
.gg5:
%if _BREAKPOINTS
	call bb_writepoints_init_reset
%endif

	testopt [internalflags2], dif2_gg_first_detected
	jz .only_run		; easy case, no cseip point detected -->


		; Enter special mode: Restore cseip breakpoint content.
	setopt [internalflags2], dif2_gg_skip_non_cseip

	sub sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
	mov bp, sp		; -> error info

	mov cx, _NUM_B_BP + _NUM_SYM_BP
				; = index above last one to restore
	call bb_restorepoints_and_init_error_info
	jnc @F			; no error ? -->

		; Error in bb_restorepoints. Try to restore other bb.

		; Exit special mode: Handle non-cseip breakpoints again.
	clropt [internalflags2], dif2_gg_skip_non_cseip

		; Enter special mode: Skip cseip breakpoints.
	setopt [internalflags2], dif2_gg_skip_cseip

		; As we already tried to restore all cseip bb points,
		;  here we skip these in the bb_restorepoints call.
	mov cx, _NUM_B_BP + _NUM_SYM_BP
	call bb_restorepoints

		; Exit special mode: No longer skip cseip breakpoints.
	clropt [internalflags2], dif2_gg_skip_cseip

		; The failure that led us here is already noted in the info.
	jmp .bb_exit


@@:
		; Success! Now discard the reserved error info.
	add sp, (_NUM_B_BP + _NUM_SYM_BP) * 2

		; Exit special mode, do not skip non-cseip breakpoints anymore.
	clropt [internalflags2], dif2_gg_skip_non_cseip

		; Enter special mode: Skip matching/restoring cseip breakpoint.
	setopt [internalflags2], dif2_gg_skip_cseip

.only_run:
	call run
.after_run:

%if _BREAKPOINTS
	sub sp, (_NUM_B_BP + _NUM_SYM_BP) * 2
	mov bp, sp

	mov cx, _NUM_B_BP + _NUM_SYM_BP
				; = index above last one to restore
	call bb_restorepoints_and_init_error_info
	jnc @F

.bb_exit:
	mov ax, -1
	push ax			; (unused: ax error info)
	push ax			; cx error point index, -1 = invalid

	jmp bb_restorepoints_exit


@@:
	add sp, (_NUM_B_BP + _NUM_SYM_BP) * 2

	call bb_check_hit
	mov cx, ax
	pushf
		; Clear all special modes. Stop specialcasing cseip breakpoint.
		;
		; This resets all the special flags for subsequent calls.
	clropt [internalflags2], \
		dif2_gg_is_first | dif2_gg_first_detected \
		| dif2_gg_skip_cseip | dif2_gg_skip_non_cseip
	popf			; CF
	jnc @F

	stc
				; cx = flags as returned by bb_check_hit
	retn

@@:
	call adjust_cseip_after_breakpoint
				; re-execute (restored) opcode one byte in front of this
	clc			; (NC)
				; cx = flags as returned by bb_check_hit
	retn
%endif
 %endif


		; Run - Start up the running program.
		;
		; INP:	b[eqflag], a[eqladdr] = address given behind '=' for command
		;	w[pspdbe] = process of debuggee
		;	[run2324] = interrupt handlers 23h and 24h to set
		;	values for registers in d[reg_eax] etc
		; OUT:	(_DEBUG && _DEBUGUPDATESAVE)
		;	 interrupt handlers' ieNext fields updated
		;	d[reg_eax] etc updated
		;	w[run_int]-> message of how execution returned
		;	UP, EI, high word efl = 0, es = ds = ss
		; CHG:	all
		; STT:	ds = ss
		;	UP
		;	(INP:es ignored)
run:
	 push ss
	 pop es
%if _PM
	call resetmode
	call remember_mode
%endif
%if _DELAY_BEFORE_BP
	clropt [internalflags3], dif3_delayed
%endif
	clropt [internalflags2], \
		dif2_tpg_have_bp | dif2_tpg_adjusted_cseip \
		| dif2_tpg_do_not_adjust | dif2_bp_failure \
		| dif2_tpg_keep_proceed_bp, 1
	call seteq			; set CS:IP to '=' address

%if _VXCHG
	mov al, 0			; restore debuggee screen
	call swapscreen
%endif

%if _ALTVID
	call setscreen
%endif

		; For DDebugX: Do this before we install our
		;  exception handlers. So if an exception
		;  is raised then it is handled by the outer
		;  instance instead of our handler.
		; (The actual problem may be that we don't
		;  restore the handlers in the entrypoint
		;  that leads to debuggerexception.)
		; Also, for non-_DEBUG DebugX too, check the
		;  validity before setting debuggee PSP and
		;  int 23h, 24h so they needn't be reset.
%if _PM
		; Load segreg values into es to make sure
		;  they're valid. (Previously done with
		;  the stack pointing into the reg array.
		;  Better to do it now with a valid stack.)
		; Only done if we may be in Protected Mode.
		;  86 Mode allows any value to be loaded.
	mov es, word [reg_ds]
	mov es, word [reg_ss]
_386	mov es, word [reg_fs]
_386	mov es, word [reg_gs]
%endif
	mov es, word [reg_es]		; last one: actual es value

	mov bx, word [pspdbe]
	call setpsp			; set debuggee's PSP
	call setint2324			; set debuggee's int 23/24

%if _DEBUG				; set this copy's divide/trace/breakpoint ints
 %if _TSR || _BOOTLDR || _DEVICE
..@patch_tsr_quit_run:
	db __TEST_IMM16
	dw __REL16__(.skipints)
 %endif
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jz .skipints
	call set_interrupts
	jmp .skipints

set_interrupts: equ $
 %endif
	push cx
	push si
	push ax
	push dx
	push es
	push bx
	push bp
	push di

%if _PM
	call ispm
	jz @F			; in PM -->
	testopt [internalflags], canswitchmode
	jz @FF			; in 86 Mode and cannot switch to PM -->

d4	call d4message
d4	asciz "In run (switch mode before calling pm_set_handlers)",13,10
	setopt [internalflags], modeswitched	; set flag for resetmode
	mov al, 0
	call sr_state		; save state
	call switchmode 	; switch to PM
		; ! handle_mode_changed not called here !
		; do not call InDos or other functions using seg/sels
@@:
	call pm_set_handlers
		; ! this calls resetmode
@@:
%endif

%if _CATCHINT06 && _DETECT95LX
	mov cx, word [inttab_number_variable]
%else
	mov cx, inttab_number
%endif
	mov si, inttab
.intloop:
	lodsb
	xchg ax, dx
	lodsw				; get address
	xchg ax, dx
%if _DEBUGUPDATESAVE
	mov di, dx			; -> IISP header
%endif

	call InDos
	jz .int21_25

	xor bx, bx
%if _PM
	call ispm
	jnz @F
	push dx
	mov dx, bx			; set segment to access (= 0)
	call setrmsegm			; get bx = selector configured to this
	pop dx
@@:
%endif
	mov es, bx			; => 86 Mode IVT (segment or selector)
%if _PM
	xor bx, bx			; bh = 0
%endif
	mov bl, al
	add bx, bx
	add bx, bx

%if _DEBUGUPDATESAVE
	push word [ es:bx + 2 ]
	push word [ es:bx ]		; get vector
	pop word [ di + ieNext ]
	pop word [ di + ieNext + 2]
%endif

	mov word [ es:bx ], dx
%if _PM
	push word [ pspdbg ]		; => lDEBUG_DATA_ENTRY (86 Mode seg)
	pop word [ es:bx + 2 ]
%else
	mov word [ es:bx + 2 ], ds	; => lDEBUG_DATA_ENTRY
%endif
	jmp short .intset

.int21_25:

%if _PM
 %if _DEBUGUPDATESAVE
	mov ah, 35h			; get interrupt vector
	 push word [pspdbg]		; => lDEBUG_DATA_ENTRY
	dual2call _doscall_return_es_parameter_es_ds
	mov word [ di + ieNext ], bx
	 pop word [ di + ieNext + 2 ]
 %endif
	mov ah, 25h			; set interrupt vector
	push word [pspdbg]		; => lDEBUG_DATA_ENTRY
	dual2call _doscall_return_es_parameter_es_ds
	pop ax				; (discard returned parameter)
%else
 %if _DEBUGUPDATESAVE
	mov ah, 35h
	int 21h				; get vector
	mov word [ di + ieNext ], bx
	mov word [ di + ieNext + 2 ], es
 %endif
	mov ah, 25h			; set interrupt vector
	int 21h				; ds => lDEBUG_DATA_ENTRY
%endif
.intset:
	loop .intloop

	pop di
	pop bp
	pop bx
	pop es
	pop dx
	pop ax
	pop si
	pop cx
 %if _DEBUG_COND
	retn
 %endif
.skipints:
%endif

.2:
	sub sp, word [run_sp_reserve]
	mov word [run_sp], sp		; save stack position

	; Disable this for now. The debugger uses its ss
	;  for 86 Mode and Protected Mode at the same area
	;  so it should always be valid to adjust SPSAV with
	;  the current run_sp, regardless of current mode.
	; Update: SPSAV should always hold a 86 Mode address.
	;  So check for our segment, not the current ss. (But
	;  for _PM=0 ss is always equal to word [pspdbg].)
	;  I assume that the original fix was intended for cases
	;  where the segment doesn't match our 86 Mode ss, that
	;  is the word [pspdbg].
%if 1
	; 16.2.2021: check if saved SS is debugger's SS. If no, don't adjust saved SP.
	; SS may be != saved SS if debugger is stopped in protected-mode - then the
	; current DPMI real-mode stack may be stored in SPSAV.
 %if _PM
	mov ax, word [pspdbg]
 %else
	mov ax, ss
 %endif
	cmp ax, word [SPSAV + 2]
	jne @F
%endif
	sub sp, word [spadjust]
	mov word [SPSAV], sp
@@:
	cli

	mov sp, regs
%ifn _ONLY386
	_386_jmps .386			; -->
	pop ax
	pop ax				; discard all high words
	pop bx
	pop ax
	pop cx
	pop ax
	pop dx
	pop ax
	pop ax				; we'll get esp later
	pop ax
	pop bp
	pop ax
	pop si
	pop ax
	pop di
		; ds, ss, cs loaded later
		; es already loaded
%endif
%ifn _ONLYNON386
 %ifn _ONLY386
	jmp short .common
 %endif

.386:
[cpu 386]
	pop eax
	pop ebx
	pop ecx
	pop edx
	pop ax
	pop ax				; we'll get esp later
	pop ebp
	pop esi
	pop edi
	; pop ax			; get ds later
	; pop ax			; discard high words of segment registers
	; pop ax			; es already loaded
	; pop ax
	; pop ax			; get ss later
	; pop ax
	; pop ax			; get cs later
	; pop ax
	add sp, byte 8 * 2
	pop fs
	pop ax
	pop gs
__CPU__
.common:
%endif
	mov ax, word [reg_eax]		; restore ax (used to discard words)
	mov ss, word [reg_ss]
%if _ONLYNON386
	mov sp, word [reg_esp]
%else
..@patch_no386_ds:
	o32			; mov esp, dword [reg_esp]
	mov sp, word [reg_esp]		; restore program stack
%endif
	mov byte [bInDbg], 0
	_386_o32		; push dword [reg_efl]
	push word [reg_efl]
	_386_o32		; push dword [reg_cs]	; high word is zero
	push word [reg_cs]
	_386_o32		; push dword [reg_eip]
	push word [reg_eip]
	test byte [reg_efl+1], 2	; IF set?
	mov ds, word [reg_ds]		; restore ds
	jz .di
	sti				; required for ring3 protected mode if IOPL==0
.di:
%if _ONLYNON386
	iret
%else
..@patch_no386_iret:
	o32			; iretd
	iret				; jump to program
%endif

; The byte at ..@patch_no386_ds will be adjusted to a ds prefix on non-386 processors.
; This does not change the following instruction's behaviour (aside from changing it
; to a 16-bit instruction as intended) and insures that sp is set in the instruction
; right behind the instruction that sets ss.

; The byte at ..@patch_no386_iret will be adjusted to an iret instruction on non-386 processors.
; This avoids the NOP that would be written there if _386_o32 was used, because the iret
; should follow right behind the sti instruction.


	usesection lDEBUG_DATA_ENTRY


%if _CATCHSYSREQ
@@:
	jmp far [cs:intr_sysreq.next]

iispentry intr_sysreq, 0
	cmp byte [cs:bInDbg], 0
	jne @B
 %if _SYSREQINT == 15h
	cmp ax, 8501h	; sysreq released?
	jne @B
 %else
	pushf
	push cs
	call @B
	push ds
	push ax
	mov ax, 40h
	mov ds, ax
	test byte [18h], 4
	pop ax
	pop ds
	jnz @F
	iret

@@:
 %endif
 %if _PM && _SYSREQ_DISABLE_INT2F_HOOK
	setopt [cs:internalflags], nohook2F	; disable hooking
 %endif
	mov word [cs:run_int], sysreqmsg	; remember interrupt type
	jmp intrtn			; jump to register saving routine
%endif


%if _CATCHINT08
iispentry intr8, 0
intr8_original: equ intr8.next
	pushf
	call far [cs:intr8_original]
	pushf
	push bx
	push ds

	 push cs
	 pop ds

	cmp byte [bInDbg], 0		; in debugger ?
	jne .reset			; yes -->

	testopt [serial_flags], sf_double_ctrl_c
	jz @F

	mov word [run_int], runint_ctrlc_msg
	jmp @FF

@@:
	mov bx, 40h
	mov ds, bx
	test byte [17h], 4		; CTRL currently pressed ?
	 push cs
	 pop ds
	jz .reset			; no -->

	mov bx, word [intr8_limit]
	test bx, bx			; enabled ?
	jz .return			; no -->
	inc word [intr8_counter]
	cmp word [intr8_counter], bx	; default is ca 5 seconds
	jb .return

	mov word [run_int], int8msg
	testopt [options], int8_disable_serial
	jz @F
	testopt [options], enable_serial
	jz @F

	clropt [options], enable_serial
	mov word [run_int], int8_kbd_msg

@@:
	pop ds
	pop bx
	popf				; (in 86 Mode)
	jmp intrtn

.reset:
	and word [intr8_counter], 0
.return:
	pop ds
	pop bx
	popf				; (in 86 Mode)
	iret
%endif


		; Interrupt 22h (program termination) handler.
int22:
	cli
.cleartraceflag:
	push cs
	pop ds
	push cs
	pop ss
	mov sp, word [run_sp]	; restore running stack
	add sp, word [run_sp_reserve]
	mov word [run_int], progtrm	; set interrupt message
	mov word [lastcmd], dmycmd	; disable T/P/G auto-repeat
%if _PM
	clropt [internalflags], protectedmode	; reset PM flag
%endif
	times 1 - (($ - $$) & 1) nop	; align in-code parameter
	call entry_to_code_seg
	dw intrtn1_code
					; jump to register saving routine


sharedentry1.hwreset:
	retf


		; Interrupt 01h (single-step interrupt) handler.
iispentry intr1, 0, sharedentry1
	lframe int
	lenter
	push ax

%ifn _PASSTF
	clropt [bp + ?frame_fl], 100h	; clear TF (always)
%endif

	mov ax, cs
	cmp word [bp + ?frame_cs], ax	; entry segment ?
	jne .if_ZR			; no --> (NZ)

		; On the NEC V20 if we trace an int3 instruction
		;  or a div instruction that faults, we will
		;  enter our Trace Interrupt entry with the
		;  interrupt stack frame pointing at the first
		;  instruction of the int 3 or int 0 handler.
		; If this happens, clear the Trace Flag and
		;  continue running our handler.
	cmp word [bp + ?frame_ip], intr0
	je .if_ZR			; that one --> (ZR)
	cmp word [bp + ?frame_ip], intr3
	je .if_ZR			; that one --> (ZR)

		; If the DOS doesn't clear the Trace Flag when
		;  it uses a PRA to return to, its iret will
		;  enable tracing and invoke our Trace Interrupt
		;  with the stack frame pointing to *the second*
		;  instruction in the PRA handler.
		; Like above, clear TF then run the PRA handler.
%if _PM
	cmp word [bp + ?frame_ip], getline_extra_int22.cleartraceflag
	je .if_ZR			; that one --> (ZR)
%endif
	cmp word [bp + ?frame_ip], int22.cleartraceflag
	je .if_ZR			; that one --> (ZR)
	cmp word [bp + ?frame_ip], debug22.cleartraceflag
.if_ZR:
	pop ax

%if _PASSTF
	jnz @F				; handle trace entry -->
	clropt [bp + ?frame_fl], 100h	; clear TF (only if we continue)
	lleave code, optimiserestoresp	; restore bp
	iret				; continue run if matched

@@:
	lleave , optimiserestoresp	; restore bp
%else
	lleave , optimiserestoresp	; restore bp (common before branch)
	jnz @F				; handle trace entry -->
	iret				; continue run if matched

@@:
%endif

	mov word [cs:run_int], int1msg	; remember interrupt type
	jmp intrtn			; jump to register saving routine


		; Interrupt 00h (divide error) handler.
iispentry intr0, 0, sharedentry1
	mov word [cs:run_int], int0msg	; remember interrupt type
	jmp intrtn			; jump to register saving routine



		; Interrupt 03h (breakpoint interrupt) handler.
iispentry intr3, 0, sharedentry1
	mov word [cs:run_int], int3msg	; remember interrupt type
%if _CATCHINT06
	jmp intrtn			; jump to register saving routine


		; Interrupt 06h (invalid opcode) handler.
iispentry intr6, 0, sharedentry2
	mov word [cs:run_int], int6msg	; remember interrupt type
%endif
	jmp intrtn			; jump to register saving routine


sharedentry2.hwreset:
	retf


%if _CATCHINT07
		; Interrupt 07h (no x87 present on 286+) handler.
iispentry intr7, 0, sharedentry2
	mov word [cs:run_int], int7msg	; remember interrupt type
	cmp byte [cs:bInDbg], 0
	jne r86m_debugger_exception
	jmp intrtn			; jump to register saving routine
%endif

%if _CATCHINT0C
		; Interrupt 0Ch (stack fault in R86M, or IRQ) handler.
iispentry intr0C, 0, sharedentry2
 %if _MCLOPT
	cmp byte [cs:master_pic_base], 20h
	jae @F
 %endif
	push ax
	mov al, 0Bh			; request In-Service Register (ISR)
	out 20h, al			; from primary PIC
	in al, 20h			; read the ISR
	test al, 1_0000b		; IRQ #4 occurred ?
	pop ax
	jnz .notours			; yes, (likely) not a stack fault -->
@@:
	mov word [cs:run_int], int0Cmsg	; remember interrupt type
	cmp byte [cs:bInDbg], 0
	jne r86m_debugger_exception
	jmp intrtn			; jump to register saving routine

.notours:
	jmp far [cs:.next]
%endif
%if _CATCHINT0D


		; Interrupt 0Dh (general protection fault in R86M, or IRQ) handler.
iispentry intr0D, 0, sharedentry2
 %if _MCLOPT
	cmp byte [cs:master_pic_base], 20h
	jae @F
 %endif
	push ax
	mov al, 0Bh			; request In-Service Register (ISR)
	out 20h, al			; from primary PIC
	in al, 20h			; read the ISR
	test al, 10_0000b		; IRQ #5 occurred ?
	pop ax
	jnz .notours			; yes, (likely) not a stack fault -->
@@:
	mov word [cs:run_int], int0Dmsg	; remember interrupt type
	cmp byte [cs:bInDbg], 0
	jne r86m_debugger_exception
	jmp intrtn			; jump to register saving routine

.notours:
	jmp far [cs:.next]
%endif
%if _CATCHINT18
		; Interrupt 18h (diskless boot hook) handler.
iispentry intr18, 0, sharedentry3
	mov word [cs:run_int], int18msg	; remember interrupt type
%endif
%if _CATCHINT19
	jmp intrtn			; jump to register saving routine


		; Interrupt 19h (boot load) handler.
iispentry intr19, 0, sharedentry3
	mov word [cs:run_int], int19msg	; remember interrupt type
	mov word [cs:lastcmd], dmycmd	; disable T/P/G auto-repeat
	clropt [cs:internalflags2], dif2_boot_loaded_kernel
%endif

		; Common interrupt routine.

		; Housekeeping.
intrtn:
	cli				; just in case
	pop word [cs:reg_eip]		; recover things from stack
	pop word [cs:reg_cs]
	pop word [cs:reg_efl]
	mov word [cs:reg_ds], ds	; ! word-aligned (AC flag)
	mov word [cs:reg_eax], ax	; ! word-aligned (AC flag)
	mov ax, cs
	mov ds, ax			; => lDEBUG_DATA_ENTRY
	times 1 - (($ - $$) & 1) nop	; align in-code parameter
	call entry_to_code_seg
	dw intrtn_code
		; To avoid delaying the code segment switch, we use the client's
		;  stack here to call (jump) via entry_to_code_seg.


sharedentry3.hwreset:
	retf


	usesection lDEBUG_CODE

	code_insure_low_byte_not_0CCh
intrtn_code:
%if _PM
	clropt [internalflags], protectedmode	; reset PM flag
.from_installdpmi:
%endif
	mov word [reg_ss], ss	; save stack position
_386	and word [reg_eip+2], byte 0	; we're from real mode
	_386_o32		; mov dword [reg_esp], esp
	mov word [reg_esp], sp
	mov ss, ax		; mov ss, cs	; (don't use the stack here)

%if _PM
	jmp @F

intrtn2_code:				; <--- entry protected mode
	mov word [ss:reg_ds], ds	; ! word-aligned (AC flag)
	mov word [ss:reg_eax], ax	; ! word-aligned (AC flag)
	mov ax, ss
	mov ds, ax		; mov ds, ss
@@:
%endif
	mov ax, 2
%ifn _ONLY386
	_386_jmps .386			; -->
	mov sp, reg_es+2
	push es
	; sub sp, ax
	; sub sp, ax			; ds already saved
	; sub sp, ax			; don't overwrite high word of di
	sub sp, byte 3*2
	push di
	sub sp, ax
	push si
	sub sp, ax
	push bp
	; sub sp, ax
	; sub sp, ax			; sp already saved
	; sub sp, ax
	sub sp, byte 3*2
	push dx
	sub sp, ax
	push cx
	sub sp, ax
	push bx
%endif
%ifn _ONLYNON386
 %ifn _ONLY386
	jmp short .common
 %endif

.386:
[cpu 386]
	mov esp, reg_gs+2
	push gs
	sub sp, ax			; don't overwrite high words of segments
	push fs
	; sub sp, ax
	; sub sp, ax			; cs already saved
	; sub sp, ax
	; sub sp, ax			; ss already saved
	; sub sp, ax
	sub sp, byte 5*2
	push es
	; sub sp, ax
	; sub sp, ax			; ds already saved
	sub sp, byte 2*2
	push edi
	push esi
	push ebp
	; sub sp, ax
	; sub sp, ax			; sp already saved
	sub sp, byte 2*2
	push edx
	push ecx
	 pushfd				; (this overwrites reg_ebx)
	 add sp, ax			; discard low word of efl
	 pop word [reg_efl+2]
	 push 0
	 pushf				; (this also overwrites reg_ebx)
	 popfd				; clear high word of efl inside debugger (resets AC flag)
	push ebx
	mov ax, word [reg_eax]		; restore ax
	push eax			; so we don't overwrite it with 2 here
__CPU__
.common:
%endif

	code_insure_low_byte_not_0CCh
		; Clean up.
intrtn1_code:
	_386_o32		; mov esp, dword [run_sp]
	mov sp, word [run_sp]		; restore running stack
	add sp, word [run_sp_reserve]
	cld				; clear direction flag
	sti				; interrupts back on

%if _SYMBOLIC
	clropt [internalflags2], dif2_xms_detection_done
		; Re-detect XMS after running.
		;
		; The actual detection is done in symbol access functions,
		;  which are only called once all breakpoints are restored.
%endif


%if _PM
	call handle_mode_changed
%endif

	call get_cseip_of_possible_breakpoint
		; Initialise this here. This means we do not need to call
		;  resetmode between proceed_wp and bb_wp and gg_wp. If
		;  more than one point needs to switch modes this avoids
		;  repeated modeswitching back and forth.

%if _DEBUG				; reset to next copy's divide/trace/breakpoint ints
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jz .skipints
	call reset_interrupts
	jmp .skipints

reset_interrupts: equ $
 %endif
	push cx
	push si
	push di
	push ax
	push bx
	push dx
	push bp

%if _PM
	call ispm
	jz @F			; in PM -->
	testopt [internalflags], canswitchmode
	jz @FF			; in 86 Mode and cannot switch to PM -->

d4	call d4message
d4	asciz "In intrtn1_code (switch mode before calling pm_reset_handlers)",13,10
	setopt [internalflags], modeswitched	; set flag for resetmode
	mov al, 0
	call sr_state		; save state
	call switchmode 	; switch to PM
		; ! handle_mode_changed not called here !
		; do not call InDos or other functions using seg/sels
@@:
	call pm_reset_handlers
		; ! this calls resetmode
@@:

	numdef OVERRIDE_BUILD_PM_DEBUG, 0
 %if ! _CATCHPMINT214C && ! _OVERRIDE_BUILD_PM_DEBUG
  %fatal Cannot build DDEBUGX: handler switching without Int21.4C hook untested
 %endif
%endif

%if CATCHINTAMOUNT
%if _CATCHINT06 && _DETECT95LX
	mov cx, word [inttab_number_variable]
%else
	mov cx, inttab_number
%endif
	mov si, inttab
	mov dx, -1			; always force
.nextint:
	lodsb
	xchg ax, bx			; bl = number
	lodsw				; si -> list
	xchg ax, si			; si -> entry, ax -> list
	xchg ax, bx			; al = number, bx -> list
	call UnhookInterruptForce
	xchg bx, si			; si -> list
	loop .nextint
%endif

	pop bp
	pop dx
	pop bx
	pop ax
	pop di
	pop si
	pop cx
 %if _DEBUG_COND
	retn
 %endif
.skipints:
%endif

	clropt [reg_efl], 100h		; clear TF

	call getpsp
	mov word [pspdbe], bx

	call getint2324			; save debuggee's int 23/24, set debugger's int 23/24

	 push ss
	 pop es
	call setpspdbg			; set PSP of debugger

	setopt [internalflags], debuggerA20|debuggeeA20	; assume A20 is on
%if _PM
	call ispm
	jz .a20done			; assume A20 on. (is this the right thing to do?)
%endif
	push es
	push ds
	push di
	push si
	push cx
	xor si, si
	mov ds, si			; ds = 0000h
	dec si
	mov es, si			; es = FFFFh
	inc si				; ds:si = 0000h:0000h =  00000h
	mov di, 0010h			; es:di = FFFFh:0010h = 100000h (same address if it overflows)
	mov cx, di			; 32 byte (16 = 10h word)
	repe cmpsw			; compare them and assume A20 line switched off if same
	pop cx
	pop si
	pop di
	pop ds
	pop es
	jne .a20done			; not equal, A20 line is switched on -->
					; if equal, the A20 line is probably switched off
	clropt [internalflags], debuggerA20|debuggeeA20	; assume A20 is off

%if 0 ;_LOCALENABLEA20
	; This doesn't serve any particular reason if we have no business accessing the HMA.
	; What's more important is that the dump, assemble, and disassemble commands *should*
	; use a disabled A20 if it is disabled to the debuggee. Thus, enabling A20 belongs, if
	; at all, into getsegmented (similar to the switch to PM) as there may be breakpoints in
	; the HMA that we need to reset.
 %if _GUARD_86M_INT2F
	push es
	xor ax, ax
	mov es, ax			; (only used in 86 Mode)
	mov ax, [es:2Fh * 4]
	cmp ax, -1
	je @F			; --> (ZR)
	or ax, [es:2Fh * 4 + 2]
@@:
	pop es
	jz @F
 %endif
	mov ax, 4300h
	int 2Fh				; XMS v2 installation check
	cmp al, 80h			; installed ?
	jne .a20done			; no -->
	mov ax, 4310h
	int 2Fh				; get entry
	mov ah, 05h
	push cs
	call callfaresbx		; local enable A20
	push ds
	pop es
	dec ax
	jnz .a20done			; not able to enable A20 -->
		; actually check here to insure it is on?
	setopt [internalflags], debuggerA20	; our A20 is on
@@:
%endif
.a20done:

	mov byte [bInDbg], 1
%if _CATCHINT19
	cmp word [run_int], int19msg
	jne @F
	setopt [internalflags3], dif3_gotint19
%endif
@@:
	cmp word [run_int], progtrm
	jnz @F
	setopt [internalflags], attachedterm
@@:

%if _FORCETEXT
	call checkgfx		; see if current mode is gfx, set to text if yes
%endif

%if _VXCHG
	mov al, 1		; restore debugger screen
	call swapscreen
 %ifn _VXCHGBIOS
	push es
	mov ax, 0040h
	mov es, ax
	mov al, [es:84h]	; did the number of screen rows change?
	mov bh, [es:62h]
	mov [vpage], bh
	cmp al, [vrows]
	mov [vrows], al
	jz @F

	testopt [internalflags6], dif6_vv_mode
	jz @F

	mov dh, al		; yes. we cannot fully restore, but at least
	mov dl, 0		;  clear bottom line to ensure the debugger displays
				;  will be seen
	mov ah, 2		; set cursor position
	int 10h
	mov bl, 7		; BH=video page, BL=attribute
	mov cx, 80		; CX=columns
	mov ax, 0920h		; AL=char to display
	int 10h
@@:
	pop es
 %else
;--- with page flips, there are problems with many BIOSes:
;--- the debugger displays may get the color of the debuggee!
;--- if there's any trick to convince the BIOS not to do this,
;--- implement it here!
	mov byte [vpage], 1
 %endif
%endif

%if _ALTVID
	call setscreen
%endif

	retn

%if 0 ;_LOCALENABLEA20
callfaresbx:
	push es
	push bx
	retf
%endif


%if _ALTVID

;--- switch to debugger/debuggee screen with option /2.
;--- since DOS/BIOS is used for output, there's no guarantee that it will work.

setscreen:
	retn	; will be patched to "push ds" if "/2" cmdline switch and second adapter exists
		; (SMC in section lDEBUG_CODE)
	mov dx, [oldcrtp]
	mov bx, [oldcols]
	mov ax, [oldmr]
	mov cx, 0040h         ; 0040h is supposed to work in both rm/pm
	mov ds, cx
	mov cx, [ss:oldcsrpos]
	and byte [10h], ~ 30h
	cmp dl, 0B4h
	jnz @F
	or  byte [10h], 30h
@@:
	xchg bx, [4Ah]
	xchg cx, [50h]
	xchg dx, [63h]
	xchg al, [49h]
	xchg ah, [84h]
	pop ds
	mov [oldcrtp], dx
	mov [oldcsrpos], cx
	mov [oldcols], bx
	mov [oldmr], ax
	retn
%endif


%if _FORCETEXT
checkgfx:
	mov dx, 3CEh		; see if in graphics mode
	in al, dx
	mov bl, al
	mov al, 6
	out dx, al
	inc dx
	in al, dx
	xchg bl, al
	dec dx
	out dx, al
	test bl, 1
	jz @F
	mov ax, 0003h
	int 10h
@@:
	retn
%endif


%if _PM
		; INP:	flag for PM
		;	flag for prior PM (from remember_mode)
		;	flag for modeswitched (set if in modeswitching)
		; OUT:	seg/sels initialised for new mode, if changed
handle_mode_changed:
	mov si, convsegs
	mov cx, convsegs.amount

	call ispm
	jz .now_pm
[cpu 286]
.now_86m:
	testopt [internalflags3], dif3_prior_pm
	jz .from_no_change
%if _MMXSUPP && _MMX_REDETECT
	call mmx_redetect
%endif
.from_pm_now_86m:
.from_pm_now_86m.loop:
	lodsw
	xchg ax, di
	cmp si, convsegs.end_fixed
	ja @F
	mov dx, word [di + soaSegment]
	mov word [di + soaSegSel], dx
	loop .from_pm_now_86m.loop
	jmp .from_done_change

@@:
		; We want to switch modes to get the segment bases.
		;  First check we aren't already modeswitched.
	testopt [internalflags], modeswitched
	jnz .from_done_change		; cancel this -->

	testopt [internalflags], canswitchmode
	jnz @FF
	jmp @F

.from_pm_now_86m.loop.nonfixed.nomodeswitch:
	lodsw
	xchg ax, di
@@:
	xor dx, dx
	mov word [di + soaSegment], dx
	mov word [di + soaSegSel], dx
	loop .from_pm_now_86m.loop.nonfixed.nomodeswitch
	jmp .from_done_change

@@:
	push cx
	push di
	push si
d4	call d4message
d4	asciz "In intrtn1_code.from_pm_now_86m (switching modes to access selectors)",13,10
	setopt [internalflags], modeswitched	; set flag for resetmode
	mov al, 0
	call sr_state			; save state
	call switchmode 		; switch to PM
		; ! handle_mode_changed not called here !
		; do not call InDos or other functions using seg/sels
	pop si
	pop di
	jmp @F

.from_pm_now_86m.loop.nonfixed:
	lodsw
	xchg ax, di
	push cx
@@:
	xor bx, bx
	xchg bx, word [di + soaSelector]
	mov ax, 0006h
	int 31h
	jc @F
	test dl, 15
	jnz @F
	test cx, 0FFF0h
	jnz @F
	shr dx, 4
	shl cx, 12
	or dx, cx
	mov word [di + soaSegment], dx
	pop cx
	mov word [di + soaSegSel], dx
	loop .from_pm_now_86m.loop.nonfixed
	call resetmode			; ! only if we did the switch
		; Note: This recursively calls this function,
		;  handle_mode_changed. Because the modeswitched
		;  flag is set during this call, this only re-
		;  initialises the fixed seg/sels with the segment
		;  values. That is redundant but does no harm.
	jmp .from_done_change
__CPU__

.now_pm:
	testopt [internalflags3], dif3_prior_pm
	jnz .from_no_change
%if _MMXSUPP && _MMX_REDETECT
	call mmx_redetect
%endif
.from_86m_now_pm:
.from_86m_now_pm.loop:
	lodsw
	xchg ax, di
	cmp si, convsegs.end_fixed
	ja @F

	mov ax, word [di + soaSelector]
	test ax, ax
	jnz .no_dosdata_change
	mov bx, word [di + soaSegment]
	mov ax, 0002h
	int 31h
	mov word [di + soaSelector], ax

.no_dosdata_change:
	mov word [di + soaSegSel], ax
	jmp @FFF

@@:
		; Magic: Do not modify if called during modeswitching.
	testopt [internalflags], modeswitched
	jnz .from_done_change		; cancel this -->

	mov bx, word [di + soaSegment]
	mov ax, 0002h
	int 31h
	jnc @F
	xor ax, ax
@@:
	mov word [di + soaSegSel], ax
	mov word [di + soaSelector], ax
	and word [di + soaSegment], 0
@@:
%if $ - .from_86m_now_pm.loop > 126
	loop .from_86m_now_pm.loop_j
	jmp .from_done_change

.from_86m_now_pm.loop_j:
	jmp .from_86m_now_pm.loop
%else
	loop .from_86m_now_pm.loop
%endif
.from_done_change:
.from_no_change:


remember_mode:
	call ispm
	jnz .from_86m
.from_pm:
	setopt [internalflags3], dif3_prior_pm
	jmp @F

.from_86m:
	clropt [internalflags3], dif3_prior_pm
@@:
	retn
%endif


%if _MMXSUPP && _MMX_REDETECT && _PM
mmx_redetect:
_no386	retn
%ifn _ONLYNON386
subcpu 386
	push eax
	push ebx
	push ecx
	push edx

	mov byte [has_mmx], 0	; if no 486 or no cpuid, reset

; Copied from init.asm, debugging messages
;  and machine variable access removed.
	mov bx, sp		; save current stack pointer to align
	and sp, ~3		; align stack to avoid AC fault
	pushfd			; push original EFLAGS
	pop eax			; get original EFLAGS
	mov ecx, eax		; save original EFLAGS in ECX (including IF)

	xor eax, 40000h		; flip AC bit in EFLAGS
	and ax, ~0200h		; clear IF
	push eax		; put new EFLAGS value on stack
	popfd			; replace EFLAGS value; DI
	pushfd			; get new EFLAGS
	pop eax			; store new EFLAGS value in EAX
	mov ax, cx		; ignore low bits (including IF)
	cmp eax, ecx
	je .cpudone_stack_eax_equals_ecx	; if 80386 -->

		; Intel486 DX CPU, Intel487 SX NDP, and Intel486 SX CPU check.
		; Checking for ability to set/clear ID flag (bit 21) in EFLAGS
		; which indicates the presence of a processor with the ability
		; to use the CPUID instruction.
	mov eax, ecx		; get original EFLAGS
	xor eax, 200000h	; flip ID bit in EFLAGS
	and ax, ~0200h		; clear IF
	push eax		; save new EFLAGS value on stack
	popfd			; replace current EFLAGS value; DI
	pushfd			; get new EFLAGS
	pop eax			; store new EFLAGS in EAX
	mov ax, cx		; ignore low bits (including IF)

.cpudone_stack_eax_equals_ecx:
	push ecx
	popfd			; restore AC,ID bits and IF in EFLAGS (86 Mode)
	mov sp, bx		; restore sp

	cmp eax, ecx		; check if it's changed
	je .cpudone		; if it's a 486 (can't toggle ID bit) -->

		; Execute CPUID instruction.
subcpu 486		; NASM (at least 2.10rc1) handles cpuid itself as a
			;  586+ instruction, but we know better. So this
			;  part is declared for 486 compatibility, and only
			;  the cpuid instructions are emitted with 586
			;  compatibility to appease NASM.
	xor eax, eax		; set up input for CPUID instruction
	  [cpu 586]
	 cpuid
	  __CPU__
	cmp eax, byte 1
	jb .cpudone		; if 1 is not a valid input value for CPUID
	xor eax, eax		; otherwise, run CPUID with eax = 1
	inc eax
	  [cpu 586]
	 cpuid
	  __CPU__
	test edx, 80_0000h
	setnz byte [has_mmx]
.cpudone:
	pop edx
	pop ecx
	pop ebx
	pop eax
	retn

subcpureset
subcpureset
%endif
%endif


%if _PM && _DEBUG
		; INP:	-
		; OUT:	es = ds
		; CHG:	eax, edx, bx, cx, bp, si, di
		; STT:	in PM
		;	do not call InDos or other functions using seg/sels
pm_set_handlers:
	xor bp, bp		; = 0 if no 386
_386	inc bp
_386	inc bp			; = 2 if 386

	 push ds
	 pop es

	mov si, exctab		; hook several exceptions
%if _DEBUGUPDATESAVE
	mov di, excsave
%endif
_386	xor edx, edx		; clear edxh
	mov dx, exc_first
.loopexc:
	lodsb
	mov bl, al

%if _DEBUGUPDATESAVE
	_386_o32	; push edx
	push dx			; preserve excXX pointer
	mov ax, 0202h
		; (edxh is zero)
	int 31h			; cx:(e)dx -> prior handler
	_386_o32	; xchg eax, edx
	xchg ax, dx
	_386_o32	; stosd
	stosw			; store offset (dword on 386+, else word)
	xchg ax, cx
	stosw			; store selector
	mov ax, 4
	sub ax, bp		; if 386, ax = 4 - 2 = 2, else ax = 4
	add di, ax		; -> next entry of excsave
	_386_o32	; pop edx
	pop dx
%endif

	mov cx, word [cssel]	; -> our handler for this exception
	mov ax, 0203h
	int 31h			; set our handler
	add dx, byte exc_second - exc_first
				; -> next handler
	cmp si, endexctab	; if another to go -->
	jb .loopexc

 %if _CATCHPMINT214C
	mov si, pminttab	; ds:si -> pminttab
  %if _DEBUGUPDATESAVE
	mov di, pmintsave	; es:di -> pmintsave
  %endif
.loopint:
	lodsb			; get interrupt number
	mov bl, al		; bl = interrupt number

  %if _DEBUGUPDATESAVE
_386	xor edx, edx		; clear edxh
	mov ax, 0204h
	int 31h			; cx:(e)dx -> prior handler
	_386_o32	; xchg eax, edx
	xchg ax, dx		; (e)ax = offset
	_386_o32	; stosd
	stosw			; store offset (dword on 386+, else word)
	xchg ax, cx
	stosw			; store selector
	mov ax, 4
	sub ax, bp		; if 386, ax = 4 - 2 = 2, else ax = 4
	add di, ax		; -> next entry of pmintsave
  %endif

	lodsw			; ax -> our handler
_386	xor edx, edx
	xchg ax, dx		; (e)dx -> our handler
	mov cx, word [cssel]	; cx:(e)dx -> our handler
	mov ax, 0205h
	int 31h
	cmp si, pminttab.end
	jb .loopint
 %endif

	jmp resetmode
%endif


%if _PM && (_DEBUG || 1 || _CATCHPMINT214C)
		; INP:	-
		; OUT:	-
		; CHG:	eax, edx, bx, cx, bp, si, di
		; STT:	in PM
		;	do not call InDos or other functions using seg/sels
pm_reset_handlers:
	xor bp, bp		; = 0 if no 386
_386	inc bp
_386	inc bp			; = 2 if 386


 %if CATCHEXCAMOUNT
		; unhook exceptions
	mov di, exctab
	mov si, excsave
.loopexc:
	mov bl, [di]		; bl = exception number
	inc di
	_386_o32	; lodsd
	lodsw			; load offset (dword on 386+, else word)
	_386_o32	; xchg eax, edx
	xchg ax, dx
	lodsw			; load selector
	xchg ax, cx		; cx:(e)dx -> prior handler
	mov ax, 4
	sub ax, bp		; if 386, ax = 4 - 2 = 2, else ax = 4
	add si, ax		; -> next entry of excsave
	mov ax, 0203h
	int 31h			; set previous handler
	cmp di, endexctab	; if another to go -->
	jb .loopexc
 %endif


 %if CATCHPMINTAMOUNT
		; unhook interrupts
	mov di, pminttab	; ds:di -> pminttab
	mov si, pmintsave	; ds:si -> pmintsave
.loopint:
	mov bl, [di]		; bl = interrupt number
	add di, 3		; -> after this pminttab entry
	_386_o32	; lodsd
	lodsw			; load offset (dword on 386+, else word)
	_386_o32	; xchg eax, edx
	xchg ax, dx
	lodsw			; load selector
	xchg ax, cx		; cx:(e)dx -> prior handler
	mov ax, 4
	sub ax, bp		; if 386, ax = 4 - 2 = 2, else ax = 4
	add si, ax		; -> next entry of pmintsave
	mov ax, 0205h
	int 31h			; set previous handler
	cmp di, pminttab.end	; if another to go -->
	jb .loopint
 %endif

	jmp resetmode
%endif


%if _DEBUG1
		; INP:	bx -> dword address, word counter
		;	dx:ax = linear address to check
		; OUT:	NC if to proceed as usual
		;	CY if address matched and counter indicates trigger
		;	 (ie, this should cause the caller to fail)
		;	If the address matched, the counter has been stepped.
		;	(The result of that step indicates whether to trigger.)
		; STT:	ds = ss = debugger data selector
handle_test_case:
	cmp word [bx], ax
	jne .proceed
	cmp word [bx + 2], dx
	jne .proceed

	test word [bx + 4], 7FFFh	; is it already at a terminal state ?
	jz .no_decrement		; yes, do not further decrement -->
	dec word [bx + 4]		; decrement (towards 0 or 8000h)
	jz .trigger			; case for decrementing 1 to 0 -->
.no_decrement:
	cmp word [bx + 4], 8000h	; decrement resulted in 8000h
	je .trigger			;  or was already in that state? -->
.proceed:
	clc
	retn

.trigger:
	stc
	retn


		; INP:	bx -> first test record (dword address, word counter)
		;	dx:ax = linear address to check
		;	cx = number of consecutive test records to check
		; OUT:	NC if to proceed as usual
		;	CY if address matched and counter indicates trigger
		;	 (ie, this should cause the caller to fail)
		; CHG:	bx, cx
		; STT:	ds = ss = debugger data selector
		;
		; Note that all test cases are tried and, if the address
		;  matches, their counters are stepped. In particular,
		;  a trigger no longer causes us to skip all further
		;  test cases that may have the same address.
		; If at least one of the matching test cases indicates a
		;  trigger condition after its stepping, the return is CY.
handle_test_case_multiple_16:
	mov cx, 16
handle_test_case_multiple:
	clc
	pushf				; initialise flags on stack with NC
	jcxz .end
.loop:
	call handle_test_case
	jnc .next
	popf				; (discard)
	stc
	pushf				; new flags on stack with CY
.next:
	add bx, 6
	loop .loop
.end:
	popf				; CF
	retn
%endif


	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
default_pp_count:	dd 1
default_tp_count:	dd 1
default_tt_count:	dd 1


	usesection lDEBUG_CODE
;	PARSE_PT - Parse 'p' or 't' command. Also resets pagedcommand flag.
;	Entry	AL	First character of command
;		SI	Address of next character
;		[internalflags2] & dif2_is_pp = set if P
;		[internalflags] & tt_p = set if TP
;		both clear if T
;	Exit	BX:CX	Number of times to repeat
;	Uses	AH,BX,CX,DX.

parse_pt:
	call guard_re
	mov word [gg_deferred_message], msg.empty_message
	and word [bb_deferred_message_in_lineout_behind], 0
	clropt [internalflags2], dif2_gg_is_gg
	clropt [internalflags], \
		tt_while | tt_silent_mode | tt_no_bb | tt_no_bb_first
	and word [tt_silent_mode_number], 0

		; TP.NB, T.NB, P.NB: trace/proceed without bb breakpoints
		;
		; Note that when such a command is repeated through lastcmd,
		; it gets an empty line, thus tt_no_bb remains clear, and
		; the corresponding command without .NB is run.
	cmp al, '.'
	jne .no_dot

	lodsw
	and ax, TOUPPER_W
	cmp ax, "NB"
	je .dot_nb
		; TP.SB, T.SB, P.SB: trace/proceed without bb for first step
	cmp ax, "SB"
	je .dot_sb
	dec si
	dec si
	jmp error

.dot_sb:
	setopt [internalflags], tt_no_bb_first
	jmp .dot_common

.dot_nb:
	setopt [internalflags], tt_no_bb
.dot_common:
	lodsb
.no_dot:
	call parseql		; process =addr
	call skipcomm0		; skip any white space

	mov dx, opt3_pp_no_paging
	mov bx, word [default_pp_count + 2]
	mov cx, word [default_pp_count]
	testopt [internalflags2], dif2_is_pp
	jnz @F
	mov dx, opt3_tp_no_paging
	mov bx, word [default_tp_count + 2]
	mov cx, word [default_tp_count]
	testopt [internalflags], tt_p
	jnz @F
	mov dx, opt3_tt_no_paging
	mov bx, word [default_tt_count + 2]
	mov cx, word [default_tt_count]
@@:				; bx:cx = default count

	test word [options3], dx
	jz @F
	clropt [internalflags], pagedcommand
@@:

		; Initialise auxbuff pointers in case there is no WHILE clause.
	call tpg_initialise_empty_auxbuff

	call iseol?
	je .ppt1		; if no count given --> (uses bx:cx = default)
	call getdword		; bx:dx = given count

	push bx
	push dx			; push count
	call skipwh0
	call iseol?
	je .no_while
	dec si
	mov dx, msg.while
	call isstring?
	je .found_while

	call guard_auxbuff
	call .handle_silent	; (error if no SILENT keyword)

	jmp .no_while


.found_while:
	call guard_auxbuff
	call skipcomma
	dec si
	mov cx, si
	lodsb
	call getexpression
	call toboolean
	call skipwh0
	push dx
	push si
	call iseol?		; expect end of line here
	je .no_while_silent

	dec si
	call .handle_silent

.no_while_silent:
	pop si
	pop dx
	test dx, dx		; condition true now ?
	jnz @F

	mov dx, msg.while_not_true
	call putsz
	jmp cmd3

@@:
%if _PM
	mov bx, word [auxbuff_switchbuffer_size]
%else
	xor bx, bx
%endif
		; (si + 1) -> line terminator (13, ';', 0) or next keyword
@@:
	dec si			; -> terminator, or blank
	cmp byte [si - 1], 32	; another blank at end ?
	je @B			; yes -->
	cmp byte [si - 1], 9
	je @B			; yes -->
	mov ax, si
	sub ax, cx		; ax = length of expression
	push bx
	add bx, ax
	cmp bx, _AUXBUFFSIZE - 1
	ja error
	 mov es, word [auxbuff_segorsel]
	 			; => auxbuff
	pop bx			; -> behind switchbuffer
	mov si, cx		; -> expression start
	mov cx, ax		; = length of expression
.loop:
	lodsb
	mov byte [es:bx], al
	inc bx			; store expression
	loop .loop
	mov byte [es:bx], 0	; terminate string
	inc bx
	mov word [auxbuff_behind_while_condition], bx
	mov word [auxbuff_behind_last_silent], bx
	 push ss
	 pop es

	setopt [internalflags], tt_while
.no_while:
	pop cx
	pop bx			; bx:cx = given count

.ppt1:
	test bx, bx
	jnz @F
	test cx, cx
	jz error		; must be at least 1, else error -->
@@:
	push bx
	push cx

	clropt [internalflags2], \
		dif2_gg_is_first | dif2_gg_first_detected \
		| dif2_gg_skip_cseip | dif2_gg_skip_non_cseip

	testopt [options], tp_do_not_skip_bp
	jnz .do_not_skip_cseip
	setopt [internalflags2], dif2_gg_is_first

	cmp byte [eqflag], 0
	jne .cseip_take_eql

	_386_PM_o32		; xor ecx, ecx
	xor cx, cx
	call get_cseip_ecx_linear
	jmp .got_cseip

.cseip_take_eql:
	mov bx, word [eqladdr + 4]
	_386_PM_o32		; mov edx, dword [eqladdr]
	mov dx, word [eqladdr]
	call getlinear_d_b
.got_cseip:
	jc error
	mov word [gg_first_cseip_linear], ax
	mov word [gg_first_cseip_linear + 2], dx
.do_not_skip_cseip:

	pop cx
	pop bx

	call seteq		; make the = operand take effect
	retn


.handle_silent:
	mov dx, msg.silent
	call isstring?
	jne error
	call skipcomma
	dec si
	call skipequals
	call iseol?
	je .no_silent_mode_number

	call getword
	mov word [tt_silent_mode_number], dx
	call chkeol

.no_silent_mode_number:
	setopt [internalflags], tt_silent_mode
	retn


		; INP:	(_PM) word [auxbuff_switchbuffer_size]
		; OUT:	word [auxbuff_behind_while_condition]
		;	word [auxbuff_behind_last_silent]
		; CHG:	dx
tpg_initialise_empty_auxbuff:
%if _PM
	mov dx, word [auxbuff_switchbuffer_size]
%else
	xor dx, dx
%endif
	mov word [auxbuff_behind_while_condition], dx
	mov word [auxbuff_behind_last_silent], dx
	retn


;	PARSEQL - Parse `=' operand for `g', 'p' and `t' commands.
;	Entry	AL	First character of command
;			SI	Address of next character
;	Exit	AL	First character beyond range
;			SI	Address of the character after that
;			eqflag	Nonzero if an `=' operand was present
;			eqladdr	Address, if one was given
;	Uses AH,BX,CX,DX.

parseql:
	mov byte [eqflag], 0	; mark '=' as absent
	cmp al, '='
	jne peq1		; if no '=' operand
	call skipwhite
	mov bx, word [reg_cs]	; default segment
	call getaddrX		; get the address into bx:(e)dx
	_386_PM_o32	; mov dword [eqladdr], edx
	mov word [eqladdr+0], dx
	mov word [eqladdr+4], bx
	inc byte [eqflag]
peq1:
	retn


		; SETEQ - Copy the = arguments to their place, if appropriate.
		; (This is not done immediately, because the command may have
		; a syntax error.)
		; Uses AX.
seteq:
	cmp byte [eqflag], 0
	je .return		; if no `=' operand
	mov ax, word [eqladdr+0]
	mov word [reg_eip], ax
%if _PM
	mov ax, word [eqladdr+2]
	mov word [reg_eip+2], ax
%endif
	mov ax, word [eqladdr+4]
	mov word [reg_cs], ax
	mov byte [eqflag], 0	; clear the flag
.return:
	retn

..@run_access_end:
