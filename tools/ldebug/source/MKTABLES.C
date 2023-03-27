/*
 * This program reads in the various data files, performs some checks,
 * and spits out the tables needed for assembly and disassembly.
 *
 * The input files are:
 *      instr.set   Instruction set
 *      instr.key   Translation from one- or two-character keys to
 *                  operand list types
 *      instr.ord   Ordering relations to enforce on the operands
 *
 * The output tables are written to DEBUGTBL.INC, which is
 * included into DEBUG.ASM.
 *
 *
 * Usage of the works is permitted provided that this
 * instrument is retained with the works, so that any entity
 * that uses the works is notified of this instrument.
 *
 * DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
 */

#ifndef DOS
#ifdef __MSDOS__
#define DOS 1
#else
#define DOS 0
#endif
#endif

#include <stdlib.h>
#if ! DOS
#include <unistd.h>
#else
#include <io.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#if DOS
#define bzero(a, b) memset(a, 0, b)
#else
#define cdecl
#endif

#define MAX_OL_TYPES       256	//82
#define MAX_OL_OFFSET	   512
#define MAX_OPTYPES	   256
#define MAX_N_ORDS          64	//30
#define LINELEN            132
#define MAX_ASM_TAB       3072	//2048
#define MAX_MNRECS         419	//400
#define MAX_SAVED_MNEMS     10
#define MAX_SLASH_ENTRIES   20
#define MAX_HASH_ENTRIES    15
#define MAX_STAR_ENTRIES    15
#define MAX_LOCKTAB_ENTRIES 53
#define MAX_AGROUP_ENTRIES  18	//14
#define MSHIFT              12 /* number of bits below machine type */
#define SORTMN              0  /* sort mnemonics */

typedef char    Boolean;
#define True    1
#define False   0

#define NUMBER(x)   (sizeof(x) / sizeof(*x))

char line[LINELEN];
const char *filename;
int lineno;

struct keytab {
 short key;  /* */
 short value;	/* index into opliststructarray */
 short width;
};
int n_keys = 0;
struct keytab olkeydict[MAX_OL_TYPES];

struct opliststruct {
	struct opliststruct * next;
	struct opliststruct * prior;
	char * olnames;
	int * ol_optype_list;
	int offsetincrement;
	int oloffset;
	int olused;
	int sequencenumber;
	int ol_is_leading_duplicate;
	int ol_is_trailing_duplicate;
};
int n_ol_types = 0;
struct opliststruct opliststructarray[MAX_OL_TYPES];

int ol_offset_to_index[MAX_OL_OFFSET];
struct opliststruct * ol_index_to_ols[MAX_OL_OFFSET];
int n_optypes = 0;
char * optypes[MAX_OPTYPES];

int n_ords = 0;
struct keytab *keyord1[MAX_N_ORDS];
struct keytab *keyord2[MAX_N_ORDS];
Boolean ordsmall[MAX_OL_TYPES];

/*
 * Equates for the assembler table.
 * These should be the same as in debug.asm.
 */

	#define ASM_END		0xFF
	#define ASM_SPECIAL	(ASM_END)
	#define ASM_A32PREF	ASM_SPECIAL
	#define ASM_O32PREF	ASM_SPECIAL
	#define ASM_A16PREF	ASM_SPECIAL
	#define ASM_O16PREF	ASM_SPECIAL
	#define ASM_DB		ASM_SPECIAL
	#define ASM_DW		ASM_SPECIAL
	#define ASM_DD		ASM_SPECIAL
	#define ASM_ORG		ASM_SPECIAL
	#define ASM_AAX		ASM_SPECIAL
	#define ASM_SEG		ASM_SPECIAL
	#define ASM_LOCKREP	ASM_SPECIAL
	#define ASM_WAIT	(ASM_SPECIAL-1)
	#define ASM_D32		(ASM_WAIT-1)
	#define ASM_D16		(ASM_D32-1)
	#define ASM_LOCKABLE	(ASM_D16-1)
	#define ASM_MACH6	(ASM_LOCKABLE-1)
	#define ASM_MACH5	(ASM_MACH6-1)
	#define ASM_MACH4	(ASM_MACH5-1)
	#define ASM_MACH3	(ASM_MACH4-1)
	#define ASM_MACH2	(ASM_MACH3-1)
	#define ASM_MACH1	(ASM_MACH2-1)
	#define ASM_ESCAPE	(ASM_MACH1-1)
	#define ASM_FIRST	ASM_ESCAPE

int n_asm_tab = 0;
unsigned char asmtab[MAX_ASM_TAB];

struct mnrec {
	struct mnrec	*next;
	char		*string;
	short		len;
	short		offset;    /* offset in mnlist */
	short		asmoffset; /* offset in asmtab */
	unsigned short	flags;
};

const unsigned short MNFLAGS_A_SUFFIX = 1;
const unsigned short MNFLAGS_O_SUFFIX = 2;
const unsigned short MNFLAGS_O_SUFFIX_REQUIRED = 4;
const unsigned short MNFLAGS_SUFFIXES = (1|2|4);
const unsigned short MNFLAGS_LOCKREPSEG = 8;
const unsigned short MNFLAGS_AAX = 16;

int num_mnrecs;
struct mnrec mnlist[MAX_MNRECS];
struct mnrec *mnhead;

int n_saved_mnems = 0;
int saved_mnem[MAX_SAVED_MNEMS];

int n_slash_entries;
int slashtab_seq[MAX_SLASH_ENTRIES];
int slashtab_mn[MAX_SLASH_ENTRIES];

int n_hash_entries;
int hashtab_seq[MAX_HASH_ENTRIES];
int hashtab_mn[MAX_HASH_ENTRIES];

int n_star_entries;
int startab_seq[MAX_STAR_ENTRIES];
int startab_mn[MAX_STAR_ENTRIES];

int n_locktab;
int locktab[MAX_LOCKTAB_ENTRIES];

int n_agroups;
int agroup_i[MAX_AGROUP_ENTRIES];
int agroup_inf[MAX_AGROUP_ENTRIES];

unsigned int n_escapes;

#ifdef __GNUC__
void fail(const char *message, ...)
	__attribute__((noreturn));
#endif

#ifdef OMIT_VOLATILE_VOID
void fail(const char *message, ...)
#else
volatile void fail(const char *message, ...)
#endif
{
	va_list args;

	va_start(args, message);
	vfprintf(stderr, message, args);
	va_end(args);
	putc('\n', stderr);
	exit(1);
}

FILE * openread(const char *path)
{
	FILE *f;

	f = fopen(path, "r");
	if (f == NULL) {
	    perror(path);
	    exit(1);
	}
	filename = path;
	lineno = 0;
	return f;
}

#ifdef __GNUC__
void linenofail(const char *message, ...)
	__attribute__((noreturn));
#endif

#ifdef OMIT_VOLATILE_VOID
void linenofail(const char *message, ...)
#else
volatile void linenofail(const char *message, ...)
#endif
{
	va_list args;

	fprintf(stderr, "Line %d of `%s':  ", lineno, filename);
	va_start(args, message);
	vfprintf(stderr, message, args);
	va_end(args);
	putc('\n', stderr);
	exit(1);
}

void * xmalloc(unsigned int len, const char *why)
{
	void *ptr = malloc(len);

	if (ptr == NULL) fail("Cannot allocate %u bytes for %s", len, why);
	return ptr;
}

Boolean get_line(FILE *ff)
{
	int n;

	for (;;) {
		if (fgets(line, LINELEN, ff) == NULL) return False;
		++lineno;
		if (line[0] == '#') continue;
		n = strlen(line) - 1;
		if (n < 0 || line[n] != '\n')
			linenofail("too long.");
		if (n > 0 && line[n-1] == '\r') --n;
		if (n == 0) continue;
		line[n] = '\0';
		return True;
	}
}

short getkey(char **pp)
{
	short key;
	char *p = *pp;

	if (*p == ' ' || *p == '\t' || *p == ';' || *p == '\0')
	    linenofail("key expected");
	key = *p++;
	if (*p != ' ' && *p != '\t' && *p != ';' && *p != '\0') {
	    key = (key << 8) | *p++;
	    if (*p != ' ' && *p != '\t' && *p != ';' && *p != '\0')
		linenofail("key too long");
	}
	*pp = p;
	return key;
}

/*
 *	Mark the given key pointer as small, as well as anything smaller than
 *	it (according to instr.ord).
 */

void marksmall(struct keytab *kp)
{
	int i;

	ordsmall[kp - olkeydict] = True;
	for (i = 0; i < n_ords; ++i)
		if (keyord2[i] == kp)
			marksmall(keyord1[i]);
}

/*
 * Add a byte to the assembler table (asmtab).
 * The format of this table is described in a long comment in debug.asm,
 * somewhere within the mini-assembler.
 */

void add_to_asmtab(unsigned long byte)
{
	if (n_asm_tab >= MAX_ASM_TAB)
		linenofail("Assembler table overflow.");
	if (byte > 0xFF)
		linenofail("Internal error: Assembler table value overflows byte.");
	asmtab[n_asm_tab++] = byte;
}


unsigned char getmachine(char **pp, Boolean dis_only)
{
	char *p = *pp;
	unsigned char value;

	if (*p != ';') return 0;
	++p;
	if (*p <= '0' || *p > '6')
		linenofail("bad machine type");
	value = *p++ - '0';
	if (!dis_only)
		add_to_asmtab(ASM_MACH1 - 1 + value);
	*pp = p;
	return value;
}

struct keytab * lookupkey(short key)
{
	struct keytab *kp;

	for (kp = olkeydict; kp < olkeydict + NUMBER(olkeydict); ++kp)
		if (key == kp->key)
			return kp;

	linenofail("can't find key %c%c", (char)(key>>8), (char)(key&0xFF) );
	return NULL;
}

char * skipwhite(char *p)
{
	while (*p == ' ' || *p == '\t') ++p;
	return p;
}

/*
 *	Data and setup stuff for the disassembler processing.
 */

/*	Data on coprocessor groups */

unsigned int fpgrouptab[] = {0xd9e8, 0xd9f0, 0xd9f8};

#define NGROUPS     9

#define GROUP(i)    (256 + 8 * ((i) - 1))
#define COPR(i)     (256 + 8 * NGROUPS + 16 * (i))
#define FPGROUP(i)  (256 + 8 * NGROUPS + 16 * 8 + 8 * (i))
#define SPARSE_BASE (256 + 8 * NGROUPS + 16 * 8 + 8 * NUMBER(fpgrouptab))

/* #define OPILLEGAL 0 */
#define OPTWOBYTE   2
#define OPGROUP     4
#define OPCOPR      6
#define OPFPGROUP   8
#define OPPREFIX    10
#define OPSIMPLE    12
#define OPTYPES     12  /* op types start here (includes simple ops) */

#define PRESEG      1   /* these should be the same as in debug.asm */
#define PREREP      2
#define PREREPZ     4
#define PRELOCK     8
#define PRE32D      0x10
#define PRE32A      0x20

/*
 *	For sparsely filled parts of the opcode map, we have counterparts
 *	to the above, which are compressed in a simple way.
 */

/*	Sparse coprocessor groups */

unsigned int sp_fpgrouptab[] = {0xd9d0, 0xd9e0, 0xdae8, 0xdbe0,
				   0xded8, 0xdfe0};

#define NSGROUPS 8	//5

#define SGROUP(i)	(SPARSE_BASE + 256 + 8 * ((i) - 1))
#define SFPGROUP(i)	(SPARSE_BASE + 256 + 8 * NSGROUPS + 8 * (i))
#define NOPS		(SPARSE_BASE + 256 + 8 * NSGROUPS + 8 * NUMBER(sp_fpgrouptab))

int optype[NOPS];
int opinfo[NOPS];
unsigned char opmach[NOPS];

/*
 * Here are the tables for the main processor groups.
 */

struct grouptab_t {
	int seq;	/* sequence number of the group */
	int info;	/* which group number it is */
} grouptab[]= {
	{0x80, GROUP(1)},	/* Intel group 1 */
	{0x81, GROUP(1)},
	{0x82, GROUP(1)},
	{0x83, GROUP(2)},
	{0xd0, GROUP(3)},	/* Intel group 2 */
	{0xd1, GROUP(3)},
	{0xd2, GROUP(4)},
	{0xd3, GROUP(4)},
	{0xc0, GROUP(5)},	/* Intel group 2a */
	{0xc1, GROUP(5)},
	{0xf6, GROUP(6)},	/* Intel group 3 */
	{0xf7, GROUP(6)},
	{0xff, GROUP(7)},	/* Intel group 5 */
	{SPARSE_BASE + 0x00, GROUP(8)}, /* Intel group 6 */
	{SPARSE_BASE + 0x01, GROUP(9)}  /* Intel group 7 */
};

/* #define NGROUPS 9 (this was done above) */

struct sp_grouptab_t {	/* sparse groups */
	int seq;	/* sequence number of the group */
	int info;	/* which group number it is */
} sp_grouptab[] = {
	{0xfe, SGROUP(1)},		/* Intel group 4 */
	{SPARSE_BASE+0xba, SGROUP(2)},	/* Intel group 8 */
	{SPARSE_BASE+0xc7, SGROUP(3)},	/* Intel group 9 */
					/* (Intel group 10 is 0F B9 = ud1) */
	{0x8f, SGROUP(4)},		/* Intel group 1a */
	{0xc6, SGROUP(5)},		/* Intel group 11 */
	{0xc7, SGROUP(5)},
	{SPARSE_BASE+0x71, SGROUP(6)},	/* Intel group 12, MMX insts psllw, psraw, psrlw */
	{SPARSE_BASE+0x72, SGROUP(7)},	/* Intel group 13, MMX insts pslld, psrad, psrld */
	{SPARSE_BASE+0x73, SGROUP(8)},	/* Intel group 14, MMX insts psllq, psrlq */
};

/* #define NSGROUPS 8 (this was done above) */

/*
 *	Creates an entry in the disassembler lookup table
 */

void entertable(int i, int type, int info)
{
	if (optype[i] != 0)
		linenofail("Duplicate information for index %d (%04Xh)", i, i);
	optype[i] = type;
	opinfo[i] = info;
}

/*
 *	Get a hex nybble from the input line or fail.
 */

int getnybble(char c)
{
	if (c >= '0' && c <= '9') return c - '0';
	if (c >= 'a' && c <= 'f') return c - 'a' + 10;
	if (c >= 'A' && c <= 'F') return c - 'A' + 10;
	linenofail("Hex digit expected instead of `%c'", c);
	return -1;
}

/*
 *	Get a hex byte from the input line and update the pointer accordingly.
 */

int getbyte(char **pp)
{
	char *p = *pp;
	int answer;

	answer = getnybble(*p++);
	answer = (answer << 4) | getnybble(*p++);
	*pp = p;
	return answer;
}

/*
 *	Get a `/r' descriptor from the input line and update the pointer
 *	accordingly.
 */

int getslash(char **pp)
{
	char *p = *pp;
	int answer;

	if (*p != '/')
		linenofail("Slash `/' expected");
	++p;
	if (*p < '0' || *p > '7')
		linenofail("Octal digit expected after slash");
	answer = *p - '0';
	++p;
	*pp = p;
	return answer;
}

int entermn(char *str, char *str_end, unsigned short flags)
{
	char *p;

	if (num_mnrecs >= MAX_MNRECS)
		linenofail("Too many mnemonics");

	if (*str == '+') {
		if (n_saved_mnems >= MAX_SAVED_MNEMS)
			linenofail("Too many mnemonics to save");
		saved_mnem[n_saved_mnems++] = num_mnrecs;
		++str;
	}

	if ( *str >= '0' && *str <= '9' )
		linenofail("Mnemonic starting with digit");

	p = xmalloc(str_end - str + 1, "mnemonic name");
	mnlist[num_mnrecs].string = p;
	mnlist[num_mnrecs].len = str_end - str;
	while (str < str_end)
		*p++ = toupper(*str++);
	*p = 0;
	mnlist[num_mnrecs].asmoffset = n_asm_tab;
	mnlist[num_mnrecs].flags = flags;
	return num_mnrecs++;
}

const char * get_mnsuffix(const struct mnrec *mnp)
{
	if ( (mnp->flags & MNFLAGS_A_SUFFIX) != 0 )
		return "_ASA";
	if ( (mnp->flags & MNFLAGS_O_SUFFIX) != 0 )
		return "_OSA";
	if ( (mnp->flags & MNFLAGS_O_SUFFIX_REQUIRED) != 0 )
		return "_OSR";
	return "";
}

/*
 *	Merge sort the indicated range of mnemonic records.
 */
#if SORTMN
struct mnrec * mn_sort(struct mnrec *start, int len)
{
	struct mnrec *p1, *p2, *answer;
	struct mnrec **headpp;
	int i;

	i = len / 2;
	if (i == 0)
		return start;

	p1 = mn_sort(start, i);
	p2 = mn_sort(start + i, len - i);
	headpp = &answer;
	for (;;) {
		if (strcmp(p1->string, p2->string) < 0) {
			*headpp = p1;
			headpp = &p1->next;
			p1 = *headpp;
			if (p1 == NULL) {
				*headpp = p2;
				break;
			}
		} else {
			*headpp = p2;
			headpp = &p2->next;
			p2 = *headpp;
			if (p2 == NULL) {
				*headpp = p1;
				break;
			}
		}
	}
	return answer;
}
#endif

/*
 *	This reads the main file, "instr.set".
 */

void read_is(FILE *f1)
{
	int i;

	entertable(0x0f, OPTWOBYTE, SPARSE_BASE);
	entertable(0x26, OPPREFIX, PRESEG | (0 << 8));	/* seg es */
	entertable(0x2e, OPPREFIX, PRESEG | (1 << 8));	/* seg cs */
	entertable(0x36, OPPREFIX, PRESEG | (2 << 8));	/* seg ss */
	entertable(0x3e, OPPREFIX, PRESEG | (3 << 8));	/* seg ds */
	entertable(0x64, OPPREFIX, PRESEG | (4 << 8));	/* seg fs */
	entertable(0x65, OPPREFIX, PRESEG | (5 << 8));	/* seg gs */
	entertable(0xf2, OPPREFIX, PREREP);		/* other prefixes */
	entertable(0xf3, OPPREFIX, PREREP | PREREPZ);
	entertable(0xf0, OPPREFIX, PRELOCK);
	entertable(0x66, OPPREFIX, PRE32D);
	entertable(0x67, OPPREFIX, PRE32A);
	opmach[0x64] = opmach[0x65] = opmach[0x66] = opmach[0x67] = 3;
	
	for (i = 0; i < NUMBER(grouptab); ++i)
		entertable(grouptab[i].seq, OPGROUP, grouptab[i].info);
	for (i = 0; i < NUMBER(sp_grouptab); ++i)
		entertable(sp_grouptab[i].seq, OPGROUP, sp_grouptab[i].info);
	for (i = 0; i < 8; ++i)
		entertable(0xd8 + i, OPCOPR, COPR(i));
	for (i = 0; i < NUMBER(fpgrouptab); ++i) {
		unsigned int j = fpgrouptab[i];
		unsigned int k = (j >> 8) - 0xd8;

		if (k > 8 || (j & 0xff) < 0xc0)
			fail("Bad value for fpgrouptab[%d]", i);
		entertable(COPR(k) + 8 + (((j & 0xff) - 0xc0) >> 3),
		OPFPGROUP, FPGROUP(i));
	}
	for (i = 0; i < NUMBER(sp_fpgrouptab); ++i) {
		unsigned int j = sp_fpgrouptab[i];
		unsigned int k = (j >> 8) - 0xd8;

		if (k > 8 || (j & 0xff) < 0xc0)
			fail("Bad value for sp_fpgrouptab[%d]", i);
		entertable(COPR(k) + 8 + (((j & 0xff) - 0xc0) >> 3),
		OPFPGROUP, SFPGROUP(i));
	}
	while (get_line(f1)) {	/* loop over lines in the file */
		int mnem;
		int mn_alt;
		char *p, *p0, *pslash, *phash, *pstar;
		unsigned char uptabidx;
		Boolean asm_only_line;
		unsigned char atab_addendum;
		unsigned short mnflags;
		long escaped;
		unsigned long escapes;

		mnflags = 0;
		asm_only_line = False;
		p0 = line;
		if (line[0] == '_') {
			asm_only_line = True;
			++p0;
		}
		atab_addendum = '\0';
		while (*p0 == '^') {
			++p0;
			uptabidx = *p0++ - '0';
			if ( uptabidx > 9 )
				linenofail("Expected digit after carat (^)");
			if ( *p0 >= '0' && *p0 <= '9' ) {
				uptabidx *= 10;
				uptabidx += *p0++ - '0';
			}
			if ( uptabidx > 15 )
				linenofail("Number %d after carat (^) too high, max 15", uptabidx);
			switch ( uptabidx )
			{
			case 15:
				mnflags |= MNFLAGS_LOCKREPSEG;
				break;
			case 14:
				atab_addendum = ASM_SPECIAL;
				break;
			case 13:
				linenofail("Required address size mnemonic suffix not supported.");
				break;
			case 12:
				mnflags |= MNFLAGS_A_SUFFIX;
				break;
			case 11:
				mnflags |= MNFLAGS_O_SUFFIX_REQUIRED;
				break;
			case 10:
				mnflags |= MNFLAGS_O_SUFFIX;
				break;
			case 5:
				atab_addendum = ASM_D32;
				break;
			case 0:
				mnflags |= MNFLAGS_AAX;
			default:
				atab_addendum = ASM_SPECIAL;
				break;
			}
		}
		p = strchr(p0, ' ');
		if (p == NULL) p = p0 + strlen(p0);

		/* check for '/', '#' and '*' separators */

		pslash = memchr(p0, '/', p - p0);
		phash = memchr(p0, '#', p - p0);
		pstar = memchr(p0, '*', p - p0);
		if (pslash != NULL) {
			mnem = entermn(p0, pslash, mnflags);
			add_to_asmtab(ASM_D16);
			++pslash;
			mn_alt = entermn(pslash, p, mnflags);
			add_to_asmtab(ASM_D32);
		} else if (phash != NULL) {
			mnem = entermn(p0, phash, mnflags);
			add_to_asmtab(ASM_D16);
			++phash;
			mn_alt = entermn(phash, p, mnflags);
			add_to_asmtab(ASM_D32);
		} else if (pstar != NULL) {
			mn_alt = entermn(p0, pstar, mnflags);	/* note the reversal */
			add_to_asmtab(ASM_WAIT);
			++pstar;
			mnem = entermn(pstar, p, mnflags);
		} else {
			mnem = entermn(p0, p, mnflags);
		}

		if (atab_addendum != '\0')
			add_to_asmtab(atab_addendum);

		atab_addendum = ASM_END;
		bzero(ordsmall, n_keys * sizeof(Boolean));
		while (*p == ' ') {		/* loop over instruction variants */
			Boolean		lockable;
			Boolean		asm_only;
			Boolean		dis_only;
			unsigned char	machine;
			unsigned long	atab_inf;
			unsigned short	atab_key;
			unsigned char	atab_xtra = 0;

			while (*p == ' ') ++p;
			asm_only = asm_only_line;
			dis_only = False;
			if (*p == '_') {	/* if assembler only */
				++p;
				asm_only = True;
			}
			else if (*p == 'D') {	/* if disassembler only */
				++p;
				dis_only = True;
			}
			lockable = False;
			if (*p == 'L') {
				++p;
				lockable = True;
				if (dis_only == False)
					add_to_asmtab(ASM_LOCKABLE);
			}
			atab_inf = i = getbyte(&p);
			if (i == 0x0F) {
				i = getbyte(&p);
				atab_inf = 0x100 + i;
				i += SPARSE_BASE;
			}
			if (optype[i] == OPGROUP) {
				int j = getslash(&p);
				int k;

				for (k = 0;; ++k) {
					if (k >= n_agroups) {
						if (++n_agroups > MAX_AGROUP_ENTRIES)
							linenofail("Too many agroup entries");
						agroup_i[k] = i;
						agroup_inf[k] = atab_inf;
						break;
					}
					if (agroup_i[k] == i)
						break;
				}
				atab_inf = 0x240 + 8 * k + j;
				i = opinfo[i] + j;
			}
			if (optype[i] == OPCOPR) {
				if (*p == '/') {
					int j = getslash(&p);

					atab_inf = 0x200 + j * 8 + (i - 0xd8);
					i = opinfo[i] + j;
				} else {
					atab_xtra = getbyte(&p);
					if (atab_xtra < 0xc0)
						linenofail("Bad second escape byte");
					i = opinfo[i] + 8 + ((atab_xtra - 0xc0) >> 3);
					if (optype[i] == OPFPGROUP)
						i = opinfo[i] + (atab_xtra & 7);
				}
			}
			if ( (mnflags & MNFLAGS_LOCKREPSEG) != 0 ) {
				add_to_asmtab(ASM_SPECIAL);
				add_to_asmtab(atab_inf);	/* Special case: store the prefix. */
				atab_addendum = '\0';
				break;
			}
			switch (*p++) {
			case '.':
				machine = getmachine(&p, dis_only);
				if (!asm_only) {
					entertable(i, OPSIMPLE, mnem);
					opmach[i] = machine;
				}
				opliststructarray[0].olused += 1;
				atab_key = 0;
				/* none of these are lockable */
				break;
			case ':': {
				struct keytab *kp = lookupkey(getkey(&p));
				int width = kp->width;
				int j;

				machine = getmachine(&p, dis_only);
				if (dis_only)
					; //atab_addendum = '\0';
				else {
					if (ordsmall[kp - olkeydict])
						linenofail("Variants out of order.");
					marksmall(kp);
				}
				atab_key = opliststructarray[kp->value].sequencenumber;
				if ((i >= 256 && i < SPARSE_BASE)
					|| i >= SPARSE_BASE + 256) {
					if (width > 2)
						linenofail("width failure");
					width = 1;
				}
				if (i & (width - 1))
					linenofail("width alignment failure");
				if (!asm_only) {
					for (j = (i == 0x90); j < width; ++j) {
						/*^^^^^^^^^  kludge for NOP instruction */
						entertable(i|j, opliststructarray[kp->value].oloffset, mnem);
						opliststructarray[kp->value].olused += 1;
						opmach[i | j] = machine;
						if (lockable) {
							if (n_locktab >= MAX_LOCKTAB_ENTRIES)
								linenofail("Too many lockable "
										   "instructions");
							locktab[n_locktab] = i | j;
							++n_locktab;
						}
					}
				} else {
					opliststructarray[kp->value].olused += 1;
				}
			 }
			 break;
			default:
				linenofail("Syntax error. (Unhandled character %c)",*(p-1));
			}
			if (atab_addendum != '\0' && dis_only == False ) {
				atab_inf = atab_inf * (unsigned short) (n_ol_types)
					+ atab_key;
				escaped = (unsigned long)atab_inf >> 8;
				escapes = 0;
				while ( escaped >= ASM_FIRST ) {
					escaped -= ASM_FIRST;
					escapes++;
					add_to_asmtab(ASM_ESCAPE);
				}
				if ( escaped < 0 )
					linenofail("ASM_ESCAPE currently must be equal to ASM_FIRST");
				if ( escapes > 1 )	// only if unusually much
					fprintf(stderr, "Line %d of `%s':  "
						"%lu assembler table escape bytes necessary.\n",
						lineno, filename, escapes );
				n_escapes += escapes;
				add_to_asmtab( escaped );
				add_to_asmtab( atab_inf & 0xFF );
				if (atab_xtra != 0)
					add_to_asmtab( atab_xtra );
			}
			if (pslash != NULL) {
				if (n_slash_entries >= MAX_SLASH_ENTRIES)
					linenofail("Too many slash entries");
				slashtab_seq[n_slash_entries] = i;
				slashtab_mn[n_slash_entries] = mn_alt;
				++n_slash_entries;
			} else if (phash != NULL) {
				if (n_hash_entries >= MAX_HASH_ENTRIES)
					linenofail("Too many hash entries");
				hashtab_seq[n_hash_entries] = i;
				hashtab_mn[n_hash_entries] = mn_alt;
				++n_hash_entries;
			} else if (pstar != NULL) {
				if (n_star_entries >= MAX_STAR_ENTRIES)
					linenofail("Too many star entries");
				startab_seq[n_star_entries] = i;
				startab_mn[n_star_entries] = mn_alt;
				++n_star_entries;
			}
		} /* end while variants */
		while (*p == ' ' || *p == '\t') p++;
		if (*p != '\0' && *p != '#')
			linenofail("Syntax error. (Trailing garbage)");
		if (atab_addendum != '\0') {
			add_to_asmtab(atab_addendum);	/* ASM_END, if applicable */
		}
	}
}

/*
 *	Print everything onto the file.
 */

struct inforec {	/* strings to put into comment fields */
	int seqno;
	char *string;
} tblcomments[] = {
	{0, "main opcode part"},
	{GROUP(1), "Intel group 1"},
	{GROUP(3), "Intel group 2"},
	{GROUP(5), "Intel group 2a"},
	{GROUP(6), "Intel group 3"},
	{GROUP(7), "Intel group 5"},
	{GROUP(8), "Intel group 6"},
	{GROUP(9), "Intel group 7"},
	{COPR(0), "Coprocessor d8"},
	{COPR(1), "Coprocessor d9"},
	{COPR(2), "Coprocessor da"},
	{COPR(3), "Coprocessor db"},
	{COPR(4), "Coprocessor dc"},
	{COPR(5), "Coprocessor dd"},
	{COPR(6), "Coprocessor de"},
	{COPR(7), "Coprocessor df"},
	{FPGROUP(0), "Coprocessor groups"},
	{-1, NULL}};

void put_dw(FILE *f2, const char *label, int *datap, int n)
{
	const char *initstr;
	int i;

	fputs(label,f2);
	while (n > 0) {
		initstr = "\tdw ";
		for (i = (n <= 8 ? n : 8); i > 0; --i) {
			fputs(initstr, f2);
			initstr = ",";
			fprintf(f2, "0%Xh", *datap++);
		}
		fputs("\n", f2);
		n -= 8;
	}
}

void dumptables(FILE *f2)
{
	int offset;
	struct mnrec *mnp;
	char *auxstr;
	struct inforec *tblptr;
	struct opliststruct *current;
	int priorolused;
	int i;
	int j;
	unsigned long k;
	unsigned int l;
	unsigned int p;

	if (num_mnrecs == 0)
		fail("No assembler mnemonics!");

	/*
	 * Sort the mnemonics alphabetically.
	 */
#if SORTMN
	mnhead = mn_sort(mnlist, num_mnrecs);
#else
	mnhead = mnlist;
	for (i = 0; i < num_mnrecs; i++)
		mnlist[i].next = &mnlist[i+1];
	mnlist[num_mnrecs-1].next = NULL;
#endif

	fprintf(f2, "\n;--- This file was generated by mktables.exe.\n" );

	/*
	 * Print out oplists[]
	 */

	fprintf(f2, "\nOPTYPES_BASE\tEQU 0%Xh\n", OPTYPES);

	fputs( "\n;--- Operand type lists.\n"
		  ";--- They were read from file INSTR.KEY.\n\n"
		  "oplists label byte\n", f2);
	for (current = &opliststructarray[0]; current != NULL; current = current->next) {
		int ot_ii;
		const char * sep = "";
		if (0 == current->ol_is_trailing_duplicate) {
			// Not used as the tail of a prior merged lead oplist.
			priorolused = 0;
		}
		fprintf(f2, "\topl ");
		for (ot_ii = 0; current->ol_optype_list[ot_ii] != 0; ++ot_ii) {
			// The for condition runs out if we wrote
			//  the entire operand list.
			fprintf(f2, "%s%s", sep, optypes[current->ol_optype_list[ot_ii]]);
			sep = ", ";	// Commas for subsequent operands.
			if (ot_ii + 1 == current->ol_is_leading_duplicate) {
				// ot_ii + 1 is the amount written so far.
				//  If it equals this field of the structure
				//  then A. the field is nonzero and B. we
				//  emitted the unique lead of the merge,
				//  the shared tail will be emitted by the
				//  next operand list structure handled.
				break;
			}
		}
		fprintf(f2, "\t; idx=%u, ofs=%Xh, used=%u",
			current->sequencenumber, current->oloffset, current->olused);
		if (0 != current->ol_is_trailing_duplicate) {
			fprintf(f2, " + %u", priorolused);
		}
		fprintf(f2, "\n");
		// Set up how often we were used plus the use count
		//  of any leads merged into us before this. This is
		//  needed to check whether this list is used at all
		//  and also retains how many times the next merged
		//  tail will be used as a tail.
		// If the next structure is not a tail to this one
		//  then the first conditional statement in this loop
		//  will reset the counter variable.
		priorolused += current->olused;
		if (0 == priorolused) {
			printf("Unused opl=\"%s\"\n", current->olnames);
		}
	}

	fprintf(f2, "\nASMMOD\tEQU opidx\n" );

	/*
	 * Dump out agroup_inf.
	 */

	fputs( "\n;--- Assembler: data on groups.\n"
		";--- If > 100h, it's a \"0F-prefix\" group.\n\n"
		"\talign 2, db 0\n"
		"agroups label word\n", f2);
	for (i = 0; i < n_agroups; ++i) {
	    fprintf(f2, "\tdw %03Xh\t; %u\n", agroup_inf[i],i);
	}

	/*
	 * Dump out the assembler indexing table (asmtab) and mnemonic list (mnlist).
	 */

	fputs( "\n;--- List of assembler mnemonics and data.\n"
		  ";--- variant's 1. argument (=a):\n"
		  ";---   if a < 0x100: one byte opcode.\n"
		  ";---   if a >= 0x100 && a < 0x200: two byte \"0F\"-opcode.\n"
		  ";---   if a >= 0x200 && a < 0x240: fp instruction.\n"
		  ";---   if a >= 0x240: refers to agroups [macro AGRP() is used].\n"
		  ";--- variant's 2. argument is index into array opindex.\n\n"
		  "mnlist label near\n"
		  "mnsuffix\n\n", f2);

	for ( p = 0, offset = 0; p < 4; p++ ) {
	for (mnp = mnhead; mnp != NULL; mnp = mnp->next) {
		/* Pass 0: Skip all suffixable mnemonics */
		if ( p == 0 && (mnp->flags & MNFLAGS_SUFFIXES) != 0 )
			continue;
		/* Pass 1: Skip all mnemonics, except those that allow an ASIZE suffix */
		if ( p == 1 && (mnp->flags & MNFLAGS_A_SUFFIX) == 0 )
			continue;
		/* Pass 2: Skip all mnemonics, except those that allow an OSIZE suffix */
		if ( p == 2 && (mnp->flags & MNFLAGS_O_SUFFIX) == 0 )
			continue;
		/* Pass 3: Skip all mnemonics, except those that require an OSIZE suffix */
		if ( p == 3 && (mnp->flags & MNFLAGS_O_SUFFIX_REQUIRED) == 0 )
			continue;

		mnp->offset = offset + 2;
		offset += mnp->len + 2;
		fprintf(f2, "\tmne %s", mnp->string );

		i = mnp->asmoffset;

		if (asmtab[i] == ASM_D16 && asmtab[i+1] == ASM_D32) {
			/* Write out ASM_D16, but don't process table.
			    The remainder of the table will be processed
			    for the 32-bit counterpart instruction. */
			fprintf(f2, ", ASM_D16\t; ofs=%Xh\n", i);
		} else if ( asmtab[i] == ASM_WAIT ) {
			/* Write out ASM_WAIT, but don't process table.
			    The remainder of the table will be processed
			    for the instruction form without WAIT prefix. */
			fprintf(f2, ", ASM_WAIT\t; ofs=%Xh\n", i);
		} else if ( (mnp->flags & MNFLAGS_LOCKREPSEG) != 0 ) {
			/* Write out an ASM_SPECIAL marker as well as the
			    prefix byte (REPxx, LOCK, or a segment prefix) */
			fprintf(f2, ", ASM_SPECIAL, %03Xh\t; ofs=%Xh\n", asmtab[i+1], i);
		} else if ( ((mnp->flags & MNFLAGS_AAX) == 0) && asmtab[i] == ASM_SPECIAL ) {
			/* Write out an ASM_SPECIAL marker, for assembler
			    directives (with no further table entries). */
			fprintf(f2, ", ASM_SPECIAL\t; ofs=%Xh\n", i);
		} else {
			j = i;
			while ( asmtab[j] == ASM_D32
				|| asmtab[j] == ASM_D16
				|| asmtab[j] == ASM_SPECIAL ) {
				switch ( asmtab[j]) {
				case ASM_SPECIAL:	/* This is only reached for AAx */
					fprintf(f2, ", ASM_SPECIAL");
					break;
				case ASM_D16:		/* This is a 16-bit counterpart instruction */
					fprintf(f2, ", ASM_D16");
					break;
				case ASM_D32:		/* This is BSWAP, or a 32-bit counterpart */
					fprintf(f2, ", ASM_D32");
					break;
				}
				j++;
			}
			fprintf(f2, "\t; ofs=%Xh\n", i );

			for (; j < n_asm_tab;) {
				Boolean lockable = False;
				unsigned char machine = 0;
				unsigned long escape_adj = 0;

				/* End the loop if all variants have been processed */
				if (asmtab[j] == ASM_END)
					break;

				/* Process and note lockability byte */
				if ( asmtab[j] == ASM_LOCKABLE) {
					lockable = True;
					j++;
				}

				/* What is this old check and comment about? */
				/* there's a problem with DEC and INC! */
				if (asmtab[j] == ASM_END) {
					/* Let's note if this branch is taken */
					fprintf(stderr, "Irregular check succeeded for"
						" mnemonic %s\n", mnp->string);
					break; }

				/* Process and note a machine requirement */
				if ( asmtab[j] >= ASM_MACH1 && asmtab[j] <= ASM_MACH6) {
					machine = asmtab[j] - ASM_MACH1 + 1;
					j++;
				}

				/* Process the escape byte.
				   This is pretty silly. Basically, handling of this is in the
				   assembler source code macros now. We still have to use the escape
				   in our internal table though because of the same reasons. */
				while ( asmtab[j] == ASM_ESCAPE ) {
					escape_adj += ( (unsigned long)ASM_ESCAPE<<8);
					j++;
				}

				/* Decode the assembler table entry:
				    get a big-endian word, */
				k = ((unsigned long)asmtab[j]<<8) + (unsigned long)asmtab[j+1];
				j += 2;
				/*  add in escaping adjustment */
				k += escape_adj;
				/*  divide by the oplist count - the remainder is our oplist, */
				l = k % (n_ol_types);
				/*  the result is the information on the actual opcode. */
				k = k / (unsigned long)(n_ol_types);

				/* Single-byte opcodes D8..DF are FPU variants. */
				if ( k >= 0xD8 && k <= 0xDF)
					fprintf( f2, "\t fpvariant ");
				else
					fprintf( f2, "\t variant ");

				/* Opcode info >= 576 refers to a group,
				    else the info is written verbatim here */
				if ( k >= 0x240 )
					fprintf(f2, "AGRP(%lu,%lu)", (k - 0x240) >> 3, (k - 0x240) & 7);
				else
					fprintf(f2, "%03lXh", k);

				/* Write our oplist's index */
				fprintf(f2, ", %u", l);

				/* If this is a FPU variant, write the additional byte */
				if ( k >= 0xD8 && k <= 0xDF) {
					fprintf(f2, ", %03Xh", asmtab[j]);
					j++;
				}

				/* If the table contained ASM_LOCKABLE, write it */
				if ( lockable == True )
					fprintf(f2, ", ASM_LOCKABLE");

				/* If the table contained a machine requirement, write it */
				if ( machine != 0 )
					fprintf(f2, ", ASM_MACH%u", machine);

				/* End of this variant */
				fprintf(f2, "\n");
			}
			fprintf(f2, "\t endvariant\n");
			i = j;
		}

	}
	/* Write a comment and the necessary label after each pass.
	    Note that the label written is that one for the NEXT pass
	    (or end_mnlist after the last pass). */
	switch ( p ) {
	case 0:
		fputs( "\n; The following mnemonics allow an address size suffix"
			"\n; but do not require it.\n"
			"\nmnlist_a_suffix_allowed label near\n"
			"mnsuffix _ASA\n\n", f2);
		break;
	case 1:
		fputs( "\n; The following mnemonics allow an operand size suffix"
			"\n; but do not require it.\n"
			"\nmnlist_o_suffix_allowed label near\n"
			"mnsuffix _OSA\n\n", f2);
		break;
	case 2:
		fputs( "\n; The following mnemonics require an operand size suffix.\n"
			"\nmnlist_o_suffix_required label near\n"
			"mnsuffix _OSR\n\n", f2);
		break;
	case 3:
		fputs( "\nend_mnlist label near\n\n", f2);
		break;
	}
	}

	if (offset >= (1 << MSHIFT)) {
		fail("%d bytes of mnemonics. That's too many.", offset);
	}

	/*
	 * Print out optype[]
	 */

	fputs( ";--- Disassembler: compressed table of the opcode types."
		  "\n;--- If the item has the format OT(xx), it refers to table 'oplists'."
		  "\n;--- Otherwise it's an offset for internal table 'disjmp'."
		  "\n\n\talign 8, db 0\n"
		  "optypes label byte", f2);
	auxstr = "\n\tdb ";
	tblptr = tblcomments;

	for (i = 0; i < SPARSE_BASE; i += 8) {
		for (j = 0; j < 8; ++j) {
			fputs(auxstr, f2);
			if ( optype[i + j] >= OPTYPES ) {
				int y = ol_offset_to_index[optype[i + j]];
				if (y == -1) {
					fail("offset not found for %u: %X", i+j, optype[i+j]);
				}
				fprintf(f2, "OT(%02X)", y );
				ol_index_to_ols[y]->olused += 1;
			} else
				fprintf(f2, "  %03Xh", optype[i + j]);
			auxstr = ",";
		}
		fprintf(f2, "\t; %02X - %02X", i, i + 7);
		if (i == tblptr->seqno) {
			fprintf(f2, " (%s)", (tblptr++)->string);
		}
		auxstr = "\n\tdb ";
	}
	auxstr = "\n;--- The rest of these are squeezed.\n" "\tdb      0,";
	for (i = SPARSE_BASE, k=1; i < NOPS; ++i)
		if ((j = optype[i]) != 0) {
			if ( j >= OPTYPES) {
				int y = ol_offset_to_index[j];
				if (y == -1) {
					fail("offset not found for %u: %X", i, j);
				}
				fprintf(f2, "%sOT(%02X)", auxstr, y);
				ol_index_to_ols[y]->olused += 1;
			} else
				fprintf(f2, "%s  %03Xh", auxstr, j);
			k++;
			if ((k & 7) == 0) {
				fprintf(f2, "\t; %02lX", k-8);
				auxstr = "\n\tdb ";
			} else
				auxstr = ",";
		}
	fputs("\n", f2);

	/*
	 * Print out opinfo[]
	 */

	fputs("\n", f2);
	for (i = 1; i < 7; i++)
		fprintf(f2, "P%u86\tequ %Xh\n", i, i << MSHIFT );

	fputs( "\n;--- Disassembler: compressed table of additional information."
		   "\n;--- Bits 0-11 usually are the offset of the mnemonics table."
		   "\n;--- Bits 12-15 are the CPU which introduced this opcode."
		   "\n\n\talign 2, db 0\n"
		   "opinfo label word\n", f2);

	for (i = 0; i < SPARSE_BASE; i += 4) {
		auxstr = "\tdw ";
		for (j = 0; j < 4; ++j) {
			fputs(auxstr, f2);
			if (opmach[i+j])
				fprintf(f2, " P%u86 +", opmach[i+j] );
			if (optype[i + j] >= OPTYPES) {
				fprintf(f2, " MN_%s%s",
				mnlist[opinfo[i+j]].string,
				get_mnsuffix(&mnlist[opinfo[i+j]]) );
			} else
				fprintf(f2, " %04Xh", opinfo[i+j] );
			auxstr = ",";
		}
		fprintf(f2, "\t; %02X\n", i);
	}
	auxstr = ";--- The rest of these are squeezed.\n" "\tdw  0,";
	for (i = SPARSE_BASE, k = 1; i < NOPS; ++i) {
		if ((j = optype[i]) != 0) {
			fprintf(f2, "%s", auxstr);
			if (opmach[i])
				fprintf(f2, " P%u86 +", opmach[i] );
			if (j >= OPTYPES) {
				fprintf(f2, " MN_%s%s",
				mnlist[opinfo[i]].string,
				get_mnsuffix(&mnlist[opinfo[i]]) );
			} else
				fprintf(f2, " %04Xh", opinfo[i] );
			k++;
			if ((k & 3) == 0) {
				fprintf(f2, "\t; %02lX", k - 4);
				auxstr = "\n\tdw ";
			} else
				auxstr = ",";
		}
	}
	fputs("\n", f2);

	/*
	 * Print out sqztab
	 */

	fputs( "\n;--- Disassembler: table converts unsqueezed numbers to squeezed."
		  "\n\n\talign 8, db 0\n"
		  "sqztab label byte\n", f2);

	k = 0;
	for (i = SPARSE_BASE; i < NOPS; i += 8) {
		auxstr = "\tdb ";
		for (j = 0; j < 8; ++j) {
			fprintf(f2, "%s%3lu", auxstr, optype[i + j] == 0 ? 0 : ++k);
			auxstr = ",";
		}
		fprintf(f2, "\t; %X\n", i);
	}

	/*
	 * Print out the cleanup tables.
	 */

	fputs("\n;--- Disassembler: table of mnemonics that change in the "
		  "presence of a WAIT" "\n;--- instruction.\n\n"
		  "\talign 2, db 0\n", f2);
	put_dw(f2, "wtab1 label word\n", startab_seq, n_star_entries);
#if 0
	for (i = 0; i < n_star_entries; ++i)
		startab_mn[i] = mnlist[startab_mn[i]].offset;
	put_dw(f2, "wtab2 label word\n", startab_mn, n_star_entries);
#else
	fputs("wtab2 label word\n", f2);
	for (i = 0; i < n_star_entries; ++i)
	fprintf(f2, "\tdw MN_%s%s\n",
		mnlist[startab_mn[i]].string,
		get_mnsuffix(&mnlist[startab_mn[i]]) );
#endif
	fprintf(f2, "N_WTAB\tequ ($ - wtab2) / 2\n");

	fputs("\n;--- Disassembler: table for operands which have a different "
		"mnemonic for" "\n;--- their 32 bit versions (66h prefix).\n\n"
		"\talign 2, db 0\n", f2);
	put_dw(f2, "ltabo1 label word\n", slashtab_seq, n_slash_entries);
#if 0
	for (i = 0; i < n_slash_entries; ++i)
		slashtab_mn[i] = mnlist[slashtab_mn[i]].offset;
	put_dw(f2, "ltabo2 label word\n", slashtab_mn, n_slash_entries);
#else
	fputs("ltabo2 label word\n", f2);
	for (i = 0; i < n_slash_entries; ++i)
	fprintf(f2, "\tdw MN_%s%s\n",
		mnlist[slashtab_mn[i]].string,
		get_mnsuffix(&mnlist[slashtab_mn[i]]) );
#endif
	fprintf(f2, "N_LTABO\tequ ($ - ltabo2) / 2\n" );

	fputs("\n;--- Disassembler: table for operands which have a different "
		"mnemonic for"  "\n;--- their 32 bit versions (67h prefix).\n\n"
		"\talign 2, db 0\n", f2);
	put_dw(f2, "ltaba1 label word\n", hashtab_seq, n_hash_entries);
#if 0
	for (i = 0; i < n_hash_entries; ++i)
		hashtab_mn[i] = mnlist[hashtab_mn[i]].offset;
	put_dw(f2, "ltaba2 label word\n", hashtab_mn, n_hash_entries);
#else
	fputs("ltaba2 label word\n", f2);
	for (i = 0; i < n_hash_entries; ++i)
	fprintf(f2, "\tdw MN_%s%s\n",
		mnlist[hashtab_mn[i]].string,
		get_mnsuffix(&mnlist[hashtab_mn[i]]) );
#endif
	fprintf(f2, "N_LTABA\tequ ($ - ltaba2) / 2\n" );

	fputs("\n;--- Disassembler: table of lockable instructions\n\n"
		"\talign 2, db 0\n", f2);
	put_dw(f2, "locktab label word\n", locktab, n_locktab);
	fprintf( f2, "N_LOCK\tequ ($ - locktab) / 2\n" );

	/*
	 * Print out miscellaneous equates.
	 */

	fprintf(f2, "\n;--- Equates used in the assembly-language code.\n\n"
			"SPARSE_BASE\tequ 0%lXh\n"
			"SFPGROUP3\tequ 0%lXh\n"
			"GROUP7\t\tequ 0%lXh\n"
			"SGROUP4\t\tequ 0%lXh\n",
			(long unsigned)SPARSE_BASE,
			(long unsigned)SFPGROUP(3),
			(long unsigned)GROUP(7),
			(long unsigned)SGROUP(4));
}

int intarraycmp(const int* a, const int* b) {
	for (; *a != 0; ++a, ++b) {
		if (*a != *b) {
			return 1;
		}
	}
	if (*b != 0) {
		return 1;
	}
	return 0;
}

int intarraylen(const int* a) {
	int count;
	for (count = 0; a[count] != 0; ++count) {}
	return count;
}

int cdecl main(int argc, char* argv[])
{
	FILE *f1;
	FILE *f2;
	struct opliststruct *current;
	int offset, prioroffset;
	int ii;
	int argii;
	int ofstoidxii;
	int ofsii;
	unsigned switchdirection = 1;
	unsigned switchstackhinting = 1;
	unsigned switchmerge = 1;
	unsigned switchdebug = 0;

	for (argii = 1; argii < argc; ++ argii) {
		if (strcmp(argv[argii], "nodirection") == 0) {
			switchdirection = 0;
		} else if (strcmp(argv[argii], "direction") == 0) {
			switchdirection = 1;
		} else if (strcmp(argv[argii], "nostackhinting") == 0) {
			switchstackhinting = 0;
		} else if (strcmp(argv[argii], "stackhinting") == 0) {
			switchstackhinting = 1;
		} else if (strcmp(argv[argii], "nomerge") == 0) {
			switchmerge = 0;
		} else if (strcmp(argv[argii], "merge") == 0) {
			switchmerge = 1;
		} else if (strcmp(argv[argii], "nodebug") == 0) {
			switchdebug = 0;
		} else if (strcmp(argv[argii], "debug") == 0) {
			switchdebug = 1;
		} else {
			fail("Invalid argument: \"%s\"", argv[argii]);
		}
	}

	/*
	 * Read in the key dictionary.
	 */

	f1 = openread("instr.key");
	optypes[n_optypes] = "";
	++ n_optypes;
	optypes[n_optypes] = "OP_END";
	++ n_optypes;
	offset = prioroffset = OPTYPES;
	for (ofstoidxii = 0; ofstoidxii < MAX_OL_OFFSET; ++ofstoidxii) {
		ol_offset_to_index[ofstoidxii] = -1;
	}
	while (get_line(f1)) {
		int * optypearray;
		int offsetincrement = 0;
		int ota_ii = 0;
		char *p = line;
		char *q = strchr(p, ';');
		char *qq;
		int i;
		unsigned filterii;
		const char ** filterstrarray;
		const char ** filterstrarrayarray[] =
			{ NULL, NULL, NULL };
		const char * dirstrarray[] =
			{ "OP_M_SRC_DST",
			  "OP_M_SRC",
			  "OP_M_DST",
			  "" };
		const char * stackstrarray[] =
			{ "OP_STACK_PUSH",
			  "OP_STACK_POP",
			  "OP_STACK_SPECIAL",
			  "" };

		if (q) {
			*q = '\0';
			q--;
			while (q > p && (*q == ' ' || *q == '\t')) {
				*q = '\0';
				q--;
			}
		}

		if (n_keys >= MAX_OL_TYPES)
			fail("Too many keys.");
		olkeydict[n_keys].key = getkey(&p);
		p = skipwhite(p);
		qq = xmalloc(strlen(p) + 1, "operand type name");
		strcpy(qq, p);
		filterii = 0;
		if (switchdirection == 0) {
			filterstrarrayarray[filterii++] = dirstrarray;
		}
		if (switchstackhinting == 0) {
			filterstrarrayarray[filterii++] = stackstrarray;
		}
		for (filterii = 0; (filterstrarray = filterstrarrayarray[filterii]); ++filterii) {
			char const *filterstr;
			char *pp;
			unsigned ii;
			for (ii = 0; *(filterstr = filterstrarray[ii]); ++ii) {
				while ( (pp = strstr(qq, filterstr)) ) {
					char *kk = pp;
					kk += strlen(filterstr);
					while (*kk == ' ' ||
						*kk == '\t' ||
						*kk == ',')
						++kk;
					memmove(pp, kk, strlen(kk) + 1);
				}
			}
		}
		if (*qq) {
			char * qqq = qq;
			for (;;) {
				++offsetincrement;
				qqq = strchr(qqq, ',');
				if (qqq == NULL) break;
				++qqq;
			}
		}
		++ offsetincrement;
		optypearray = xmalloc(sizeof(int) * (offsetincrement + 1), "operand type array");
		if (*qq) {
			char * qqq = qq;
			for (;;) {
				int ot_ii;
				int found = 0;
				int optypenamelen;
				char * optypenamestart = qqq;
				char * optypenameend;
				qqq = strchr(qqq, ',');
				if (qqq) {
					optypenameend = qqq;
				} else {
					optypenameend = optypenamestart + strlen(optypenamestart);
				}
				while (optypenameend > optypenamestart &&
					(*optypenamestart == ' ' ||
					*optypenamestart == '\t')) { ++optypenamestart; }
				while (optypenameend > optypenamestart &&
					(*(optypenameend - 1) == ' ' ||
					*(optypenameend - 1) == '\t')) { --optypenameend; }
				optypenamelen = optypenameend - optypenamestart;
				if (0 == optypenamelen) {
					fail("Invalid operand type name in qq=\"%s\"", qq);
				}
				for (ot_ii = 2; ot_ii < n_optypes; ++ot_ii) {
					if (0 != memcmp(optypes[ot_ii], optypenamestart, optypenamelen)) {
						continue;
					}
					if (0 != optypes[ot_ii][optypenamelen]) {
						continue;
					}
					found = ot_ii;
				}
				if (0 == found) {
					if (n_optypes >= MAX_OPTYPES) {
						fail("Too many operand types in array");
					}
					optypes[n_optypes] = xmalloc(optypenamelen + 1, "operand type name in array");
					memcpy(optypes[n_optypes], optypenamestart, optypenamelen);
					optypes[n_optypes][optypenamelen] = 0;
					found = n_optypes;
					++n_optypes;
				}
				if (ota_ii > offsetincrement) {
					fail("Operand type size mismatch");
				}
				optypearray[ota_ii] = found;
				++ ota_ii;
				if (qqq == NULL) break;
				++qqq;
			}
		}
		if (ota_ii != offsetincrement - 1) {
			fail("Operand type size mismatch");
		}
		optypearray[ota_ii] = 1;	/* OP_END */
		++ ota_ii;
		optypearray[ota_ii] = 0;	/* end of list */
		++ ota_ii;

		for (i = 0;; ++i) {
			if (i >= n_ol_types) {
				struct opliststruct * ols = &opliststructarray[n_ol_types];
				if (n_ol_types >= MAX_OL_TYPES)
					fail("Too many operand list types.");
				ols->ol_optype_list = optypearray;
				ols->olnames = qq;
				ols->offsetincrement = offsetincrement;
				if (offset == OPTYPES) {
					if (*qq || n_ol_types) {
						fail("First oplist expected empty in qq=\"%s\"", qq);
					}
				}
				offset += offsetincrement;
				if (switchdebug) {
				  printf("idx=%d qq=\"%s\"", n_ol_types, qq);
				  for (ota_ii = 0; 0 != optypearray[ota_ii]; ++ota_ii) {
					printf(" ot=%d \"%s\"", optypearray[ota_ii], optypes[optypearray[ota_ii]]);
				  }
				  printf(" ot=%d \"%s\"", optypearray[ota_ii], optypes[optypearray[ota_ii]]);
				  printf("\n");
				}
				++n_ol_types;
				break;
			}

			if (intarraycmp(optypearray, opliststructarray[i].ol_optype_list) == 0) {
				free(optypearray);
				free(qq);
				break;
			}
		}
		olkeydict[n_keys].value = i;
		olkeydict[n_keys].width = 1;
		if (strstr(p, "OP_ALL") != NULL)
			olkeydict[n_keys].width = 2;
		else if (strstr(p, "OP_R_ADD") != NULL)
			olkeydict[n_keys].width = 8;
		++n_keys;
		if ( (unsigned)(offset + 1 - n_ol_types * 2) >= 256)
			fail("Offset=%d n_ol_types=%d of operand list too large.",
				offset, n_ol_types);
	}
	fclose(f1);
	for (ofsii = 0; ofsii < n_ol_types; ++ofsii) {
		int jj = opliststructarray[ofsii].offsetincrement;
		int kk = intarraylen(opliststructarray[ofsii].ol_optype_list);
		if (jj != kk) {
			fail("Offset differs qq=\"%s\" jj=%d kk=%d",
				opliststructarray[ofsii].olnames, jj, kk);
		}
	}
	current = &opliststructarray[0];
	// Link together all saved oplist structures in the array.
	//  This is a doubly linked list for fast re-ordering.
	// The first structure is always at index 0 of the array.
	//  The last structure has its prior pointer initialised
	//  but its next pointer is left zero-initialised (NULL).
	for (ofsii = 1; ofsii < n_ol_types; ++ofsii) {
		struct opliststruct *next = &opliststructarray[ofsii];
		current->next = next;
		next->prior = current;
		current = next;
	}

	if (switchmerge) {
		// Note that this always has both current and other
		//  as the 2nd entry in the linked list initially.
		//  The 1st entry (also index 0 of the array) is
		//  special and does not participate in merging.
		// (It is expected that the first entry will be
		//  the empty oplist (just an OP_END) and that its
		//  offset will be the first oplist offset.)
	  for (current = opliststructarray[0].next; current != NULL; current = current->next) {
		struct opliststruct *other;
		for (other = opliststructarray[0].next; other != NULL; other = other->next) {
			if (other == current) {
				// Cannot merge with itself.
				continue;
			}
			if (other->ol_is_trailing_duplicate) {
				// Cannot merge other as the tail if it
				//  already is the tail of another.
				continue;
			}
			if (current->ol_is_leading_duplicate) {
				// Cannot merge current as the lead if it
				//  already is the lead of another.
				continue;
			}
			// Now other can become the tail merged to behind
			//  current, if current is longer than other and
			//  the array contents match.
			// This next check will skip this other if it is
			//  longer than or the same length as current.
			if (current->offsetincrement <= other->offsetincrement) {
				// The current is shorter or the same
				//  length, cannot merge. (Duplicates
				//  shouldn't exist here anymore.)
				continue;
			}
			if (0 != memcmp(current->ol_optype_list +
					current->offsetincrement -
					other->offsetincrement,
					other->ol_optype_list,
					sizeof(int) * other->offsetincrement)) {
				// Entire array of other (shareable array tail)
				//  does not match the tail of current.
				continue;
			}
			// Can set up the merge.
			if (switchdebug) {
			  printf("Match candidate current=\"%s\" len=%d other=\"%s\" len=%d\n",
				current->olnames, current->offsetincrement,
				other->olnames, other->offsetincrement);
			}
			// Check if we already have other pointed to by
			//  current's next pointer. If yes, no work needed.
			if (current->next != other) {
				struct opliststruct *end = other;
				/* find end of contiguous block */
				while (end->ol_is_leading_duplicate) {
					end = end->next;
					if (end == NULL) {
						fail("Invalid linked list pointer");
					}
				}
				/* unlink contiguous block */
				if (end->next) {
					end->next->prior = other->prior;
				}
				other->prior->next = end->next;
				/* link the block behind current */
				end->next = current->next;
				if (current->next) {
					current->next->prior = end;
				}
				other->prior = current;
				current->next = other;
				// Now current points to other as the next.
			}
			// This field holds the amount of entries to be
			//  emitted for current, also. Must be at least one
			//  in order to indicate this is a lead.
			current->ol_is_leading_duplicate =
				current->offsetincrement - other->offsetincrement;
			if (!current->ol_is_leading_duplicate) {
				fail("Invalid merged lead length");
			}
			// Indicate in other that it is used as a tail.
			other->ol_is_trailing_duplicate = 1;
			if (switchdebug) {
			  printf("leading=%d\n", current->ol_is_leading_duplicate);
			}
			/* search from new start again */
			current = other = &opliststructarray[0];
			break;
		}
	  }
	}

	// This starts with the array index 0, which is special.
	//  It did not participate in the merging and always gets
	//  the start offset value.
	offset = OPTYPES;
	current = &opliststructarray[0];
	for (ii = 0; current != NULL; current = current->next, ++ii) {
		int jj = current->offsetincrement;
		if (current->ol_is_leading_duplicate) {
			jj = current->ol_is_leading_duplicate;
		}
		// jj holds how many operands to emit for this oplist.
		//  If this is a merged lead it holds the length of the
		//  lead that is unique to this oplist, after which
		//  the shared tail comes which is handled in the next
		//  oplist structure (found by the next pointer).

		current->sequencenumber = ii;
		ol_index_to_ols[ii] = current;
		current->oloffset = offset;
		if (offset >= MAX_OL_OFFSET) {
			fail("Offset too large");
		}
		if (ol_offset_to_index[offset] != -1) {
			fail("Offset already in use, offset=%u index=%u",
				offset, ol_offset_to_index[offset]);
		}
		ol_offset_to_index[offset] = ii;
		if ( (unsigned)(offset + 1 - ii * 2) >= 256)
			fail("Offset=%d ii=%d of operand list too large.",
				offset, ii);
		offset += jj;
	}

	/*
	 * Read in the ordering relations.
	 */

	f1 = openread("instr.ord");
	while (get_line(f1)) {
		char *p = line;

		if (n_ords >= MAX_N_ORDS)
			fail ("Too many ordering restrictions.");
		keyord1[n_ords] = lookupkey(getkey(&p));
		p = skipwhite(p);
		keyord2[n_ords] = lookupkey(getkey(&p));
		if (*p != '\0')
			fail("Syntax error in ordering file.");
		++n_ords;
	}
	fclose(f1);

	/*
	 * Do the main processing.
	 */

	f1 = openread("instr.set");
	read_is(f1);
	fclose(f1);

	/*
	 * Write the file.
	 */

	f2 = fopen("debugtbl.tmp", "w");
	if (f2 == NULL) {
		perror("debugtbl.tmp");
		exit(1);
	}

	dumptables(f2);

	fclose(f2);

	/*
	 * Move the file to its original position.
	 */

	unlink("debugtbl.old");

	if (rename("debugtbl.inc", "debugtbl.old") == -1) {
		perror("rename debugtbl.inc -> debugtbl.old");
		//return 1;
	}
	if (rename("debugtbl.tmp", "debugtbl.inc") == -1) {
		perror("rename debugtbl.tmp -> debugtbl.inc");
		return 1;
	}

	if (n_escapes)
		printf("Escapes used: %u\n", n_escapes);
	puts("Done.");

	return 0;
}
