(*assembler and interpreter of pascal code*)

(*k. jensen, n. wirth, ch. jacobi, eth may 76*)

program pcode(input, output, prd, prr);

(* note for the implementation.
   ===========================
this interpreter is written for the case where all the fundamental
types take one storage unit.
in an implementation all the handling of the sp pointer has to take
into account the fact taht the types may have a length different from
one. so in push and pop operations the implementor has to increase
and decrease the sp not by 1 but by a number depending on the type
concerned.
where the number of units of storage is used explicitely,
this value must not be corrected, because
the compiler has computed it taking into account the lengths of the
types involved.
the same holds for the handling of the np pointer (which must not be
corrected)							 *)

label
    1;
const
    codemax = 8650;
    pcmax = 17500;
    maxstk = 13650; (* size of variable store *)
    overi = 13655; (* size of integer constant table = 5 *)
    overr = 13660; (* size of real constant table = 5 *)
    overs = 13730; (* size of set constant table = 70 *)
    overb = 13820;
    overm = 18000;
    maxstr = 18001;
    largeint = 26144;
    begincode = 3;
    inputadr = 5;
    outputadr = 6;
    prdadr = 7;
    prradr = 8;
    duminst = 62;

type
    bit4 = 0..15;
    bit6 = 0..127;
    bit20 = -26143..26143;
    set058 = set of 0..58;
    datatype = (undef, int, reel, bool, sett, adr, mark, 
	car);
    address = -1..maxstr;
    beta = packed array [1..25] of char; (*error message*)

var
    code: array [0..codemax] of packed	(* the program *) 
	record 
	    op1: bit6;
	    p1: bit4;
	    q1: bit20;
	    op2: bit6;
	    p2: bit4;
	    q2: bit20
	end;
    pc: 0..pcmax;		(*program address register*)
    op: bit6;
    p: bit4;
    q: bit20;			(*instruction register*)

    store: array [0..overm] of 
	record 
	    case datatype of
		int: (
		    vi: integer
		);
		reel: (
		    vr: real
		);
		bool: (
		    vb: boolean
		);
		sett: (
		    vs: set058
		);
		car: (
		    vc: char
		);
		adr: (
		    va: address
		);
		(*address in store*)
		mark: (
		    vm: integer
		)
	end;
    mp, sp, np, ep: address;	(* address registers *)
    (*mp  points to beginning of a data segment
     sp  points to top of the stack
     ep  points to the stack wenn it is growth the maximum
     np  points to top of dynamicly allocated area*)

    interpreting: boolean;
    prd, prr: text; (*prd for read only, prr for write only *)

    instr: array [bit6] of alfa; (* mnemonic instruction codes *)
    cop: array [bit6] of integer;
    sptable: array [0..20] of alfa;
    (*standard functions and procedures*)

    (*localy used for interpreting one instruction*)
    ad, ad1: address;
    b: boolean;
    i, j, i1, i2: integer;
    c: char;

(*--------------------------------------------------------------------*)

    procedure load;
    const
	maxlabel = 1850;
    type
	labelst = (entered, defined); (*label situation*)
	labelrg = 0..maxlabel;			(*label range*)
	labelrec = 
	    record 
		val: address;
		st: labelst
	    end;
    var
	icp, rcp, scp, bcp, mcp: address;	(*pointers to next free position*)
	word: array [1..10] of char;
	i: integer;
	ch: char;
	labeltab: array [labelrg] of labelrec;
	labelvalue: address;

	procedure init;
	var
	    i: integer;
	begin
	    instr[0] := 'lod       ';
	    instr[1] := 'ldo       ';
	    instr[2] := 'str       ';
	    instr[3] := 'sro       ';
	    instr[4] := 'lda       ';
	    instr[5] := 'lao       ';
	    instr[6] := 'sto       ';
	    instr[7] := 'ldc       ';
	    instr[8] := '...       ';
	    instr[9] := 'ind       ';
	    instr[10] := 'inc       ';
	    instr[11] := 'mst       ';
	    instr[12] := 'cup       ';
	    instr[13] := 'ent       ';
	    instr[14] := 'ret       ';
	    instr[15] := 'csp       ';
	    instr[16] := 'ixa       ';
	    instr[17] := 'equ       ';
	    instr[18] := 'neq       ';
	    instr[19] := 'geq       ';
	    instr[20] := 'grt       ';
	    instr[21] := 'leq       ';
	    instr[22] := 'les       ';
	    instr[23] := 'ujp       ';
	    instr[24] := 'fjp       ';
	    instr[25] := 'xjp       ';
	    instr[26] := 'chk       ';
	    instr[27] := 'eof       ';
	    instr[28] := 'adi       ';
	    instr[29] := 'adr       ';
	    instr[30] := 'sbi       ';
	    instr[31] := 'sbr       ';
	    instr[32] := 'sgs       ';
	    instr[33] := 'flt       ';
	    instr[34] := 'flo       ';
	    instr[35] := 'trc       ';
	    instr[36] := 'ngi       ';
	    instr[37] := 'ngr       ';
	    instr[38] := 'sqi       ';
	    instr[39] := 'sqr       ';
	    instr[40] := 'abi       ';
	    instr[41] := 'abr       ';
	    instr[42] := 'not       ';
	    instr[43] := 'and       ';
	    instr[44] := 'ior       ';
	    instr[45] := 'dif       ';
	    instr[46] := 'int       ';
	    instr[47] := 'uni       ';
	    instr[48] := 'inn       ';
	    instr[49] := 'mod       ';
	    instr[50] := 'odd       ';
	    instr[51] := 'mpi       ';
	    instr[52] := 'mpr       ';
	    instr[53] := 'dvi       ';
	    instr[54] := 'dvr       ';
	    instr[55] := 'mov       ';
	    instr[56] := 'lca       ';
	    instr[57] := 'dec       ';
	    instr[58] := 'stp       ';
	    instr[59] := 'ord       ';
	    instr[60] := 'chr       ';
	    instr[61] := 'ujc       ';

	    sptable[0] := 'get	      ';
	    sptable[1] := 'put	      ';
	    sptable[2] := 'rst	      ';
	    sptable[3] := 'rln	      ';
	    sptable[4] := 'new	      ';
	    sptable[5] := 'wln	      ';
	    sptable[6] := 'wrs	      ';
	    sptable[7] := 'eln	      ';
	    sptable[8] := 'wri	      ';
	    sptable[9] := 'wrr	      ';
	    sptable[10] := 'wrc	      ';
	    sptable[11] := 'rdi	      ';
	    sptable[12] := 'rdr	      ';
	    sptable[13] := 'rdc	      ';
	    sptable[14] := 'sin	      ';
	    sptable[15] := 'cos	      ';
	    sptable[16] := 'exp	      ';
	    sptable[17] := 'log	      ';
	    sptable[18] := 'sqt	      ';
	    sptable[19] := 'atn	      ';
	    sptable[20] := 'sav	      ';
	    cop[0] := 105;
	    cop[1] := 65;
	    cop[2] := 70;
	    cop[3] := 75;
	    cop[6] := 80;
	    cop[9] := 85;
	    cop[10] := 90;
	    cop[26] := 95;
	    cop[57] := 100;
	    pc := begincode;
	    icp := maxstk + 1;
	    rcp := overi + 1;
	    scp := overr + 1;
	    bcp := overs + 2;
	    mcp := overb + 1;
	    for i := 1 to 10 do 
		word[i] := ' ';
	    for i := 0 to maxlabel do 
		with labeltab[i] do begin
		    val := -1;
		    st := entered
		end;
	    reset(prd)
	end; (*init*)

	procedure errorl(string: beta); (*error in loading*)
	begin
	    writeln;
	    write(string);
	    halt
	end; (*errorl*)

	procedure update(x: labelrg); (*when a label definition lx is found*)
	(*resp. current element and successor element
			      of a list of future reference*)
	var
	    curr, succ: -1..pcmax;
	    endlist: boolean;
	begin
	    if labeltab[x].st = defined then 
		errorl(' duplicated label        ')
	    else begin
		if labeltab[x].val <> -1 then begin (*forward reference(s)*)
		    curr := labeltab[x].val;
		    endlist := false;
		    while not endlist do 
			with code[curr div 2] do begin
			    if odd(curr) then begin
				succ := q2;
				q2 := labelvalue
			    end else begin
				succ := q1;
				q1 := labelvalue
			    end;
			    if succ = -1 then 
				endlist := true
			    else 
				curr := succ
			end
		end;
		labeltab[x].st := defined;
		labeltab[x].val := labelvalue
	    end
	end; (*update*)

	procedure assemble;
	forward;

	procedure generate; (*generate segment of code*)
	var
	    x: integer; (* label nummer *)
	    again: boolean;
	begin
	    again := true;
	    while again do begin
		read(prd, ch); (* first line of character*)
		case ch of
		    'i':
			readln(prd);
		    'l':
			begin
			    read(prd, x);
			    if not eoln(prd) then 
				read(prd, ch);
			    if ch = '=' then 
				read(prd, labelvalue)
			    else 
				labelvalue := pc;
			    update(x);
			    readln(prd)
			end;
		    'q':
			begin
			    again := false;
			    readln(prd)
			end;
		    ' ':
			begin
			    read(prd, ch);
			    assemble
			end
		end
	    end
	end; (*generate*)

	procedure assemble; (*translate symbolic code into machine code and store*)
	label
	    1; (*goto 1 for instructions without code generation*)
	var
	    name: alfa;
	    b: boolean;
	    r: real;
	    s: set058;
	    c1: char;
	    i, s1, lb, ub: integer;

	    procedure lookup(x: labelrg); (* search in label table*)
	    begin
		case labeltab[x].st of
		    entered:
			begin
			    q := labeltab[x].val;
			    labeltab[x].val := pc
			end;
		    defined:
			q := labeltab[x].val (*case label..*)
		end
	    end; (*lookup*)

	    procedure labelsearch;
	    var
		x: labelrg;
	    begin
		while (ch <> 'l') and not eoln(prd) do 
		    read(prd, ch);
		read(prd, x);
		lookup(x)
	    end; (*labelsearch*)

	    procedure getname;
	    begin
		word[1] := ch;
		read(prd, word[2], word[3]);
		if not eoln(prd) then 
		    read(prd, ch) (*next character*);
		pack(word, 1, name)
	    end; (*getname*)

	    procedure typesymbol;
	    var
		i: integer;
	    begin
		if ch <> 'i' then begin
		    case ch of
			'a':
			    i := 0;
			'r':
			    i := 1;
			's':
			    i := 2;
			'b':
			    i := 3;
			'c':
			    i := 4
		    end;
		    op := cop[op] + i
		end
	    end; (*typesymbol*)

	begin
	    p := 0;
	    q := 0;
	    op := 0;
	    getname;
	    instr[duminst] := name;
	    while instr[op] <> name do 
		op := op + 1;
	    if op = duminst then 
		errorl(' illegeal instruction    ');

	    case op of				(* get parameters p,q *)

		(*equ,neq,geq,grt,leq,les*)
		17, 18, 19, 20, 21, 22:
		    begin
			case ch of
			    'a':
				null; (*p = 0*)
			    'i':
				p := 1;
			    'r':
				p := 2;
			    'b':
				p := 3;
			    's':
				p := 4;
			    'c':
				p := 6;
			    'm':
				begin
				    p := 5;
				    read(prd, q)
				end
			end
		    end;

		(*lod,str*)
		0, 2:
		    begin
			typesymbol;
			read(prd, p, q)
		    end;
		4: (*lda*)
		    read(prd, p, q);
		12:
		    begin (*cup*)
			read(prd, p);
			labelsearch
		    end;

		11: (*mst*)
		    read(prd, p);

		14: (*ret*)
		    case ch of
			'p':
			    p := 0;
			'i':
			    p := 1;
			'r':
			    p := 2;
			'c':
			    p := 3;
			'b':
			    p := 4;
			'a':
			    p := 5
		    end;

		(*lao,ixa,mov*)
		5, 16, 55:
		    read(prd, q);
		(*ldo,sro,ind,inc,dec*)
		1, 3, 9, 10, 57:
		    begin
			typesymbol;
			read(prd, q)
		    end;

		(*ent,ujp,fjp,xjp*)
		23, 24, 25:
		    labelsearch;

		13:
		    begin (*ent*)
			read(prd, p);
			labelsearch
		    end;

		15:
		    begin (*csp*)
			for i := 1 to 9 do 
			    read(prd, ch);
			getname;
			while name <> sptable[q] do 
			    q := q + 1
		    end;

		7:
		    begin (*ldc*)
			case ch of		(*get q*)
			    'i':
				begin
				    p := 1;
				    read(prd, i);
				    if abs(i) >= largeint then begin
					op := 8;
					store[icp].vi := i;
					q := maxstk;
					repeat
					    q := q + 1
					until store[q].vi = i;
					if q = icp then begin
					    icp := icp + 1;
					    if icp = overi then 
						errorl(' integer table overflow  ')
					end
				    end else 
					q := i
				end;

			    'r':
				begin
				    op := 8;
				    p := 2;
				    read(prd, r);
				    store[rcp].vr := r;
				    q := overi;
				    repeat
					q := q + 1
				    until store[q].vr = r;
				    if q = rcp then begin
					rcp := rcp + 1;
					if rcp = overr then 
					    errorl(' real table overflow     ')
				    end
				end;

			    'n':
				null; (*p,q = 0*)

			    'b':
				begin
				    p := 3;
				    read(prd, q)
				end;

			    'c':
				begin
				    p := 6;
				    repeat
					read(prd, ch)
				    until ch <> ' ';
				    if ch <> '''' then 
					errorl(' illegal character       ');
				    read(prd, ch);
				    q := ord(ch);
				    read(prd, ch);
				    if ch <> '''' then 
					errorl(' illegal character       ')
				end;
			    '(':
				begin
				    op := 8;
				    p := 4;
				    s := [];
				    read(prd, ch);
				    while ch <> ')' do begin
					read(prd, s1, ch);
					s := s + [s1]
				    end;
				    store[scp].vs := s;
				    q := overr;
				    repeat
					q := q + 1
				    until store[q].vs = s;
				    if q = scp then begin
					scp := scp + 1;
					if scp = overs then 
					    errorl(' set table overflow      ')
				    end
				end
			end (*case*)
		    end;

		26:
		    begin (*chk*)
			typesymbol;
			read(prd, lb, ub);
			if op = 95 then 
			    q := lb
			else begin
			    store[bcp - 1].vi := lb;
			    store[bcp].vi := ub;
			    q := overs;
			    repeat
				q := q + 2
			    until (store[q - 1].vi = lb) and (store[q].vi = ub);
			    if q = bcp then begin
				bcp := bcp + 2;
				if bcp = overb then 
				    errorl(' boundary table overflow ')
			    end
			end
		    end;

		56:
		    begin (*lca*)
			if mcp + 16 >= overm then 
			    errorl(' multiple table overflow ');
			mcp := mcp + 16;
			q := mcp;
			for i := 0 to 15 do begin (*stringlgth*)
			    read(prd, ch);
			    store[q + i].vc := ch
			end
		    end;

		6: (*sto*)
		    typesymbol;
		27, 28, 29, 30, 31, 32, 33,
		34, 35, 36, 37, 38, 39, 40,
		41, 42, 43, 44, 45, 46, 47,
		48, 49, 50, 51, 52, 53, 54,
		58:
		    null;

		(*ord,chr*)
		59, 60:
		    goto 1;

		61: (*ujc*)
		    null
	    end (*must have same length as ujp*); (*case*)


	    (* store instruction *)
	    with code[pc div 2] do 
		if odd(pc) then begin
		    op2 := op;
		    p2 := p;
		    q2 := q
		end else begin
		    op1 := op;
		    p1 := p;
		    q1 := q
		end;
	    pc := pc + 1;
	1:
	    readln(prd)
	end; (*assemble*)

    begin (*load*)
	init;
	generate;
	pc := 0;
	generate

    end; (*load*)

(*------------------------------------------------------------------------*)


    procedure pmd;
    var
	s: integer;
	i: integer;

	procedure pt;
	begin
	    write(s: 6);
	    if abs(store[s].vi) < maxint then 
		write(store[s].vi)
	    else 
		write('too big ');
	    s := s - 1;
	    i := i + 1;
	    if i = 4 then begin
		writeln(output);
		i := 0
	    end
	end; (*pt*)

    begin
	write(' pc =', pc - 1: 5, ' op =', op: 3, '  sp =', sp: 5, '  mp =', mp: 5, '  np =', np: 5);
	writeln;
	writeln('--------------------------------------');

	s := sp;
	i := 0;
	while s >= 0 do 
	    pt;
	s := maxstk;
	while s >= np do 
	    pt
    end; (*pmd*)

    procedure errori(string: beta);
    begin
	writeln;
	writeln(string);
	pmd;
	goto 1
    end; (*errori*)

    function base(ld: integer): address;
    var
	ad: address;
    begin
	ad := mp;
	while ld > 0 do begin
	    ad := store[ad + 1].vm;
	    ld := ld - 1
	end;
	base := ad
    end; (*base*)

    procedure compare;
(*comparing is only correct if result by comparing integers will be*)
    begin
	i1 := store[sp].va;
	i2 := store[sp + 1].va;
	i := 0;
	b := true;
	while b and (i <> q) do 
	    if store[i1 + i].vi = store[i2 + i].vi then 
		i := i + 1
	    else 
		b := false
    end; (*compare*)

    procedure callsp;
    var
	line: boolean;
	adptr, adelnt: address;
	i: integer;

	procedure readi(var f: text);
	var
	    ad: address;
	begin
	    ad := store[sp - 1].va;
	    read(f, store[ad].vi);
	    store[store[sp].va].vc := f^;
	    sp := sp - 2
	end; (*readi*)

	procedure readr(var f: text);
	var
	    ad: address;
	begin
	    ad := store[sp - 1].va;
	    read(f, store[ad].vr);
	    store[store[sp].va].vc := f^;
	    sp := sp - 2
	end; (*readr*)

	procedure readc(var f: text);
	var
	    c: char;
	    ad: address;
	begin
	    read(f, c);
	    ad := store[sp - 1].va;
	    store[ad].vc := c;
	    store[store[sp].va].vc := f^;
	    store[store[sp].va].vi := ord(f^);
	    sp := sp - 2
	end; (*readc*)

	procedure writestr(var f: text);
	var
	    i, j, k: integer;
	    ad: address;
	begin
	    ad := store[sp - 3].va;
	    k := store[sp - 2].vi;
	    j := store[sp - 1].vi;
	    (* j and k are numbers of characters *)
	    if k > j then 
		for i := 1 to k - j do 
		    write(f, ' ')
	    else 
		j := k;
	    for i := 0 to j - 1 do 
		write(f, store[ad + i].vc);
	    sp := sp - 4
	end; (*writestr*)


	procedure getfile(var f: text);
	var
	    ad: address;
	begin
	    ad := store[sp].va;
	    get(f);
	    store[ad].vc := f^;
	    sp := sp - 1
	end; (*getfile*)

	procedure putfile(var f: text);
	var
	    ad: address;
	begin
	    ad := store[sp].va;
	    f^ := store[ad].vc;
	    put(f);
	    sp := sp - 1
	end; (*putfile*)

    begin (*callsp*)
	case q of
	    0: (*get*)
		case store[sp].va of
		    5:
			getfile(input);
		    6:
			errori(' get on output file      ');
		    7:
			getfile(prd);
		    8:
			errori(' get on prr file         ')
		end;
	    1: (*put*)
		case store[sp].va of
		    5:
			errori(' put on read file        ');
		    6:
			putfile(output);
		    7:
			errori(' put on prd file         ');
		    8:
			putfile(prr)
		end;
	    2:
		begin (*rst*)
		    (*for testphase*)
		    np := store[sp].va;
		    sp := sp - 1
		end;
	    3:
		begin (*rln*)
		    case store[sp].va of
			5:
			    begin
				readln(input);
				store[inputadr].vc := input^
			    end;
			6:
			    errori(' readln on output file   ');
			7:
			    begin
				readln(input);
				store[inputadr].vc := input^
			    end;
			8:
			    errori(' readln on prr file      ')
		    end;
		    sp := sp - 1
		end;
	    4:
		begin (*new*)
		    ad := np - store[sp].va;
		    (*top of stack gives the length in units of storage *)
		    if ad <= ep then 
			errori(' store overflow          ');
		    np := ad;
		    ad := store[sp - 1].va;
		    store[ad].va := np;
		    sp := sp - 2
		end;

	    5:
		begin (*wln*)
		    case store[sp].va of
			5:
			    errori(' writeln on input file   ');
			6:
			    writeln(output);
			7:
			    errori(' writeln on prd file     ');
			8:
			    writeln(prr)
		    end;
		    sp := sp - 1
		end;
	    6: (*wrs*)
		case store[sp].va of
		    5:
			errori(' write on input file     ');
		    6:
			writestr(output);
		    7:
			errori(' write on prd file       ');
		    8:
			writestr(prr)
		end;
	    7:
		begin (*eln*)
		    case store[sp].va of
			5:
			    line := eoln(input);
			6:
			    errori(' eoln output file        ');
			7:
			    line := eoln(prd);
			8:
			    errori(' eoln on prr file        ')
		    end;
		    store[sp].vb := line
		end;
	    8:
		begin (*wri*)
		    case store[sp].va of
			5:
			    errori(' write on input file     ');
			6:
			    write(output, store[sp - 2].vi: store[sp - 1].vi);
			7:
			    errori(' write on prd file       ');
			8:
			    write(prr, store[sp - 2].vi: store[sp - 1].vi)
		    end;
		    sp := sp - 3
		end;
	    9:
		begin (*wrr*)
		    case store[sp].va of
			5:
			    errori(' write on input file     ');
			6:
			    write(output, store[sp - 2].vr: store[sp - 1].vi);
			7:
			    errori(' write on prd file       ');
			8:
			    write(prr, store[sp - 2].vr: store[sp - 1].vi)
		    end;
		    sp := sp - 3
		end;
	    10:
		begin (*wrc*)
		    case store[sp].va of
			5:
			    errori(' write on input file     ');
			6:
			    write(output, store[sp - 2].vc: store[sp - 1].vi);
			7:
			    errori(' write on prd file       ');
			8:
			    write(prr, chr(store[sp - 2].vi): store[sp - 1].vi)
		    end;
		    sp := sp - 3
		end; (*rdi*)
	    11:
		case store[sp].va of
		    5:
			readi(input);
		    6:
			errori(' read on output file     ');
		    7:
			readi(prd);
		    8:
			errori(' read on prr file        ')
		end; (*rdr*)
	    12:
		case store[sp].va of
		    5:
			readr(input);
		    6:
			errori(' read on output file     ');
		    7:
			readr(prd);
		    8:
			errori(' read on prr file        ')
		end; (*rdc*)
	    13:
		case store[sp].va of
		    5:
			readc(input);
		    6:
			errori(' read on output file     ');
		    7:
			readc(prd);
		    8:
			errori(' read on prr file        ')
		end; (*sin*)
	    14:
		store[sp].vr := sin(store[sp].vr); (*cos*)
	    15:
		store[sp].vr := cos(store[sp].vr); (*exp*)
	    16:
		store[sp].vr := exp(store[sp].vr); (*log*)
	    17:
		store[sp].vr := ln(store[sp].vr); (*sqt*)
	    18:
		store[sp].vr := sqrt(store[sp].vr); (*atn*)
	    19:
		store[sp].vr := arctan(store[sp].vr); (*sav*)
	    20:
		begin
		    ad := store[sp].va;
		    store[ad].va := np;
		    sp := sp - 1
		end
	end (*case q*)
    end; (*callsp*)

begin (* main *)
    rewrite(prr);
    load; (* assembles and stores code *)
    writeln(output); (* for testing *)
    pc := 0;
    sp := -1;
    mp := 0;
    np := maxstk + 1;
    ep := 5;
    store[inputadr].vc := input^;
    store[prdadr].vc := prd^;
    interpreting := true;
    while interpreting do begin

	(*fetch*)
	with code[pc div 2] do 
	    if odd(pc) then begin
		op := op2;
		p := p2;
		q := q2
	    end else begin
		op := op1;
		p := p1;
		q := q1
	    end;
	pc := pc + 1;

	(*execute*)
	case op of

	    105, 106, 107, 108, 109, 0:
		begin (*lod*)
		    ad := base(p) + q;
		    sp := sp + 1;
		    store[sp] := store[ad]
		end;

	    65, 66, 67, 68, 69, 1:
		begin (*ldo*)
		    sp := sp + 1;
		    store[sp] := store[q]
		end;

	    70, 71, 72, 73, 74, 2:
		begin (*str*)
		    store[base(p) + q] := store[sp];
		    sp := sp - 1
		end;

	    75, 76, 77, 78, 79, 3:
		begin (*sro*)
		    store[q] := store[sp];
		    sp := sp - 1
		end;

	    4:
		begin (*lda*)
		    sp := sp + 1;
		    store[sp].va := base(p) + q
		end;

	    5:
		begin (*lao*)
		    sp := sp + 1;
		    store[sp].va := q
		end;

	    80, 81, 82, 83, 84, 6:
		begin (*sto*)
		    store[store[sp - 1].va] := store[sp];
		    sp := sp - 2
		end;

	    7:
		begin (*ldc*)
		    sp := sp + 1;
		    if p = 1 then begin
			store[sp].vi := q
		    end else if p = 6 then 
			store[sp].vc := chr(q)
		    else if p = 3 then 
			store[sp].vb := q = 1
		    else  (* load nil *)
			store[sp].va := maxstr
		end;

	    8:
		begin (*lci*)
		    sp := sp + 1;
		    store[sp] := store[q]
		end;

	    85, 86, 87, 88, 89, 9:
		begin (*ind*)
		    ad := store[sp].va + q;
		    (* q is a number of storage units *)
		    store[sp] := store[ad]
		end;

	    90, 91, 92, 93, 94, 10:
		begin (*inc*)
		    store[sp].vi := store[sp].vi + q
		end;

	    11:
		begin (*mst*)
		    (*p=level of calling procedure minus level of called
		    procedure + 1;  set dl and sl, increment sp*)
		    (* then lenth of this element is
		    		max(intsize,realsize,boolsize,charsize,ptrsize *)
		    store[sp + 2].vm := base(p);
		    (* the length of this element is ptrsize *)
		    store[sp + 3].vm := mp;
		    (* idem *)
		    store[sp + 4].vm := ep;
		    (* idem *)
		    sp := sp + 5
		end;

	    12:
		begin (*cup*)	(*p=no of locations for parameters, q=entry point*)
		    mp := sp - (p + 4);
		    store[mp + 4].vm := pc;
		    pc := q
		end;

	    13: (*ent*)
		if p = 1 then begin
		    sp := mp + q; (*q = length of dataseg*)
		    if sp > np then 
			errori(' store overflow          ')
		end else begin
		    ep := sp + q;
		    if ep > np then 
			errori(' store overflow          ')
		end;
	    (*q = max space required on stack*)

	    14:
		begin (*ret*)
		    case p of
			0:
			    sp := mp - 1;
			1, 2, 3, 4, 5:
			    sp := mp
		    end;
		    pc := store[mp + 4].vm;
		    ep := store[mp + 3].vm;
		    mp := store[mp + 2].vm
		end;

	    15: (*csp*)
		callsp;

	    16:
		begin (*ixa*)
		    i := store[sp].vi;
		    sp := sp - 1;
		    store[sp].va := q * i + store[sp].va
		end;

	    17:
		begin (*equ*)
		    sp := sp - 1;
		    case p of
			1:
			    store[sp].vb := store[sp].vi = store[sp + 1].vi;
			0:
			    store[sp].vb := store[sp].va = store[sp + 1].va;
			6:
			    store[sp].vb := store[sp].vc = store[sp + 1].vc;
			2:
			    store[sp].vb := store[sp].vr = store[sp + 1].vr;
			3:
			    store[sp].vb := store[sp].vb = store[sp + 1].vb;
			4:
			    store[sp].vb := store[sp].vs = store[sp + 1].vs;
			5:
			    begin
				compare;
				store[sp].vb := b
			    end
		    end
		end; (*case p*)

	    18:
		begin (*neq*)
		    sp := sp - 1;
		    case p of
			0:
			    store[sp].vb := store[sp].va <> store[sp + 1].va;
			1:
			    store[sp].vb := store[sp].vi <> store[sp + 1].vi;
			6:
			    store[sp].vb := store[sp].vc <> store[sp + 1].vc;
			2:
			    store[sp].vb := store[sp].vr <> store[sp + 1].vr;
			3:
			    store[sp].vb := store[sp].vb <> store[sp + 1].vb;
			4:
			    store[sp].vb := store[sp].vs <> store[sp + 1].vs;
			5:
			    begin
				compare;
				store[sp].vb := not b
			    end
		    end
		end; (*case p*)

	    19:
		begin (*geq*)
		    sp := sp - 1;
		    case p of
			0:
			    errori(' <,<=,>,>= for address   ');
			1:
			    store[sp].vb := store[sp].vi >= store[sp + 1].vi;
			6:
			    store[sp].vb := store[sp].vc >= store[sp + 1].vc;
			2:
			    store[sp].vb := store[sp].vr >= store[sp + 1].vr;
			3:
			    store[sp].vb := store[sp].vb >= store[sp + 1].vb;
			4:
			    store[sp].vb := store[sp].vs >= store[sp + 1].vs;
			5:
			    begin
				compare;
				store[sp].vb := b or (store[i1 + i].vi >= store[i2 + i].vi)
			    end
		    end
		end; (*case p*)

	    20:
		begin (*grt*)
		    sp := sp - 1;
		    case p of
			0:
			    errori(' <,<=,>,>= for address   ');
			1:
			    store[sp].vb := store[sp].vi > store[sp + 1].vi;
			6:
			    store[sp].vb := store[sp].vc > store[sp + 1].vc;
			2:
			    store[sp].vb := store[sp].vr > store[sp + 1].vr;
			3:
			    store[sp].vb := store[sp].vb > store[sp + 1].vb;
			4:
			    errori(' set inclusion           ');
			5:
			    begin
				compare;
				store[sp].vb := not b and (store[i1 + i].vi > store[i2 + i].vi)
			    end
		    end
		end; (*casep*)

	    21:
		begin (*leq*)
		    sp := sp - 1;
		    case p of
			0:
			    errori(' <,<=,>,>= for address   ');
			1:
			    store[sp].vb := store[sp].vi <= store[sp + 1].vi;
			6:
			    store[sp].vb := store[sp].vc <= store[sp + 1].vc;
			2:
			    store[sp].vb := store[sp].vr <= store[sp + 1].vr;
			3:
			    store[sp].vb := store[sp].vb <= store[sp + 1].vb;
			4:
			    store[sp].vb := store[sp].vs <= store[sp + 1].vs;
			5:
			    begin
				compare;
				store[sp].vb := b or (store[i1 + i].vi <= store[i2 + i].vi)
			    end
		    end
		end; (*case p*)

	    22:
		begin (*les*)
		    sp := sp - 1;
		    case p of
			0:
			    errori(' <,<=,>,>= for address   ');
			1:
			    store[sp].vb := store[sp].vi < store[sp + 1].vi;
			6:
			    store[sp].vb := store[sp].vc < store[sp + 1].vc;
			2:
			    store[sp].vb := store[sp].vr < store[sp + 1].vr;
			3:
			    store[sp].vb := store[sp].vb < store[sp + 1].vb;
			5:
			    begin
				compare;
				store[sp].vb := not b and (store[i1 + i].vi < store[i2 + i].vi)
			    end
		    end
		end; (*case p*)


	    23: (*ujp*)
		pc := q;

	    24:
		begin (*fjp*)
		    if not store[sp].vb then 
			pc := q;
		    sp := sp - 1
		end;

	    25:
		begin (*xjp*)
		    pc := store[sp].vi + q;
		    sp := sp - 1
		end;

	    95: (*chka*)
		if (store[sp].va < np) or (store[sp].va > maxstr - q) then 
		    errori(' bad pointer value       ');
	    96, 97, 98, 99, 26: (*chk*)
		if (store[sp].vi < store[q - 1].vi) or (store[sp].vi > store[q].vi) then 
		    errori(' value out of range      ');

	    27:
		begin (*eof*)
		    i := store[sp].vi;
		    if i = inputadr then begin
			store[sp].vb := eof(input)
		    end else 
			errori(' code in error           ')
		end;

	    28:
		begin (*adi*)
		    sp := sp - 1;
		    store[sp].vi := store[sp].vi + store[sp + 1].vi
		end;

	    29:
		begin (*adr*)
		    sp := sp - 1;
		    store[sp].vr := store[sp].vr + store[sp + 1].vr
		end;

	    30:
		begin (*sbi*)
		    sp := sp - 1;
		    store[sp].vi := store[sp].vi - store[sp + 1].vi
		end;

	    31:
		begin (*sbr*)
		    sp := sp - 1;
		    store[sp].vr := store[sp].vr - store[sp + 1].vr
		end;

	    32:
		begin (*sgs*)
		    store[sp].vs := [store[sp].vi]
		end;

	    33:
		begin (*flt*)
		    store[sp].vr := store[sp].vi
		end;

	    34:
		begin (*flo*)
		    store[sp - 1].vr := store[sp - 1].vi
		end;

	    35:
		begin (*trc*)
		    store[sp].vi := trunc(store[sp].vr)
		end;

	    36: (*ngi*)
		store[sp].vi := -store[sp].vi;

	    37: (*ngr*)
		store[sp].vr := -store[sp].vr;

	    38: (*sqi*)
		store[sp].vi := sqr(store[sp].vi);

	    39: (*sqr*)
		store[sp].vr := sqr(store[sp].vr);

	    40: (*abi*)
		store[sp].vi := abs(store[sp].vi);

	    41: (*abr*)
		store[sp].vr := abs(store[sp].vr);

	    42: (*not*)
		store[sp].vb := not store[sp].vb;

	    43:
		begin (*and*)
		    sp := sp - 1;
		    store[sp].vb := store[sp].vb and store[sp + 1].vb
		end;

	    44:
		begin (*ior*)
		    sp := sp - 1;
		    store[sp].vb := store[sp].vb or store[sp + 1].vb
		end;

	    45:
		begin (*dif*)
		    sp := sp - 1;
		    store[sp].vs := store[sp].vs - store[sp + 1].vs
		end;

	    46:
		begin (*int*)
		    sp := sp - 1;
		    store[sp].vs := store[sp].vs * store[sp + 1].vs
		end;

	    47:
		begin (*uni*)
		    sp := sp - 1;
		    store[sp].vs := store[sp].vs + store[sp + 1].vs
		end;

	    48:
		begin (*inn*)
		    sp := sp - 1;
		    i := store[sp].vi;
		    store[sp].vb := i in store[sp + 1].vs
		end;

	    49:
		begin (*mod*)
		    sp := sp - 1;
		    store[sp].vi := store[sp].vi mod store[sp + 1].vi
		end;

	    50:
		begin (*odd*)
		    store[sp].vb := odd(store[sp].vi)
		end;

	    51:
		begin (*mpi*)
		    sp := sp - 1;
		    store[sp].vi := store[sp].vi * store[sp + 1].vi
		end;

	    52:
		begin (*mpr*)
		    sp := sp - 1;
		    store[sp].vr := store[sp].vr * store[sp + 1].vr
		end;

	    53:
		begin (*dvi*)
		    sp := sp - 1;
		    store[sp].vi := store[sp].vi div store[sp + 1].vi
		end;

	    54:
		begin (*dvr*)
		    sp := sp - 1;
		    store[sp].vr := store[sp].vr / store[sp + 1].vr
		end;

	    55:
		begin (*mov*)
		    i1 := store[sp - 1].va;
		    i2 := store[sp].va;
		    sp := sp - 2;
		    for i := 0 to q - 1 do 
			store[i1 + i] := store[i2 + i]
			(* q is a number of storage units *)
		end;

	    56:
		begin (*lca*)
		    sp := sp + 1;
		    store[sp].va := q
		end;

	    100, 101, 102, 103, 104, 57:
		begin (*dec*)
		    store[sp].vi := store[sp].vi - q
		end;

	    58: (*stp*)
		interpreting := false; (*ord*)

	    59:
		begin (*only used to change the tagfield*)
		    null
		end; (*chr*)

	    60:
		begin
		    null
		end; (*ujc*)

	    61:
		errori(' case - error            ')
	end
    end; (*while interpreting*)

1:
    null
end.
