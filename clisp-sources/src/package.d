/*
 * Package Management for CLISP
 * Bruno Haible 1990-2005
 * Sam Steingold 1999-2004
 * German comments translated into English: Stefan Kain 2002-02-20
 */

#include "lispbibl.c"
#include "arilev0.c" /* for hashcode calculation */

/* data structure of the symbols: see LISPBIBL.D
 data structure of the symbol table:
 a vector with 3 Slots:
   size    Fixnum >0, <2^24, = length of the table
   table   vector of length size,
             contains single symbols (/= NIL) and symbol-lists
   count   number of symbols in the table, Fixnum >=0 */
#define Symtab_size(symtab)  (TheSvector(symtab)->data[0])
#define Symtab_table(symtab)  (TheSvector(symtab)->data[1])
#define Symtab_count(symtab)  (TheSvector(symtab)->data[2])
/* consistency rule: for each string there is in the table
   at most one symbol with this printname. */

/* UP: Creates a new empty symbol-table.
 make_symtab(size)
 > size: the desired size of the table (odd number, >0, <2^24)
 < result: new symbol-table of this size
 can trigger GC */
local maygc object make_symtab (uintL size) {
  var object table = allocate_vector(size); /* vector with size NIL-entries */
  pushSTACK(table);
  var object symtab = allocate_vector(3); /* vector of length 3 */
  Symtab_table(symtab) = popSTACK(); /* insert table */
  Symtab_size(symtab) = fixnum(size); /* insert size */
  Symtab_count(symtab) = Fixnum_0; /* insert count := 0 */
  return symtab;
}

/* UP: Calculates the hashcode of a string. This is a 24-bit-number.
 string_hashcode(string,invert)
 > string: a string
 > invert: whether to implicitly case-invert the string
 < result: the hashcode of the string */
local uint32 string_hashcode (object string, bool invert) {
  var uintL len;
  var uintL offset;
  string = unpack_string_ro(string,&len,&offset);
  var uint32 hashcode = 0; /* hashcode, only the lower 24 Bit count */
  if (len > 0) {
    SstringDispatch(string,X, {
      var const cintX* charptr = &((SstringX)TheVarobject(string))->data[offset];
      /* there are len characters, starting at charptr */
      /* Look at all len characters, not just at the first min(len,16)
         characters, as we did earlier, because a bad hash function quasi
         turns the hash table into a few long linear lists. */
      var uintC count;
      dotimesC(count, len, {
        /* rotate hashcode by 5 bits to the left: */
        hashcode = hashcode << 5; hashcode = hashcode + (hashcode >> 24);
        /* 'add' next byte via XOR: */
        var cintX c = *charptr++;
        hashcode = hashcode ^ (uint32)(invert ? as_cint(invert_case(as_chart(c))) : c);
      });
    });
  }
  return hashcode & 0x00FFFFFF;
}

/* UP: Reorganizes a symbol-table, after it has grown, and
 tries to save Conses.
 rehash_symtab(symtab)
 > symtab: symbol-table
 < result: reorganized symbol-table (EQ to the first).
 call only, if BREAK_SEM_2 is set
 can trigger GC */
local maygc object rehash_symtab (object symtab);

/* auxiliary functions: */

/* takes a Cons from free-conses or returns a fresh one.
 new_cons()
 < result: new Cons.
 stack layout: free-conses, newtable, listr, symbol, entry.
 can trigger GC */
local maygc object new_cons (void) {
  var object free = STACK_4; /* free-conses */
  if (!nullp(free)) {
    STACK_4 = Cdr(free); /* shorten free-conses */
    return free;
  } else {
    return allocate_cons(); /* request new Cons from memory-management */
  }
}

/* inserts an additional symbol into the new table.
 newinsert(sym,size);
 > sym: symbol
 stack layout: tab, oldtable, free-conses, newtable, listr.
 can trigger GC */
local maygc void newinsert (object sym, uintL size) {
  var uintL index = /* Index = Hashcode mod size */
    string_hashcode(Symbol_name(sym),false) % size;
  /* entry in the newtable */
  var object entry = TheSvector(STACK_1)->data[index];
  if ((!nullp(entry)) || nullp(sym)) {
    /* if entry=NIL and sym/=NIL, then simply enter sym.
       else, entry must be extended by cons-ing: */
    pushSTACK(sym); /* save symbol */
    pushSTACK(entry); /* save entry */
    if (!listp(entry)) {
      /* if entry is not a list, replace with (new-cons entry NIL): */
      var object new_entry = new_cons();
      Cdr(new_entry) = NIL; Car(new_entry) = STACK_0;
      STACK_0 = new_entry;
    }
    /* and cons symbol in front of it: */
    var object new_entry = new_cons();
    Cdr(new_entry) = popSTACK(); /* enter entry resp. list as CDR */
    Car(new_entry) = popSTACK(); /* enter symbol as CAR */
    sym = new_entry; /* and then enter new_entry */
  }
  TheSvector(STACK_1)->data[index] = sym; /* enter new entry in newtable */
}

local object rehash_symtab (object symtab) {
  pushSTACK(symtab); /* save symbol-table */
  var uintL oldsize = posfixnum_to_V(Symtab_size(symtab)); /* old size */
  var uintL newsize; /* new size */
  var object size; /* new size (as Fixnum) */
  pushSTACK(Symtab_table(symtab)); /* oldtable = old table-vector */
  pushSTACK(NIL); /* free-conses := NIL */
 #ifdef TYPECODES /* Svector_length is limited to max. 2^32-1 */
  /* new size = min(floor(oldsize*1.6),2^32-1) */
  { /* multiply oldsize (>0, <2^32) with 1.6*2^31, then divide by 2^31 : */
    var uint32 prod_hi;
    var uint32 prod_lo;
    mulu32(oldsize,3435973888UL, prod_hi=,prod_lo=);
    newsize =
      (prod_hi < (1UL<<31) ? (prod_hi << 1) | (prod_lo >> 31) : (1UL<<31)-1 );
  }
 #else /* Svector_length is limited to max. 2^24-1 */
  /* new size = min(floor(oldsize*1.6),2^24-1) */
  { /* multiply oldsize (>0, <2^24) with 1.6*2^7, then divide by 2^7 : */
    var uint32 prod = oldsize * 205UL;
    newsize = (prod < (1UL<<31) ? prod>>7 : (1UL<<24)-1 );
  } /* newsize is now >= oldsize > 0 and < 2^24 */
 #endif
  /* make newsize odd by rounding off: */
  newsize = (newsize - 1) | 1 ;
  /* calculate size: */
  size = fixnum(newsize);
  /* if newsize <= oldsize , the table does not need to be enlarged: */
  if (newsize <= oldsize) {
    skipSTACK(3);
    return symtab;
  }
  { /* new vector with size NILs */
    var object newtable = allocate_vector(newsize);
    pushSTACK(newtable); /* save */
  }
  /* here we could protect against breaks.
     stack layout: tab, oldtable, free-conses, newtable.
     transfer symbols from oldtable to newtable:
     first process the symbols, that sit in lists
     (maybe Conses become free): */
  {
    var gcv_object_t* offset = 0; /* offset = sizeof(gcv_object_t)*index */
    var uintC count;
    dotimespC(count,oldsize, {
      var object oldentry = /* entry with number index in oldtable */
        *(gcv_object_t*)(pointerplus(&TheSvector(STACK_2)->data[0],
                                     (aint)offset));
      if (consp(oldentry)) /* this time process only non-empty symbol-lists */
        do {
          pushSTACK(Cdr(oldentry)); /* save rest-list */
          /* cons oldentry in front of free-conses */
          Cdr(oldentry) = STACK_2; STACK_2 = oldentry;
          /* enter symbol in the new table */
          newinsert(Car(oldentry),newsize);
          oldentry = popSTACK(); /* rest-list */
        } while (consp(oldentry));
      offset++;
    });
  }
  { /* then process symbols, that sit there collision-free: */
    var gcv_object_t* offset = 0; /* offset = sizeof(gcv_object_t)*index */
    var uintC count;
    dotimespC(count,oldsize, {
      var object oldentry = /* entry with number index in oldtable */
        *(gcv_object_t*)(pointerplus(&TheSvector(STACK_2)->data[0],
                                     (aint)offset));
      if (!listp(oldentry)) { /* this time process only symbols /= NIL */
        pushSTACK(oldentry); /* dummy, so that the stack is fine */
        newinsert(oldentry,newsize); /* enter into the new table */
        skipSTACK(1);
      }
      offset++;
    });
  }
  /* stack layout: tab, oldtable, free-conses, newtable. */
  { /* update tab: */
    var object newtable = popSTACK(); /* newtable */
    skipSTACK(2);
    symtab = popSTACK(); /* tab */
    Symtab_size(symtab) = size;
    Symtab_table(symtab) = newtable;
  }
  /* here, breaks could be allowed again. */
  return symtab;
}

/* UP: Searches a symbol with given print-name in the symbol-table.
 symtab_lookup(string,invert,symtab,&sym)
 > string: string
 > invert: whether to implicitly case-invert the string
 > symtab: symbol-table
 < result: true if found, false if not found.
 if found:
   < sym: the symbol from the symbol-table, that has the given printname */
local bool symtab_lookup (object string, bool invert, object symtab, object* sym_) {
  var uintL index = /* Index = Hashcode mod size */
    string_hashcode(string,invert) % (uintL)posfixnum_to_V(Symtab_size(symtab));
  /* entry in the table */
  var object entry = TheSvector(Symtab_table(symtab))->data[index];
  if (!listp(entry)) { /* entry is a single symbol */
    /* first string and printname of the found symbol are equal ? */
    if ((invert ? string_gleich_inverted : string_gleich)
        (string,Symbol_name(entry))) {
      if (sym_) { *sym_ = entry; }
      return true;
    } else {
      return false;
    }
  } else { /* entry is a symbol-list */
    while (consp(entry)) {
      /* first string and printname of the symbol are equal ? */
      if ((invert ? string_gleich_inverted : string_gleich)
          (string,Symbol_name(Car(entry))))
        goto found;
      entry = Cdr(entry);
    }
    return false; /* not found */
  found: /* found as CAR of entry */
    if (sym_) { *sym_ = Car(entry); }
    return true;
  }
}

/* UP: Searches a given symbol in the symbol-table.
 symtab_find(sym,symtab)
 > sym: symbol
 > symtab: symbol-table
 < result: true, if found */
local bool symtab_find (object sym, object symtab) {
  var uintL index = /* Index = Hashcode mod size */
    string_hashcode(Symbol_name(sym),false) % (uintL)posfixnum_to_V(Symtab_size(symtab));
  /* entry in the table */
  var object entry = TheSvector(Symtab_table(symtab))->data[index];
  if (!listp(entry)) { /* entry is a single symbol */
    /* sym and found symbol are equal ? */
    if (eq(sym,entry))
      return true;
    else
      return false;
  } else { /* entry is a symbol-list */
    if (nullp(memq(sym,entry))) return false; /* not found */
    else return true; /* found as CAR from entry */
  }
}

/* UP: Inserts a given symbol into the symbol-table (destructively).
 symtab_insert(sym,symtab)
 > sym: symbol
 > symtab: symbol-table
 < result: new symbol-table, EQ to the old one
 call only, if BREAK_SEM_2 is set
 can trigger GC */
local maygc object symtab_insert (object sym, object symtab) {
  { /* first test if reorganization is necessary: */
    var uintL size = posfixnum_to_V(Symtab_size(symtab));
    var uintL count = posfixnum_to_V(Symtab_count(symtab));
    /* if count>=2*size , the table must be reorganized: */
    if (count >= 2*size) {
      pushSTACK(sym); /* save symbol */
      symtab = rehash_symtab(symtab);
      sym = popSTACK();
    }
  }
  /* then insert the symbol: */
  var uintL index = /* Index = Hashcode mod size */
    string_hashcode(Symbol_name(sym),false) % (uintL)posfixnum_to_V(Symtab_size(symtab));
  /* entry in the table */
  var object entry = TheSvector(Symtab_table(symtab))->data[index];
  if (!nullp(entry) || nullp(sym)) {
    /* if entry=NIL and sym/=NIL, then simply enter sym.
       else, entry must be extended by cons-ing: */
    pushSTACK(symtab); /* save symtab */
    pushSTACK(sym); /* save Symbol */
    pushSTACK(entry); /* save entry */
    if (!listp(entry)) {
      /* if entry is not a list, replace with (cons entry NIL): */
      var object new_entry = allocate_cons();
      Car(new_entry) = STACK_0;
      STACK_0 = new_entry;
    }
    { /* and cons symbol in front of it: */
      var object new_entry = allocate_cons();
      Cdr(new_entry) = popSTACK(); /* enter entry resp. list as CDR */
      Car(new_entry) = popSTACK(); /* enter symbol as CAR */
      sym = new_entry; /* and then enter new_entry */
    }
    symtab = popSTACK();
  }
  TheSvector(Symtab_table(symtab))->data[index] = sym; /* enter new entry */
  Symtab_count(symtab) = fixnum_inc(Symtab_count(symtab),1); /* (incf count) */
  return symtab;
}

/* UP: Removes a symbol from a symbol-table.
 symtab_delete(sym,symtab)
 > sym: symbol
 > symtab: symboltable */
local void symtab_delete (object sym, object symtab) {
  var uintL index = /* Index = Hashcode mod size */
    string_hashcode(Symbol_name(sym),false) % (uintL)posfixnum_to_V(Symtab_size(symtab));
  var gcv_object_t* entryptr = &TheSvector(Symtab_table(symtab))->data[index];
  var object entry = *entryptr; /* entry in the table */
  if (!listp(entry)) { /* entry is a single symbol */
    /* sym and found symbol eq ? */
    if (!eq(sym,entry))
      goto notfound;
    /* replace entry with NIL: */
    *entryptr = NIL;
  } else { /* entry is a symbol-list */
    while (consp(entry)) {
      /* sym and symbol from entry eq ? */
      if (eq(sym,Car(entry)))
        goto found;
      entryptr = &Cdr(entry); entry = *entryptr;
    }
    goto notfound; /* not found */
  found: /* found as CAR of *entryptr = entry */
    /* -> discard a list-element: */
    *entryptr = Cdr(entry); /* replace entry with Cdr(entry) */
  }
  /* finally decrement the symbol-counter by 1: (decf count) */
  Symtab_count(symtab) = fixnum_inc(Symtab_count(symtab),-1);
  return;
 notfound:
  pushSTACK(unbound); /* PACKAGE-ERROR slot PACKAGE */
  pushSTACK(sym);
  fehler(package_error,
         GETTEXT("symbol ~S cannot be deleted from symbol table"));
}

/* lookup the STRING among the EXTernal (resp. INTernal) symbols of PACK */
#define package_lookup_ext(string,invert,pack,res_)                         \
  symtab_lookup(string,invert,ThePackage(pack)->pack_external_symbols,res_)
#define package_lookup_int(string,invert,pack,res_)                         \
  symtab_lookup(string,invert,ThePackage(pack)->pack_internal_symbols,res_)

/* Test whether there is an inherited symbol with the given name.
 inherited_lookup(string,invert,pack,&sym)
 Return true if string is found in (package-use-list pack).
 > string: a Lisp string object
 > invert: whether to implicitly case-invert the string
 > pack: is a Lisp package object
 The symbol found is returned in *SYM_ (if SYM_ is not NULL). */
local bool inherited_lookup (object string, bool invert, object pack, object* sym_) {
  var object packlistr = ThePackage(pack)->pack_use_list;
  while (consp(packlistr)) {
    var object usedpack = Car(packlistr);
    if (package_lookup_ext(string,invert,usedpack,sym_))
      return true;
    packlistr = Cdr(packlistr);
  }
  return false;
}

/* Check whether the symbol is inherited by the package.
 inherited_find(symbol,pack)
 SYMBOL is a Lisp symbol object
 PACK is a Lisp package object */
local bool inherited_find (object symbol, object pack) {
  var object list = ThePackage(pack)->pack_use_list;
  while (consp(list)) {
    if (symtab_find(symbol,ThePackage(Car(list))->pack_external_symbols))
      return true;
    list = Cdr(list);
  }
  return false;
}

/* data structure of package, see LISPBIBL.D.
 Components:
 pack_external_symbols   symbol-table of the externally present symbols
 pack_internal_symbols   symbol-table of the internally present symbols
 pack_shadowing_symbols  list of the shadowing-symbols
 pack_use_list           use-list, a list of packages
 pack_used_by_list       used-by-list, a list of packages
 pack_name               the name, an immutable simple-string
 pack_nicknames          the nicknames, a list of immutable simple-strings
 pack_docstring          the documentation string or NIL

 consistency rules:
 1. All packages are listed in ALL_PACKAGES exactly once.
 2. The union over ALL_PACKAGES of { name } U nicknames is disjoint.
 3. for any two packages p,q:
    p in use_list(q) <==> q in used_by_list(q)
 4. p is a Package.
    accessible(p) = ISymbols(p) U ESymbols(p) U
                    U { ESymbols(q) | q in use_list(p) }
 5. For each Package p
    shadowing_symbols(p) is a subset of ISymbols(p) U ESymbols(p)
    and therefore also      a subset of accessible(p).
 6. s is a string, p is a package.
    If more than one element of accessible(p) has print name = s, then
    exactly one of these symbols is in shadowing_symbols(p).
 7. s is a string, p is a package.
    At most one symbol with the print name = s
    is in ISymbols(p) U ESymbols(p).
 8. If s is a symbol with the Home Package p /= NIL,
    then s is in ISymbols(p) U ESymbols(p). */

/* UP: make sure pack_shortest_name is indeed the shortest */
local void ensure_pack_shortest_name (object pack) {
  var object shortest_name = ThePackage(pack)->pack_name;
  var uintL shortest_len = Sstring_length(shortest_name);
  var object nick_list = ThePackage(pack)->pack_nicknames;
  while (consp(nick_list)) {
    var object nick = Car(nick_list); nick_list = Cdr(nick_list);
    var uintL nick_len = Sstring_length(nick);
    if (nick_len < shortest_len) {
      shortest_len = nick_len;
      shortest_name = nick;
    }
  }
  ThePackage(pack)->pack_shortest_name = shortest_name;
}

/* UP: Creates a new package, without testing for name-conflicts.
 make_package(name,nicknames,case_sensitive_p)
 > name: name (an immutable simple-string)
 > nicknames: nicknames (a list of immutable simple-strings)
 > case_sensitive_p: flag, if case-sensitive
 > case_inverted_p: flag, if case-inverted
 < result: new package
 can trigger GC */
local maygc object make_package (object name, object nicknames,
                                 bool case_sensitive_p, bool case_inverted_p) {
  set_break_sem_2();
  pushSTACK(nicknames); pushSTACK(name); /* save nicknames and names */
  /* create table for external symbols: */
  { var object symtab = make_symtab(11); pushSTACK(symtab); }
  /* create table for internal symbols: */
  { var object symtab = make_symtab(63); pushSTACK(symtab); }
  /* create new package: */
  var object pack = allocate_package();
  /* and fill: */
  if (case_sensitive_p) { mark_pack_casesensitive(pack); }
  if (case_inverted_p) { mark_pack_caseinverted(pack); }
  ThePackage(pack)->pack_internal_symbols = popSTACK();
  ThePackage(pack)->pack_external_symbols = popSTACK();
  ThePackage(pack)->pack_shadowing_symbols = NIL;
  ThePackage(pack)->pack_use_list = NIL;
  ThePackage(pack)->pack_used_by_list = NIL;
  ThePackage(pack)->pack_name = popSTACK();
  ThePackage(pack)->pack_nicknames = popSTACK();
  ThePackage(pack)->pack_docstring = NIL;
  ensure_pack_shortest_name(pack);
  /* and insert in ALL_PACKAGES: */
  pushSTACK(pack);
  var object new_cons = allocate_cons();
  pack = popSTACK();
  Car(new_cons) = pack; Cdr(new_cons) = O(all_packages);
  O(all_packages) = new_cons;
  /* finished: */
  clr_break_sem_2();
  return pack;
}

/* UP: Searches a symbol of given printname in the shadowing-list
 of a package.
 shadowing_lookup(string,invert,pack,&sym)
 > string: string
 > invert: whether to implicitly case-invert the string
 > pack: package
 < result: true, if found.
 < sym: the symbol from the shadowing-list, that has the given printname
        (if found) */
local bool shadowing_lookup (object string, bool invert, object pack, object* sym_) {
  var object list = ThePackage(pack)->pack_shadowing_symbols;
  /* traverse shadowing-list: */
  while (consp(list)) {
    if ((invert ? string_gleich_inverted : string_gleich)
        (string,Symbol_name(Car(list))))
      goto found;
    list = Cdr(list);
  }
  return false; /* not found */
 found: /* found */
  if (sym_) { *sym_ = Car(list); }
  return true;
}

/* UP: Searches a given symbol in the shadowing-list of a package.
 shadowing_find(sym,pack)
 > sym: symbol
 > pack: package
 < result: true if found. */
#define shadowing_find(s,p) (!nullp(memq(s,ThePackage(p)->pack_shadowing_symbols)))

/* UP: Adds a symbol to the shadowing-list of a package, that does not yet
 contain a symbol of the same name.
 shadowing_insert(&sym,&pack)
 > sym: symbol (in STACK)
 > pack: package (in STACK)
 < sym: symbol, EQ to the old one
 < pack: package, EQ to the old one
 can trigger GC */
local maygc void shadowing_insert (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  /* insert a new cons with symbol as CAR in front of the shadowing-symbols: */
  var object new_cons = allocate_cons();
  var object pack = *pack_;
  Car(new_cons) = *sym_;
  Cdr(new_cons) = ThePackage(pack)->pack_shadowing_symbols;
  ThePackage(pack)->pack_shadowing_symbols = new_cons;
}

/* UP: Removes a symbol of given name from the shadowing-list
 of a package.
 shadowing_delete(string,invert,pack)
 > string: string
 > invert: whether to implicitly case-invert the string
 > pack: package */
local void shadowing_delete (object string, bool invert, object pack) {
  var gcv_object_t* listptr = &ThePackage(pack)->pack_shadowing_symbols;
  var object list = *listptr;
  /* list = *listptr traverses the shadowing-list */
  while (consp(list)) {
    if ((invert ? string_gleich_inverted : string_gleich)
        (string,Symbol_name(Car(list))))
      goto found;
    listptr = &Cdr(list); list = *listptr;
  }
  /* no symbol with this name found, done. */
  return;
 found:
  /* equality: remove. After that we are done, because there can be only
     one symbol of the same printname in the shadowing-list. */
  *listptr = Cdr(list); /* replace list with Cdr(list) */
  return;
}

/* UP: Tests, if a symbol in a package is accessible and is not
 shadowed by another symbol of the same name.
 accessiblep(sym,pack)
 > sym: symbol
 > pack: package
 < result: true if sym is accessible in pack and is not shadowed,
             else false */
global bool accessiblep (object sym, object pack) {
  /* method:
     First, search a symbol of equal name in the shadowing-list;
     if not found, search the symbol among the present ones and
     then among the inherited symbols.
     Other possible method (not realized here):
     If the home-package of sym is equal to pack, sym is present in pack,
     done. Else search a present symbol of equal name.
     sym found -> finished.
     Found another one -> sym is not in the shadowing-list and
     thus not visible.
     none found -> search sym among the inherited symbols. */
  var object shadowingsym;
  /* First, search in the shadowing-list of pack: */
  if (shadowing_lookup(Symbol_name(sym),false,pack,&shadowingsym)) {
    /* shadowingsym = symbol, found in the shadowing-list */
    return (eq(shadowingsym,sym)); /* compare with sym */
  } else { /* no symbol of equal name in the shadowing-list */
    /* Search among the internal symbols: */
    if (symtab_find(sym,ThePackage(pack)->pack_internal_symbols))
      return true;
    /* Search among the external symbols: */
    if (symtab_find(sym,ThePackage(pack)->pack_external_symbols))
      return true;
    /* Search among the external symbols of the packages from the use-list: */
    if (inherited_find(sym,pack))
      return true;
    return false;
  }
}

/* UP: tests, if a symbol is accessible in a package as
 external symbol.
 externalp(sym,pack)
 > sym: symbol
 > pack: package
 < result:
     true if sym is accessible in pack as external symbol,
     (in this case, sym is not shadowed, because a symbol,
      possibly shadowing sym, should be listed in shadowing-symbols(pack),
      according to the consistency-rules 5 and 7 identical with sym),
     else false */
global bool externalp (object sym, object pack) {
  return symtab_find(sym,ThePackage(pack)->pack_external_symbols);
}

/* UP: locates an external symbol with a given printname in a package.
 find_external_symbol(string,invert,pack,&sym)
 > string: string
 > invert: whether to implicitly case-invert the string
 > pack: package
 < result: true, if an external symbol with that printname has been found in pack.
 < sym: this symbol, if found. */
global bool find_external_symbol (object string, bool invert, object pack, object* sym_) {
  return package_lookup_ext(string,invert,pack,sym_);
}

/* UP: searches a package of given name or nickname
 find_package(string)
 > string: string
 < result: package of this name or NIL */
global object find_package (object string) {
  var object packlistr = O(all_packages); /* traverse package-list */
  var object pack;
  while (consp(packlistr)) {
    pack = Car(packlistr); /* Package to be tested */
    /* test name: */
    if (string_gleich(string,ThePackage(pack)->pack_name))
      goto found;
    { /* test nickname: */
      /* traverse nickname-list */
      var object nicknamelistr = ThePackage(pack)->pack_nicknames;
      while (consp(nicknamelistr)) {
        if (string_gleich(string,Car(nicknamelistr)))
          goto found;
        nicknamelistr = Cdr(nicknamelistr);
      }
    }
    packlistr = Cdr(packlistr); /* next package */
  }
  /* not found */
  return NIL;
 found: /* found */
  return pack;
}

/* UP: Searches a symbol of given printname in a package.
 find_symbol(string,invert,pack,&sym)
 > string: string
 > invert: whether to implicitly case-invert the string
 > pack: package
 < sym: symbol, if found; else NIL
 < result:   0, if not found
             1, if available as external symbol
             2, if inherited via use-list
             3, if available as internal symbol
         + (-4, if available in the shadowing-list) */
local sintBWL find_symbol (object string, bool invert, object pack, object* sym_) {
  /* First search in the shadowing-list of pack: */
  if (shadowing_lookup(string,invert,pack,sym_)) {
    /* *sym_ = symbol, found in the shadowing-list */
    /* Search for it among the internal symbols: */
    if (symtab_find(*sym_,ThePackage(pack)->pack_internal_symbols))
      return 3-4; /* found among the internal symbols */
    /* Search it among the external symbols: */
    if (symtab_find(*sym_,ThePackage(pack)->pack_external_symbols))
      return 1-4; /* found among the external symbols */
    /* contradiction to consistency rule 5. */
    pushSTACK(*sym_); pushSTACK(pack);
    fehler(serious_condition,GETTEXT("~S inconsistent: symbol ~S is a shadowing symbol but not present"));
  } else { /* symbol not yet found */
    /* search among the internal symbols: */
    if (package_lookup_int(string,invert,pack,sym_))
      return 3; /* found among the internal symbols */
    /* search among the external symbols: */
    if (package_lookup_ext(string,invert,pack,sym_))
      return 1; /* found among the external symbols */
    /* search among the external packages from the use-list: */
    if (inherited_lookup(string,invert,pack,sym_))
      return 2; /* found among the inherited symbols */
    /* not found */
    *sym_ = NIL; return 0;
  }
}
/* Actually, one has to search in the shadowing-list only after
   one has searched among the present symbols, because the symbol in the
   shadowing-list is already present (consistency rule 5). */

/* raise a continuable error when func(obj) was called on a locked package pack
 continue means "Ignore the lock and proceed"
 can trigger GC */
local maygc void cerror_package_locked (object func, object pack, object obj) {
  pushSTACK(NIL);              /* 7 continue-format-string */
  pushSTACK(S(package_error)); /* 6 error type */
  pushSTACK(S(Kpackage));      /* 5 :PACKAGE */
  if (consp(pack)) pushSTACK(Car(pack)); /* from check-redefinition */
  else pushSTACK(pack);        /* 4 PACKAGE-ERROR slot PACKAGE */
  pushSTACK(NIL);              /* 3 error-format-string */
  pushSTACK(func);             /* 2 */
  pushSTACK(obj);              /* 1 */
  pushSTACK(pack);             /* 0 */
  /* CLSTEXT "can trigger GC", so it cannot be called until
     all the arguments have been already pushed on the STACK */
  STACK_7 = CLSTEXT("Ignore the lock and proceed"); /* continue-format-string */
  STACK_3 = CLSTEXT("~A(~S): ~S is locked"); /* error-format-string */
  funcall(L(cerror_of_type),8);
}
/* check the package lock */
#define check_pack_lock(func,pack,obj)                          \
  if (pack_locked_p(pack)) cerror_package_locked(func,pack,obj)
#define safe_check_pack_lock(func,pack,obj)                     \
  do { pushSTACK(pack); pushSTACK(obj); /* save */              \
       check_pack_lock(func, STACK_1 /*pack*/,STACK_0 /*obj*/); \
       obj = popSTACK(); pack = popSTACK(); /* restore */       \
  } while(0)

/* UP: Inserts a symbol into a package, that hasn't yet a a symbol
 of the same name. Does not check for conflicts.
 make_present(sym,pack);
 > sym: symbol
 > pack: package
 only call, if BREAK_SEM_2 is set
 can trigger GC */
local maygc void make_present (object sym, object pack) {
  if (!eq(pack,O(keyword_package))) {
    if (nullp(Symbol_package(sym)))
      Symbol_package(sym) = pack;
    /* Insert symbol into the internal symbols: */
    symtab_insert(sym,ThePackage(pack)->pack_internal_symbols);
  } else {
    if (symmacro_var_p(TheSymbol(sym))) {
      /* HyperSpec/Body/mac_define-symbol-macro.html says that making a
         global symbol-macro special is undefined; likewise for constants. */
      pushSTACK(pack); pushSTACK(sym); pushSTACK(TheSubr(subr_self)->name);
      fehler(program_error,
             GETTEXT("~S: Importing ~S into ~S would turn it into a constant, but it is already a global symbol-macro."));
    }
    if (nullp(Symbol_package(sym)))
      Symbol_package(sym) = pack;
    /* Modify symbol and insert into the external symbols: */
    Symbol_value(sym) = sym; /* sym gets itself as value */
    /* Mark as constant: */
    set_const_flag(TheSymbol(sym));
    symtab_insert(sym,ThePackage(pack)->pack_external_symbols);
  }
}

/* UP: Interns a symbol with a given printname in a package.
 intern(string,invert,pack,&sym)
 > string: string
 > invert: whether to implicitly case-invert the string
 > pack: package
 < sym: symbol
 < result: 0, if not found, but newly created
           1, if available as external symbol
           2, if inherited via use-list
           3, if available as internal symbol
 can trigger GC */
global maygc uintBWL intern (object string, bool invert, object pack, object* sym_) {
  {
    var sintBWL ergebnis = find_symbol(string,invert,pack,sym_); /* search */
    if (!(ergebnis==0))
      return ergebnis & 3; /* found -> finished */
  }
  pushSTACK(pack); /* save package */
  if (pack_locked_p(pack)) {
    /* when STRING comes from READ, it points to a re-usable buffer
       that will be overwritten during the CERROR i/o
       therefore we must copy and save it */
    pushSTACK(coerce_ss(string));
    cerror_package_locked(S(intern),STACK_1/*pack*/,STACK_0/*string*/);
    string = popSTACK();
  }
  if (invert)
    string = string_invertcase(string);
  string = coerce_imm_ss(string); /* string --> immutable simple-string */
  var object sym = make_symbol(string); /* (make-symbol string) */
  pack = popSTACK();
  /* enter this new symbol into the package: */
  set_break_sem_2(); /* protect against breaks */
  Symbol_package(sym) = pack; /* enter home-package */
  pushSTACK(sym); /* save symbol */
  make_present(sym,pack); /* intern into this package */
  *sym_ = popSTACK();
  clr_break_sem_2(); /* allow breaks */
  return 0;
}

/* UP: Interns a symbol of given printname into the keyword-package.
 intern_keyword(string)
 > string: string
 < result: symbol, a keyword
 can trigger GC */
global maygc object intern_keyword (object string) {
  var object sym;
  intern(string,false,O(keyword_package),&sym);
  return sym;
}

/* UP: lookup the string among the internal and, if not found,
 external symbols of the package PACK
 tab, if supplied, is the assignment that will set the table in which the
 STRINNG was found */
#define package_lookup(string,invert,pack,res_,tab)  \
  (symtab_lookup(string,invert,tab ThePackage(pack)->pack_internal_symbols,res_) || \
   symtab_lookup(string,invert,tab ThePackage(pack)->pack_external_symbols,res_))

/* UP: Imports a symbol into a package and turns it into a shadowing-symbol.
 Possibly another present symbol in this package
 of the same name is uninterned.
 shadowing_import(&sym,&pack);
 > sym: symbol (in STACK)
 > pack: package (in STACK)
 < sym: symbol, EQ to the old one
 < pack: package, EQ to the old one
 can trigger GC */
local maygc void shadowing_import (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  check_pack_lock(S(shadowing_import),*pack_,*sym_);
  set_break_sem_2(); /* protect against breaks */
  {
    var object sym = *sym_;
    var object pack = *pack_;
    /* Searches an internal or external symbol of the same name: */
    var object othersym;
    var object tab_found;
    var object string = Symbol_name(sym);
    pushSTACK(string); /* save string */
    if (package_lookup(string,false,pack,&othersym,tab_found=)) {
      /* a symbol othersym of the same name was
         already present in the package */
      if (!eq(othersym,sym)) { /* was it the to be imported symbol itself? */
        /* no -> have to take othersym away from the internal resp. */
        /* from the external symbols: */
        symtab_delete(othersym,tab_found);
        /* Was this symbol taken away from its home-package,
           its home-package must be set to NIL: */
        if (eq(Symbol_package(othersym),pack))
          Symbol_package(othersym) = NIL;
        /* symbol sym must be added to the package pack. */
        make_present(sym,pack);
      }
    } else { /* symbol sym must be added to the package pack. */
      make_present(sym,pack);
    }
  }
  /* symbol must be added to the shadowing-list of the package. */
  shadowing_delete(popSTACK(),false,*pack_); /* remove string from */
  /* the shadowing-list */
  shadowing_insert(sym_,pack_); /* add symbol to the shadowing-list */
  clr_break_sem_2(); /* allow breaks */
}

/* UP: Shadows in a package all symbols accessible from other packages
 of give name by one symbol present in this package
 of the same name.
 shadow(&sym,invert,&pack)
 > sym: symbol or string (in STACK)
 > invert: whether to implicitly case-invert the string
 > pack: package (in STACK)
 < pack: package, EQ to the old
 can trigger GC */
local maygc void do_shadow (const gcv_object_t* sym_, bool invert, const gcv_object_t* pack_) {
  check_pack_lock(S(shadow),*pack_,*sym_);
  set_break_sem_2(); /* protect against breaks */
  /* Search an internal or external symbol of the same name: */
  var object string = /* only the name of the symbol counts. */
    test_stringsymchar_arg(*sym_,invert);
  var object pack = *pack_;
  pushSTACK(NIL); /* make room for othersym */
  pushSTACK(string); /* save string */
  var object othersym;
  if (package_lookup(string,invert,pack,&othersym,)) {
    STACK_1 = othersym;
  } else {
    /* not found -> create new symbol of the same name: */
    if (invert)
      string = string_invertcase(string);
    string = coerce_imm_ss(string); /* string --> immutable simple-string */
    var object othersym = make_symbol(string); /* new symbol */
    STACK_1 = othersym;
    make_present(othersym,*pack_); /* enter into the package */
    /* home-package of the new symbols is pack */
    Symbol_package(STACK_1) = *pack_;
  }
  /* stack-layout: othersym, string
     In the package, now symbol othersym of the same name is present.
     remove string from the shadowing-list */
  shadowing_delete(popSTACK(),invert,*pack_);
  /* therefore add othersym to the shadowing-list */
  shadowing_insert(&STACK_0,pack_);
  skipSTACK(1); /* forget othersym */
  clr_break_sem_2(); /* allow breaks */
}
local maygc void shadow (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  do_shadow(sym_,false,pack_);
}
local maygc void cs_shadow (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  do_shadow(sym_,true,pack_);
}

/* UP: Removes a symbol from the set of present symbols of a package
 and does conflict resolution if it was in the shadowing-list
 of this package and a name conflict arises.
 unintern(&sym,&pack)
 > sym: symbol (in STACK)
 > pack: package (in STACK)
 < sym: symbol, EQ to the old
 < pack: package, EQ to the old
 < result: T if found and deleted, NIL if nothing has been done.
 can trigger GC */
local maygc object unintern (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  check_pack_lock(S(unintern),*pack_,*sym_);
  var object sym = *sym_;
  var object pack = *pack_;
  var object symtab;
  /* search sym among the internal and the external symbols: */
  if (symtab_find(sym,symtab=ThePackage(pack)->pack_internal_symbols)
      || symtab_find(sym,symtab=ThePackage(pack)->pack_external_symbols)) {
    /* found symbol sym in the table symtab */
    if (shadowing_find(sym,pack)) { /* search in the shadowing-list */
      /* possible conflict -> build up selection-list: */
      pushSTACK(symtab); /* save symboltable */
      pushSTACK(NIL); /* start option-list */
      pushSTACK(ThePackage(pack)->pack_use_list); /* traverse use-list */
      /* stack-layout: symboltable, OL, use-list-rest */
      while (mconsp(STACK_0)) {
        var object othersym;
        pack = Car(STACK_0); /* package from the use-list */
        STACK_0 = Cdr(STACK_0);
        /* search inherited symbol of the same name: */
        if (package_lookup_ext(Symbol_name(*sym_),false,pack,&othersym)) {
          /* check that othersym is not in the option-list yet */
          var object temp = STACK_1;
          while (mconsp(temp)) {
            if (eq(Cdr(Cdr(Car(temp))),othersym))
              goto next_package;
            temp = Cdr(temp);
          }
          /* othersym is a symbol of the same name, inherited from pack */
          pushSTACK(temp=ThePackage(pack)->pack_name); /* name of pack */
          pushSTACK(othersym); /* symbol */
          pushSTACK(NIL);
          pushSTACK(NIL); /* "symbol ~A from ~A will become a shadowing symbol" */
          pushSTACK(Symbol_name(othersym)); /* symbolname */
          pushSTACK(pack); /* package */
          STACK_2 = CLSTEXT("symbol ~A from ~A will become a shadowing symbol");
          /* (FORMAT NIL "..." symbolname packagename) */
          funcall(S(format),4);
          temp = value1;
          pushSTACK(temp); /* total-string */
          temp = allocate_cons();
          Car(temp) = popSTACK();
          Cdr(temp) = popSTACK();
          pushSTACK(temp); /* (cons total-string othersym) */
          temp = allocate_cons();
          Cdr(temp) = popSTACK();
          Car(temp) = popSTACK();
          /* temp = (list packagename total-string othersym) */
          /* STACK is correct, again */
          /* push to the option-list: */
          pushSTACK(temp);
          temp = allocate_cons();
          Car(temp) = popSTACK(); Cdr(temp) = STACK_1;
          STACK_1 = temp;
        }
       next_package:;
      }
      skipSTACK(1);
      /* option-list build-up finished.
         stack-layout: symboltable, OL
         if (length OL) >= 2, there's a conflict: */
      if (mconsp(STACK_0) && mconsp(Cdr(STACK_0))) {
        /* raise a correctable error, options is STACK_0 already */
        pushSTACK(*pack_); /* PACKAGE-ERROR slot PACKAGE */
        pushSTACK(*pack_); /* package */
        pushSTACK(*sym_); /* symbol */
        correctable_error(package_error,GETTEXT("Uninterning ~S from ~S uncovers a name conflict.\nYou may choose the symbol in favour of which to resolve the conflict."));
        pushSTACK(value1);
      } else
        STACK_0 = NIL;
      /* STACK_0 is the selection (NIL if no conflict arises) */
      /* stack-layout: symboltable, selection */
      set_break_sem_3();
      {
        var object sym = *sym_;
        var object pack = *pack_;
        /* remove symbol from symboltable: */
        symtab_delete(sym,STACK_1);
        /* if it was removed from its home-package,
           set the home-package to NIL: */
        if (eq(Symbol_package(sym),pack))
          Symbol_package(sym) = NIL;
        /* discard symbol from shadowing-list: */
        shadowing_delete(Symbol_name(sym),false,pack);
      }
      if (!nullp(STACK_0))
        /* in case of a conflict: import selected symbol: */
        shadowing_import(&STACK_0,pack_);
      skipSTACK(2); /* forget symboltable & selection */
      clr_break_sem_3();
      return T; /* that's it */
    } else { /* no conflict */
      set_break_sem_2();
      symtab_delete(sym,symtab); /* delete symbol */
      if (eq(Symbol_package(sym),pack))
        Symbol_package(sym) = NIL; /* maybe set home-package to NIL */
      clr_break_sem_2();
      return T;
    }
  } else /* not found */
    return NIL;
}

/* UP: raise a continuable error and query the user about how to proceed
 return true when an abort was requested
 dialog_type == 0 or 1 or 2
 can trigger GC */
local maygc bool query_intern_conflict (object pack, object sym, object other,
                                        int dialog_type) {
  pushSTACK(NIL); /* place for OPTIONS */
  pushSTACK(pack); /* PACKAGE-ERROR slot PACKAGE */
  pushSTACK(other); pushSTACK(pack); pushSTACK(sym);
  switch (dialog_type) {        /* fill OPTIONS */
    case 0: /* conflict */
      STACK_4=CLOTEXT("((IMPORT \"import it and unintern the other symbol\" . T)"
                      " (IGNORE \"do not import it, leave undone\" . NIL))");
      break;
    case 1: /* conflict & shadowing */
      STACK_4=CLOTEXT("((IMPORT \"import it, unintern one other symbol and shadow the other symbols\" . T)"
                      " (IGNORE \"do not import it, leave undone\" . NIL))");
      break;
    case 2: /* shadowing */
      STACK_4=CLOTEXT("((IMPORT \"import it and shadow the other symbol\" . T)"
                      " (IGNORE \"do nothing\" . NIL))");
      break;
    default: NOTREACHED;
  }
  correctable_error(package_error,(dialog_type == 1
                                   ? GETTEXT("Importing ~S into ~S produces a name conflict with ~S and other symbols.")
                                   : GETTEXT("Importing ~S into ~S produces a name conflict with ~S.")));
  return nullp(value1);
}

/* UP: Imports a symbol into a package and does conflict resolution
 in case, that a name conflict arises either with a symbol
 inherited from anotherpackage or with an already present symbol
 in this package of the same name.
 import(&sym,&pack);
 > sym: symbol (in STACK)
 > pack: package (in STACK)
 < pack: package, EQ to the old
 can trigger GC */
global maygc void import (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  var object sym = *sym_;
  var object pack = *pack_;
  var object string = Symbol_name(sym);
  var object othersym;
  var object othersymtab;
  /* search symbol of the same name among the internal
     and the external symbols: */
  if (package_lookup(string,false,pack,&othersym,othersymtab=)) {
    /* othersym = symbol of the same name, found in othersymtab */
    if (eq(othersym,sym)) /* the same symbol -> nothing to do */
      return;
    /* not the same symbol was present -> must throw out othersym and
       insert the given symbol sym for it.
       determine beforehand, if there are additional inherited
       symbols there, and then raise Continuable Error. */
    pushSTACK(string);
    pushSTACK(othersym);
    pushSTACK(othersymtab);
    /* first calculate inherited-flag: */
    var bool inheritedp = inherited_lookup(string,false,pack,NULL);
    /* stack-layout: symbol-name, othersym, othersymtab. */
    /* raise Continuable Error: */
    if (query_intern_conflict(*pack_,*sym_,othersym,inheritedp ? 1 : 0)) {
      skipSTACK(3); return; /* yes -> do not import, finished */
    }
    /* import: */
    set_break_sem_2();
    pack = *pack_;
    { /* remove othersym from pack: */
      var object othersym = STACK_1;
      symtab_delete(othersym,STACK_0); /* remove othersym from othersymtab */
      if (eq(Symbol_package(othersym),pack))
        Symbol_package(othersym) = NIL; /* maybe home-package := NIL */
    }
    /* insert sym in pack: */
    make_present(*sym_,pack);
    /* remove symbols of the same name from the shadowing-list of pack: */
    shadowing_delete(STACK_2,false,*pack_);
    /* if inherited-flag, turn sym in pack into a shadowing-symbol: */
    if (inheritedp)
      shadowing_insert(sym_,pack_);
    clr_break_sem_2();
    skipSTACK(3); return;
  } else {
    /* no symbol of the same name was present.
       Search a symbol of the same name, that is inherited (there is
       at most one, according to the consistency rules 6 and 5): */
    var object otherusedsym;
    if (!inherited_lookup(string,false,pack,&otherusedsym)
        || eq(otherusedsym,sym)) {
      /* insert sym simply in pack: */
      set_break_sem_2();
      make_present(sym,pack);
      clr_break_sem_2();
    } else {
      /* no -> raise Continuable Error and query user: */
      if (query_intern_conflict(pack,sym,otherusedsym,2))
        return; /* yes -> do not import, finished */
      /* import: */
      set_break_sem_2();
      /* insert sym in pack: */
      make_present(*sym_,*pack_);
      /* turn sym in pack into a shadowing-symbol: */
      shadowing_insert(sym_,pack_);
      clr_break_sem_2();
    }
  }
}

/* UP: Sets a symbol back from external to internal status in
 einer package.
 unexport(&sym,&pack);
 > sym: symbol (in STACK)
 > pack: package (in STACK)
 < pack: package, EQ to the old
 can trigger GC */
local maygc void unexport (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  check_pack_lock(S(unexport),*pack_,*sym_);
  var object sym = *sym_;
  var object pack = *pack_;
  var object symtab;
  if (symtab_find(sym,symtab=ThePackage(pack)->pack_external_symbols)) {
    /* sym is external in pack */
    if (eq(pack,O(keyword_package))) { /* test for keyword-package */
      pushSTACK(pack); /* PACKAGE-ERROR slot PACKAGE */
      pushSTACK(pack);
      fehler(package_error,GETTEXT("UNEXPORT in ~S is illegal"));
    }
    set_break_sem_2();
    symtab_delete(sym,symtab); /* remove sym from the external symbols */
    /* therefor, insert it into the internal symbols */
    symtab_insert(sym,ThePackage(pack)->pack_internal_symbols);
    clr_break_sem_2();
  } else {
    /* Search, if the symbol is accessible at all. */
    /* Search among the internal symbols: */
    if (symtab_find(sym,ThePackage(pack)->pack_internal_symbols))
      return;
    /* Search among the external symbols of the packages from the use-list: */
    if (inherited_find(sym,pack))
      return;
    /* not found among the accessible symbols */
    pushSTACK(pack); /* PACKAGE-ERROR slot PACKAGE */
    pushSTACK(pack); pushSTACK(sym);
    fehler(package_error,
           GETTEXT("UNEXPORT works only on accessible symbols, not on ~S in ~S"));
  }
}

/* UP: Sets a present symbol into external status.
 make_external(sym,pack);
 > sym: symbol
 > pack: package, in which the symbol is present
 can trigger GC */
local maygc void make_external (object sym, object pack) {
  if (symtab_find(sym,ThePackage(pack)->pack_external_symbols))
    return; /* symbol already external -> nothing to do */
  set_break_sem_2();
  /* remove sym from the internal symbols */
  symtab_delete(sym,ThePackage(pack)->pack_internal_symbols);
  /* therefor, insert it into the external symbols */
  symtab_insert(sym,ThePackage(pack)->pack_external_symbols);
  clr_break_sem_2();
}

/* UP: Exports a symbol from a package
 export(&sym,&pack);
 > sym: symbol (in STACK)
 > pack: package (in STACK)
 < sym: symbol, EQ to the old
 < pack: package, EQ to the old
 can trigger GC */
global maygc void export (const gcv_object_t* sym_, const gcv_object_t* pack_) {
  check_pack_lock(S(export),*pack_,*sym_);
  var object sym = *sym_;
  var object pack = *pack_;
  /* search sym among the external symbols of pack: */
  if (symtab_find(sym,ThePackage(pack)->pack_external_symbols))
    return; /* found -> finished */
  var bool import_it = false;
  /* import_it = flag, if symbol has to be imported first. */
  /* search sym among the internal symbols of pack: */
  if (!(symtab_find(sym,ThePackage(pack)->pack_internal_symbols))) {
    /* symbol sym is not present in package pack */
    import_it = true;
    /* Search, if it is at least accessible: */
    if (inherited_find(sym,pack))
      goto found;
    /* symbol sym is not even accessible in the package pack ==>
       raise correctable error: */
    pushSTACK(NIL); /* place for OPTIONS */
    pushSTACK(pack); /* PACKAGE-ERROR slot PACKAGE */
    /* "symbol ~S has to be imported in ~S before being exported" */
    pushSTACK(pack); pushSTACK(sym); pushSTACK(S(export));
    STACK_4 = CLOTEXT("((IMPORT \"import the symbol first\" . T)"
                      " (IGNORE \"do nothing, do not export the symbol\" . NIL))");
    correctable_error(package_error,GETTEXT("~S: Symbol ~S should be imported into ~S before being exported."));
    if (nullp(value1)) /* NIL-option selected? */
      return; /* yes -> do not export, finished */
   found: ;
  }
  /* Test for name-conflict: */
  pushSTACK(NIL); /* conflict-resolver:=NIL */
  /* stack-layout: conflict-resolver (a list of pairs (sym . pack),
                   for which shadowing_import has to be applied).
  used-by-list is searched */
  pushSTACK(ThePackage(*pack_)->pack_used_by_list);
  while (mconsp(STACK_0)) {
    var object usingpack = Car(STACK_0); /* USE-ing package */
    STACK_0 = Cdr(STACK_0);
    var object othersym;
    if (find_symbol(Symbol_name(*sym_),false,usingpack,&othersym) > 0)
      /* othersym is a symbol of the same name in usingpack */
      if (!eq(othersym,*sym_)) {
        var gcv_object_t *othersym_, *usingpack_;
        /* it is not sym itself -> there is a conflict */
        pushSTACK(othersym); othersym_ = &STACK_0;
        pushSTACK(usingpack); usingpack_ = &STACK_0;
        /* stack-layout: conflict-resolver, used-by-list-rest,
                         other symbol, USE-ing package. */
        pushSTACK(NIL);         /* space for OPTIONS */
        pushSTACK(*pack_);      /* PACKAGE-ERROR slot PACKAGE */
        pushSTACK(usingpack);   /* USE-ing package */
        pushSTACK(usingpack);   /* USE-ing package */
        pushSTACK(othersym);    /* other symbol */
        pushSTACK(*pack_);      /* package */
        pushSTACK(*sym_);       /* symbol */
        { /* construct options-list: */
          var object temp;
          pushSTACK(ThePackage(*pack_)->pack_name); /* package name */
          pushSTACK(CLSTEXT("the symbol to export, "));
          pushSTACK(*sym_); /* symbol */
          funcall(L(prin1_to_string),1); /* (prin1-to-string Symbol) */
          pushSTACK(value1);
          /* (string-concat "The new symbol " (prin1-to-string Symbol)) */
          temp = string_concat(2);
          pushSTACK(temp);
          temp = listof(2); /* (list* symbol (string-concat ...) 'T) */
          Cdr(Cdr(temp)) = T;
          pushSTACK(temp);
          pushSTACK(ThePackage(*usingpack_)->pack_name); /* USE-ing pack */
          pushSTACK(CLSTEXT("the old symbol, "));
          pushSTACK(*othersym_); /* other symbol */
          /* (prin1-to-string anderesSymbol) */
          funcall(L(prin1_to_string),1);
          pushSTACK(value1);
          /* (string-concat "The old symbol " (prin1-to-string old-symbol)) */
          temp = string_concat(2);
          pushSTACK(temp);
          temp = listof(2); /* (list* other-symbol (string-concat ...) 'NIL) */
          /* Cdr(Cdr(temp)) = NIL; not needed */
          pushSTACK(temp);
          temp = listof(2); /* (list (list s1 ... 'T) (list s2 ... 'NIL)) */
          STACK_6 = temp; /* options */
        }
        correctable_error(package_error,GETTEXT("Exporting ~S from ~S produces a name conflict with ~S from ~S.\nYou may choose which symbol should be accessible in ~S."));
        pushSTACK(nullp(value1)?STACK_1/*other symbol*/:*sym_);/*solvingsym*/
        { /* extend conflict-resolver with (solvingsym . usingpack) : */
          var object new_cons = allocate_cons();
          Car(new_cons) = popSTACK(); /* solvingsym */
          Cdr(new_cons) = popSTACK(); /* usingpack */
          /* new_cons = (cons solvingsym usingpack) */
          /* cons in front of conflict-resolver: */
          STACK_0 = new_cons;
          new_cons = allocate_cons();
          Car(new_cons) = popSTACK(); /* (solvingsym . usingpack) */
          Cdr(new_cons) = STACK_1;
          STACK_1 = new_cons;
        }
        /* stack-layout: conflict-resolver, used-by-list-rest. */
      }
  }
  skipSTACK(1);
  /* stack-layout: conflict-resolver. */
  /* Now maybe import symbol sym: */
  if (import_it) {
    /* import sym in pack: */
    import(sym_,pack_);
    /* This importing can be aborted by CERROR.
       An abort is not dangerous at this point, because up to now
       the symbol is only internal in the package (except if it is
       the KEYWORD package, that can not be USE-ed). */
  }
  set_break_sem_3(); /* protect against breaks */
  /* now resolve the conflicts: */
  while (mconsp(STACK_0)) {
    var object cons_sym_pack = Car(STACK_0);
    STACK_0 = Cdr(STACK_0);
    pushSTACK(Car(cons_sym_pack)); /* solvingsym */
    pushSTACK(Cdr(cons_sym_pack)); /* usingpack */
    shadowing_import(&STACK_1,&STACK_0); /* import and shadow */
    skipSTACK(2);
  }
  skipSTACK(1);
  make_external(*sym_,*pack_); /* make sym in pack external */
  clr_break_sem_3(); /* allow breaks */
}

/* UP: Applies a function to all symbols in a symboltable.
 (In the worst case, this function may delete the symbol via symtab_delete
 from the table.)
 map_symtab(fun,symtab);
 > fun: function with one argument
 > symtab: symboltable
 can trigger GC */
local maygc void map_symtab (object fun, object symtab) {
  pushSTACK(fun); /* function */
  pushSTACK(Symtab_table(symtab)); /* table vector */
  /* number of entries */
  var uintL size = posfixnum_to_V(Symtab_size(symtab));
  var gcv_object_t* offset = 0; /* offset = sizeof(gcv_object_t)*index */
  var uintC count;
  dotimespC(count,size, {
    var object entry = /* entry with number index in table */
      *(gcv_object_t*)(pointerplus(&TheSvector(STACK_0)->data[0],(aint)offset));
    if (atomp(entry)) {
      if (!nullp(entry)) {
        /* entry is a symbol /= NIL */
        pushSTACK(entry); funcall(STACK_2,1); /* apply function */
      }
    } else {
      /* process non-empty symbol list */
      pushSTACK(entry);
      do {
        var object listr = STACK_0;
        STACK_0 = Cdr(listr);
        /* apply function to symbol */
        pushSTACK(Car(listr)); funcall(STACK_3,1);
      } while (!matomp(STACK_0));
      skipSTACK(1);
    }
    offset++;
  });
  skipSTACK(2);
}

/* UP: Applies a C-function to all symbols of a symbol table.
 (In the worst case, this function may delete the symbol via symtab_delete
 from the table.)
 map_symtab_c(fun,data,symtab);
 > fun: function with two arguments, may trigger GC
 > data: first argument for the function
 > symtab: symbol table
 can trigger GC */
typedef maygc void one_sym_function_t (void* data, object sym);
local maygc void map_symtab_c (one_sym_function_t* fun, void* data, object symtab) {
  pushSTACK(Symtab_table(symtab)); /* table vector */
  /* number of entries */
  var uintL size = posfixnum_to_V(Symtab_size(symtab));
  var gcv_object_t* offset = 0; /* offset = sizeof(gcv_object_t)*index */
  var uintC count;
  dotimespC(count,size, {
    var object entry = /* entry with number index in table */
      *(gcv_object_t*)(pointerplus(&TheSvector(STACK_0)->data[0],(aint)offset));
    if (atomp(entry)) {
      if (!nullp(entry)) { /* entry is a symbol /= NIL */
        (*fun)(data,entry); /* apply function */
      }
    } else { /* process non-empty symbol list */
      pushSTACK(entry);
      do {
        var object listr = STACK_0;
        STACK_0 = Cdr(listr);
        (*fun)(data,Car(listr)); /* apply function to symbol */
      } while (!matomp(STACK_0));
      skipSTACK(1);
    }
    offset++;
  });
  skipSTACK(1);
}

/* UP: Effectuates, that all external symbols of a given list of packages
 become implicitly accessible in a given package.
 use_package(packlist,pack);
 > packlist: list of packages, that are to be USE-ed
 > pack: package
 the list packlist is thereby destroyed!
 can trigger GC */
local one_sym_function_t use_package_aux;
local maygc void use_package (object packlist, object pack) {
  safe_check_pack_lock(S(use_package),pack,packlist);
  { /* packlist := (delete-duplicates packlist :test #'eq) : */
    var object packlist1 = packlist;
    while (consp(packlist1)) {
      var object to_delete = Car(packlist1);
      /* remove to_delete destructively from (cdr packlist1) : */
      var object packlist2 = packlist1; /* starts at packlist1 */
      var object packlist3; /* always = (cdr packlist2) */
      while (consp(packlist3=Cdr(packlist2))) {
        if (eq(Car(packlist3),to_delete)) {
          /* remove (car packlist3) destructively from the list: */
          Cdr(packlist2) = Cdr(packlist3);
        } else { /* advance: */
          packlist2 = packlist3;
        }
      }
      packlist1 = Cdr(packlist1);
    }
  }
  /* Remove all the packages from packlist, that are equal to pack
     or that already occur in the use-list of pack: */
  pushSTACK(pack); /* save package pack */
  pushSTACK(packlist); /* save list of packages to be USE-ed */
  {
    var gcv_object_t* packlistr_ = &STACK_0;
    var object packlistr = *packlistr_;
    /* packlistr loops over packlist, packlistr = *packlistr_ */
    while (consp(packlistr)) {
      /* test, if (car packlistr) must be discarded: */
      var object pack_to_test = Car(packlistr);
      if (eq(pack_to_test,pack))
        goto delete_pack_to_test;
      if (!nullp(memq(pack_to_test,ThePackage(pack)->pack_use_list)))
        goto delete_pack_to_test;
      if (true) { /* do not discard, advance: */
        packlistr_ = &Cdr(packlistr); packlistr = *packlistr_;
      } else {    /* discard (car packlistr) : */
       delete_pack_to_test:
        packlistr = *packlistr_ = Cdr(packlistr);
      }
    }
  }
  /* build conflict list.
     A conflict is an at least two-element list
     of symbols of the same printname, together with the package,
     from which this symbol is taken:
     ((pack1 . sym1) ...) means, that on execution of the USE-PACKAGE
     the symbole sym1,... (from pack1 etc.) would compete for
     the visibility in package pack.
     The conflict list is the list of all occurring conflicts. */
  {
    var gcv_object_t *pack_ = &STACK_1;
    var gcv_object_t *packlist_ = &STACK_0;
    var gcv_object_t *conflicts_, *conflict_resolver_;
    pushSTACK(NIL); /* (so far empty) conflict list */
    conflicts_ = &STACK_0;
    /* stack-layout: pack, packlist, conflicts. */
    { /* peruse package list: */
      pushSTACK(*packlist_);
      while (mconsp(STACK_0)) {
        var object pack_to_use = Car(STACK_0);
        STACK_0 = Cdr(STACK_0);
        /* apply use_package_aux to all external symbols of pack_to_use: */
        map_symtab_c(&use_package_aux,conflicts_,
                     ThePackage(pack_to_use)->pack_external_symbols);
      }
      skipSTACK(1);
    }
    { /* reconstruct conflict list: Each conflict ((pack1 . sym1) ...) is
       transformed into ((packname1 pack1 . sym1) ...). */
      pushSTACK(*conflicts_); /* traverse conflict list */
      while (mconsp(STACK_0)) {
        var object conflict = Car(STACK_0);
        STACK_0 = Cdr(STACK_0);
        pushSTACK(conflict); /* process conflict */
        while (mconsp(STACK_0)) {
          var object new_cons = allocate_cons(); /* new cons */
          var object old_cons = Car(STACK_0); /* (pack . sym) */
          /* replace pack by its name */
          Car(new_cons) = ThePackage(Car(old_cons))->pack_name;
          /* insert new-cons */
          Cdr(new_cons) = old_cons; Car(STACK_0) = new_cons;
          STACK_0 = Cdr(STACK_0);
        }
        skipSTACK(1);
      }
      skipSTACK(1);
    }
    /* conflict-list finished. */
    pushSTACK(NIL); /* conflict-resolver := NIL */
    conflict_resolver_ = &STACK_0;
    /* stack-layout: pack, packlist, conflicts, conflict-resolver. */
    /* treat conflicts with user-queries: */
    while (!nullp(*conflicts_)) { /* only necessary for conflicts/=NIL */
      /* raise correctable error: */
      pushSTACK(Car(*conflicts_));           /* OPTIONS */
      pushSTACK(*pack_);        /* PACKAGE-ERROR slot PACKAGE */
      pushSTACK(*pack_);
      pushSTACK(Symbol_name(Cdr(Cdr(Car(Car(*conflicts_)))))); /* name */
      pushSTACK(fixnum(llength(*conflicts_))); /* (length conflicts) */
      pushSTACK(*pack_); pushSTACK(*packlist_); pushSTACK(S(use_package));
      correctable_error(package_error,GETTEXT("(~S ~S ~S): ~S name conflicts remain\nWhich symbol with name ~S should be accessible in ~S?"));
      pushSTACK(value1); /* sym */
      {
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK(); /* sym */
        Cdr(new_cons) = *conflict_resolver_;
        /* conflict-resolver := (cons sym conflict-resolver) */
        *conflict_resolver_ = new_cons;
      }
      *conflicts_ = Cdr(*conflicts_);
    }
    /* stack-layout: pack, packlist, conflicts, conflict-resolver. */
    { /* resolve conflicts: */
      set_break_sem_3();
      /* traverse conflict-resolver: */
      while (mconsp(STACK_0)) {
        pushSTACK(Car(STACK_0)); /* symbol from conflict-resolver */
        /* make it into a shadowing-symbol in pack */
        shadowing_import(&STACK_0,&STACK_4);
        skipSTACK(1);
        STACK_0 = Cdr(STACK_0);
      }
      skipSTACK(2); /* forget conflicts and conflict-resolver */
      /* stack-layout: pack, packlist. */
      /* traverse packlist: */
      while (mconsp(STACK_0)) {
        pushSTACK(Car(STACK_0)); /* pack_to_use */
        { /* (push pack_to_use (package-use-list pack)) */
          var object new_cons = allocate_cons();
          var object pack = STACK_2;
          Car(new_cons) = STACK_0; /* pack_to_use */
          Cdr(new_cons) = ThePackage(pack)->pack_use_list;
          ThePackage(pack)->pack_use_list = new_cons;
        }
        { /* (push pack (package-used-by-list pack_to_use)) */
          var object new_cons = allocate_cons();
          var object pack_to_use = popSTACK();
          Car(new_cons) = STACK_1; /* pack */
          Cdr(new_cons) = ThePackage(pack_to_use)->pack_used_by_list;
          ThePackage(pack_to_use)->pack_used_by_list = new_cons;
        }
        STACK_0 = Cdr(STACK_0);
      }
      skipSTACK(2); /* forget pack and packlist */
      clr_break_sem_3();
    }
  }
}

/* UP: Auxiliary function for use_package:
 Test the argument (an external symbol from one of the packages of
 packlist), if it creates a conflict. If yes, extend conflicts.
 can trigger GC */
local maygc void use_package_aux (void* data, object sym) {
  var gcv_object_t* localptr = (gcv_object_t*)data;
  /* Pointer to local variables of use_package:
     *(localptr STACKop 2) = pack,
     *(localptr STACKop 1) = packlist,
     *(localptr STACKop 0) = conflicts. */
  var object string = Symbol_name(sym); /* printname of the passed symbol */
  /* Is there a conflict between the symbols with printname = string ?
     travares conflict list so far (((pack1 . sym1) ...) ...) : */
  {
    var object conflictsr = *(localptr STACKop 0);
    while (consp(conflictsr)) {
      /* conflict already treated?
         (car conflictsr) = next conflict,
         (car (car conflictsr)) = its first cons,
         (cdr (car (car conflictsr))) = the symbol therein,
         is its printname = string ? */
      if (string_gleich(Symbol_name(Cdr(Car(Car(conflictsr)))),string))
        goto ok;
      conflictsr = Cdr(conflictsr);
    }
  }
  pushSTACK(string); /* save string */
  /* build new conflict: */
  pushSTACK(NIL); /* new conflict (still empty) */
  { /* test, if a symbol of the same name is already accessible in pack: */
    var object othersym;
    var sintBWL code = find_symbol(string,false,*(localptr STACKop 2),&othersym);
    if (code < 0) {
      /* Eponymous symbol in the shadowing-list impedes conflict. */
      skipSTACK(2); goto ok;
    }
    if (code > 0) {
      /* accessible, but not shadowing ->
         extend conflict by (pack . othersym) : */
      pushSTACK(othersym);
      {
        var object temp = allocate_cons();
        Cdr(temp) = popSTACK(); /* othersym */
        Car(temp) = *(localptr STACKop 2); /* pack */
        pushSTACK(temp); /* (pack . othersym) */
      }
      {
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
        STACK_0 = new_cons;
      }
    }
  }
  /* Test, in which packages from packlist a symbol of the same name
     is external: */
  {
    var object packlistr = *(localptr STACKop 1); /* traverse packlist */
    while (consp(packlistr)) {
      var object pack_to_use = Car(packlistr);
      packlistr = Cdr(packlistr);
      var object othersym;
      if (package_lookup_ext(STACK_1,false,pack_to_use,&othersym)) {
        /* othersym has the printname = string and is
           external in pack_to_use.
           push (pack_to_use . othersym) on conflict: */
        pushSTACK(packlistr); /* save packlistr */
        pushSTACK(pack_to_use);
        pushSTACK(othersym);
        {
          var object new_cons = allocate_cons();
          Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
          pushSTACK(new_cons); /* (cons pack_to_use othersym) */
        }
        {
          var object new_cons = allocate_cons();
          Car(new_cons) = popSTACK();
          packlistr = popSTACK();
          Cdr(new_cons) = STACK_0;
          /* conflict := (cons (cons pack_to_use othersym) conflict) */
          STACK_0 = new_cons;
        }
      }
    }
  }
  {
    var object conflict = popSTACK(); /* the completed conflict */
    /* conflict := (delete-duplicates conflict :key #'cdr :test #'eq): */
    {
      var object conflict1 = conflict;
      while (consp(conflict1)) {
        var object to_delete = Cdr(Car(conflict1));
        /* Remove all elements with CDR=to_delete
           destructively from (cdr conflict1) : */
        var object conflict2 = conflict1; /* starts at conflict1 */
        var object conflict3; /* always = (cdr conflict2) */
        while (consp(conflict3=Cdr(conflict2))) {
          if (eq(Cdr(Car(conflict3)),to_delete)) {
            /* discard (car conflict3) destructively from the list: */
            Cdr(conflict2) = Cdr(conflict3);
          } else { /* advance: */
            conflict2 = conflict3;
          }
        }
        conflict1 = Cdr(conflict1);
      }
    }
    /* if conflict has a length >=2 , it is consed to conflicts: */
    if (consp(conflict) && mconsp(Cdr(conflict))) {
      pushSTACK(conflict);
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK(); /* conflict */
      Cdr(new_cons) = *(localptr STACKop 0); /* conflicts */
      /* conflicts := (cons conflict conflicts) */
      *(localptr STACKop 0) = new_cons;
    }
  }
  skipSTACK(1); /* forget string */
 ok: ;
}

/* UP: Effectuates, that a given package is not USE-ed anymore
 by (another) package.
 unuse_1package(pack,qpack);
 > pack: package
 > qpack: package
 Removes qpack from the use-list of pack
 and pack from the used-by-list of qpack.
 can trigger GC */
local maygc void unuse_1package (object pack, object qpack) {
  safe_check_pack_lock(S(use_package),pack,qpack);
  set_break_sem_2();
  /* remove qpack from the use-list of pack: */
  ThePackage(pack)->pack_use_list =
    deleteq(ThePackage(pack)->pack_use_list,qpack);
  /* remove pack from the used-by-list of qpack: */
  ThePackage(qpack)->pack_used_by_list =
    deleteq(ThePackage(qpack)->pack_used_by_list,pack);
  clr_break_sem_2();
}

/* UP: Effectuates, that a list of given packages is not USE-ed anymore
 by a given package.
 unuse_package(packlist,pack);
 > packlist: list of packages
 > pack: package
 Removes all packages from packlist from the use-list of pack
 and pack from the used-by-lists of all packages from packlist.
 can trigger GC */
local maygc void unuse_package (object packlist, object pack) {
  pushSTACK(pack);
  pushSTACK(packlist);
  set_break_sem_3();
  /* traverse packlist: */
  while (mconsp(STACK_0)) {
    unuse_1package(STACK_1,Car(STACK_0));
    STACK_0 = Cdr(STACK_0);
  }
  clr_break_sem_3();
  skipSTACK(2);
}

/* UP: returns the current package
 get_current_package()
 < result: current package
 can trigger GC */
global maygc object get_current_package (void) {
  var object pack = Symbol_value(S(packagestern)); /* value of *PACKAGE* */
  if (packagep(pack) && !pack_deletedp(pack)) {
    return pack;
  } else {
    var object newpack = /* reset *PACKAGE* */
      Symbol_value(S(packagestern)) = O(default_package);
    /* get_current_package() is often called by the reader,
       so we need to save and restore the read buffers */
    pushSTACK(O(token_buff_1)); O(token_buff_1) = NIL;
    pushSTACK(O(token_buff_2)); O(token_buff_2) = NIL;
    pushSTACK(NIL);             /* 8: "Proceed with the new value." */
    pushSTACK(S(type_error));   /* 7: error type */
    pushSTACK(S(Kdatum));       /* 6: :DATUM */
    pushSTACK(pack);            /* 5: TYPE-ERROR slot DATUM */
    pushSTACK(S(Kexpected_type)); /* 4: :EXPECTED-TYPE */
    pushSTACK(S(package));      /* 3: TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(NIL);             /* 2: "The value of ..." */
    pushSTACK(pack);            /* 1: old name */
    pushSTACK(newpack);         /* 0: new name */
    STACK_2 = CLSTEXT("The value of *PACKAGE* was not a package and was reset. The old value was ~S. The new value is ~S.");
    STACK_8 = CLSTEXT("Proceed with the new value.");
    funcall(L(cerror_of_type),9);
    O(token_buff_2) = popSTACK(); /* restore read buffers */
    O(token_buff_1) = popSTACK();
    return Symbol_value(S(packagestern));
  }
}

/* UP: checks a package-argument.
 Tests, if it is a package or a package name, and returns it as
 a package. Else error message.
 test_package_arg(obj)
 > obj: argument
 < result: argument turned into a package
 can trigger GC */
local maygc object test_package_arg (object obj) {
 restart_package_arg:
  if (packagep(obj)) { /* package -> mostly OK */
    if (!pack_deletedp(obj))
      return obj;
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* PACKAGE-ERROR slot PACKAGE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(package_error,GETTEXT("~S: Package ~S has been deleted."));
    obj = value1;
    goto restart_package_arg;
  }
  if (stringp(obj))
  string: { /* string -> search package with name obj: */
    var object pack = find_package(obj);
    if (!nullp(pack))
      return pack;
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* PACKAGE-ERROR slot PACKAGE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(package_error,GETTEXT("~S: There is no package with name ~S"));
    obj = value1;
    goto restart_package_arg;
  }
  if (symbolp(obj)) { /* symbol -> string */
    obj = Symbol_name(obj); goto string; /* use print name, no case-invert */
  }
  if (charp(obj)) { /* character -> string */
    var object new_string = allocate_string(1);
    TheSnstring(new_string)->data[0] = char_code(obj);
    obj = new_string;
    goto string;
  }
  pushSTACK(NIL); /* no PLACE */
  pushSTACK(obj); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_packname)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,GETTEXT("~S: argument should be a package or a package name, not ~S"));
  obj = value1;
  goto restart_package_arg;
}

LISPFUNNR(make_symbol,1) { /* (MAKE-SYMBOL printname), CLTL p. 168 */
  var object arg = check_string(popSTACK());
  VALUES1(make_symbol(coerce_imm_ss(arg)));
}

LISPFUNNR(find_package,1) { /* (FIND-PACKAGE name), CLTL p. 183 */
  var object pack = popSTACK();
  if (packagep(pack)) VALUES1(pack);
  else {
    var object name = test_stringsymchar_arg(pack,false);
    VALUES1(find_package(name)); /* search package */
  }
}

LISPFUNN(pfind_package,1) { /* (SYSTEM::%FIND-PACKAGE name) */
  VALUES1(test_package_arg(popSTACK())); /* argument as package */
}

LISPFUNNR(package_name,1) { /* (PACKAGE-NAME package), CLTL p. 184 */
  var object pack = popSTACK();
  if (packagep(pack) && pack_deletedp(pack)) {
    VALUES1(NIL);
  } else {
    pack = test_package_arg(pack); /* argument as package */
    VALUES1(ThePackage(pack)->pack_name); /* the name */
  }
}

LISPFUNNR(package_nicknames,1)
{ /* (PACKAGE-NICKNAMES package), CLTL p. 184 */
  var object pack = popSTACK();
  if (packagep(pack) && pack_deletedp(pack)) {
    VALUES1(NIL);
  } else {
    pack = test_package_arg(pack); /* argument as package */
    /* copy nicknamelist for safety reasons */
    VALUES1(copy_list(ThePackage(pack)->pack_nicknames));
  }
}

/* UP: checks name and nicknames -
 arguments of RENAME-PACKAGE and MAKE-PACKAGE.
 Tests, if STACK_4 is a name, and turns it into a immutable simple-string.
 Tests, if STACK_3 is a name or a list of names, and turns it
 into a new list of immutable simple-strings.
 > subr-self: caller (a SUBR)
 can trigger GC */
local maygc void test_names_args (void) {
  /* check name for string and turn it into a simple-string: */
  STACK_4 = coerce_imm_ss(test_stringsymchar_arg(STACK_4,false));
  { /* convert nickname-argument into a list: */
    var object nicknames = STACK_3;
    if (!boundp(nicknames)) {
      STACK_3 = NIL; /* no nicknames specified -> default NIL */
    } else {
      if (!listp(nicknames)) {
        /* nicknames not a list -> turn it into a one-element list: */
        nicknames = allocate_cons();
        Car(nicknames) = STACK_3;
        STACK_3 = nicknames;
      }
    }
  }
  { /* check nickname(s) for string, turn into simple-strings
     and build a new nicknamelist: */
    pushSTACK(NIL); /* new nicknamelist := NIL */
    while (mconsp(STACK_4)) {
      {
        var object nickname = Car(STACK_4); /* next nickname */
        STACK_4 = Cdr(STACK_4);
        /* as simple-string */
        nickname = coerce_imm_ss(test_stringsymchar_arg(nickname,false));
        /* cons in front of the new nicknamelist: */
        pushSTACK(nickname);
      }
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      Cdr(new_cons) = STACK_0;
      STACK_0 = new_cons;
    }
    var object nicknames = popSTACK();
    STACK_3 = nicknames; /* new nicknamelist replaces the old */
  }
}

/* (RENAME-PACKAGE pack name [nicknames]), CLTL p. 184 */
LISPFUN(rename_package,seclass_default,2,1,norest,nokey,0,NIL) {
  /* Test, if pack is a package: */
  STACK_2 = test_package_arg(STACK_2);
  check_pack_lock(S(rename_package),STACK_2,STACK_1);
  /* check name and nicknames:
     name is a package designator here (but not in make-package!) */
  if (packagep(STACK_1)) STACK_1 = ThePackage(STACK_1)->pack_name;
  pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL); /* dummies on the stack */
  test_names_args();
  skipSTACK(3);
  var object pack = STACK_2;
  { /* test, if a package-name-conflict arises: */
    var object name = STACK_1;
    var object nicknamelistr = STACK_0;
    /* name loops over the names and all nicknames */
    loop { /* find package with this name: */
      var object found = find_package(name);
      if (!(nullp(found) || eq(found,pack))) {
        /* found, but another one than the given package: */
        pushSTACK(pack); /* PACKAGE-ERROR slot PACKAGE */
        pushSTACK(name); pushSTACK(TheSubr(subr_self)->name);
        fehler(package_error,GETTEXT("~S: there is already a package named ~S"));
      }
      /* none or only the given package has the Name name ->
         no conflict with this (nick)name, continue: */
      if (atomp(nicknamelistr))
        break;
      name = Car(nicknamelistr); /* next nickname */
      nicknamelistr = Cdr(nicknamelistr); /* shorten remaining nicknamelist */
    }
  }
  /* There are no conflicts. */
  set_break_sem_2();
  ThePackage(pack)->pack_name = STACK_1;
  ThePackage(pack)->pack_nicknames = STACK_0;
  clr_break_sem_2();
  ensure_pack_shortest_name(pack);
  skipSTACK(3);
  VALUES1(pack); /* pack as value */
}

LISPFUNNR(package_use_list,1) { /* (PACKAGE-USE-LIST package), CLTL p. 184 */
  var object pack = test_package_arg(popSTACK()); /* argument as package */
  /* copy use-list for safety reasons */
  VALUES1(copy_list(ThePackage(pack)->pack_use_list));
}

LISPFUNNR(package_used_by_list,1)
{ /* (PACKAGE-USED-BY-LIST package), CLTL p. 184 */
  var object pack = test_package_arg(popSTACK()); /* argument as package */
  /* copy used-by-list for safety reasons */
  VALUES1(copy_list(ThePackage(pack)->pack_used_by_list));
}

LISPFUNNR(package_shadowing_symbols,1)
{ /* (PACKAGE-SHADOWING-SYMBOLS package), CLTL p. 184 */
  var object pack = test_package_arg(popSTACK()); /* argument as package */
  /* copy shadowing-list for safety reasons */
  VALUES1(copy_list(ThePackage(pack)->pack_shadowing_symbols));
}

/* (EXT:PACKAGE-CASE-SENSITIVE-P package) */
LISPFUNNR(package_case_sensitive_p,1) {
  var object pack = test_package_arg(popSTACK());
  VALUES_IF(pack_casesensitivep(pack));
}

/* ((SETF EXT:PACKAGE-CASE-SENSITIVE-P) value package) */
LISPFUNN(set_package_case_sensitive_p,2) {
  var object pack = test_package_arg(popSTACK());
  var bool value = !nullp(popSTACK());
  if (value) mark_pack_casesensitive(pack);
  else mark_pack_caseinsensitive(pack);
  VALUES_IF(value);
}

/* (EXT:PACKAGE-CASE-INVERTED-P package) */
LISPFUNNR(package_case_inverted_p,1) {
  var object pack = test_package_arg(popSTACK());
  VALUES_IF(pack_caseinvertedp(pack));
}

/* ((SETF EXT:PACKAGE-CASE-INVERTED-P) value package) */
LISPFUNN(set_package_case_inverted_p,2) {
  var object pack = test_package_arg(popSTACK());
  var bool value = !nullp(popSTACK());
  if (value) mark_pack_caseinverted(pack);
  else mark_pack_casepreserved(pack);
  VALUES_IF(value);
}

/* (SYS::PACKAGE-DOCUMENTATION package) */
LISPFUNNR(package_documentation,1) {
  var object pack = test_package_arg(popSTACK());
  VALUES1(ThePackage(pack)->pack_docstring);
}

/* ((SETF SYS::PACKAGE-DOCUMENTATION) new-value package)
 documentation is either a doc-string or a list (doc-string impnotes-id) */
LISPFUNN(set_package_documentation,2) {
  STACK_0 = test_package_arg(STACK_0);
  if (!listp(STACK_1)) STACK_1 = check_string(STACK_1);
  VALUES1(ThePackage(STACK_0)->pack_docstring = STACK_1);
  skipSTACK(2);
}

LISPFUNNR(package_shortest_name,1)
{ /* (EXT:PACKAGE-SHORTEST-NAME package) */
  var object pack = test_package_arg(popSTACK());
  VALUES1(ThePackage(pack)->pack_shortest_name);
}

LISPFUNNR(package_lock,1)
{ /* (EXT:PACKAGE-LOCK package) */
  var object pack = test_package_arg(popSTACK());
  VALUES_IF(pack_locked_p(pack));
}

/* ((SETF EXT:PACKAGE-LOCK) lock package) */
LISPFUNN(set_package_lock,2) {
  var bool unlock_p = nullp(STACK_1);
  var object pack = STACK_0;
  if (mconsp(pack)) {
    while (mconsp(STACK_0)) {
      var object pa = test_package_arg(Car(STACK_0)); STACK_0 = Cdr(STACK_0);
      if (unlock_p) mark_pack_unlocked(pa);
      else          mark_pack_locked(pa);
    }
  } else if (nullp(pack)) { /* do nothing - package list was empty */
  } else {
    pack = test_package_arg(pack);
    if (unlock_p) mark_pack_unlocked(pack);
    else          mark_pack_locked(pack);
  }
  skipSTACK(2);
  VALUES_IF(!unlock_p);
}

/* barf when SYMBOL is an unaccessible special variable
   being modified from a non-home package.
   See compiler.lisp:set-check-lock.
   can trigger GC */
#define SYM_VAL_LOCK(symbol,pack)                                         \
  (!nullp(pack) && !eq(pack,Symbol_value(S(packagestern))) /* non-home */ \
   && special_var_p(TheSymbol(symbol))  /* special */                     \
   && !externalp(symbol,pack) /* for IN-PACKAGE forms */                  \
   && !accessiblep(symbol,Symbol_value(S(packagestern)))) /* accessible */
global maygc void symbol_value_check_lock (object caller, object symbol) {
  var object pack = Symbol_package(symbol);
  if (SYM_VAL_LOCK(symbol,pack))
    check_pack_lock(caller,pack,symbol);
}
LISPFUNN(symbol_value_lock,1) { /* SYS::SYMBOL-VALUE-LOCK */
  var object symb = check_symbol(popSTACK());
  var object pack = Symbol_package(symb);
  VALUES_IF(SYM_VAL_LOCK(symb,pack) && pack_locked_p(pack));
}

/* (SYSTEM::CHECK-PACKAGE-LOCK caller package symbol)
   when FUNCTION is (P)SETQ, calls symbol_value_check_lock() */
LISPFUNN(check_package_lock,3) {
  if (mconsp(STACK_1)) { /* package is actually a list of packages */
    var bool locked = true;
    var object list = STACK_1;
    /* for the package list to be "locked", _all_ members must be locked
       non-package members mean that the argument was a defmethod spec like
       (eql 1), which means unlocked: you can always redefine such methods */
    while (locked && mconsp(list)) {
      locked = (packagep(Car(list)) ? pack_locked_p(Car(list)) : false);
      list = Cdr(list);
    }
    if (locked) /* all packages are locked --> error */
      cerror_package_locked(STACK_2,STACK_1,STACK_0);
  } else if (packagep(STACK_1)) /* just one package - check it */
    check_pack_lock(STACK_2,STACK_1,STACK_0);
  skipSTACK(3);
  mv_count = 0;
}

LISPFUNNR(list_all_packages,0)
{ /* (LIST-ALL-PACKAGES) returns a list of all packages, CLTL p. 184 */
  VALUES1(reverse(O(all_packages))); /* (copy of the list, as a precaution) */
}

/* UP: check the last argument &optional (pack *package*) of
 a LISP-function.
 test_optional_package_arg()
 > STACK_0: last argument
 < STACK_0: argument transformed into a package
 can trigger GC */
local maygc void test_optional_package_arg (void) {
  var object pack = STACK_0;
  if (!boundp(pack)) {
    STACK_0 = get_current_package(); /* default is the value of *PACKAGE* */
  } else {
    STACK_0 = test_package_arg(pack);
  }
}

/* UP: Check of the arguments of INTERN and FIND-SYMBOL.
 test_intern_args()
 can trigger GC */
local maygc void test_intern_args (void) {
  STACK_1 = check_string(STACK_1); /* test string */
  test_optional_package_arg(); /* test package */
}

/* UP: Transforms a INTERN/FIND-SYMBOL - result into a keyword.
 intern_result(code)
 > code : flag as for intern and find_symbol
 < result : corresponding keyword */
local object intern_result (uintBWL code) {
  switch (code) {
    case 0: return NIL;           /* 0 -> NIL */
    case 1: return S(Kexternal);  /* 1 -> :EXTERNAL */
    case 2: return S(Kinherited); /* 2 -> :INHERITED */
    case 3: return S(Kinternal);  /* 3 -> :INTERNAL */
    default: NOTREACHED;
  }
}

/* (INTERN string [package]) and its case-inverted variant */
local maygc Values do_intern (bool invert) {
  test_intern_args(); /* test arguments */
  var object pack = popSTACK();
  var object string = popSTACK();
 #if !defined(VALUE1_EXTRA)
  var uintBWL code = intern(string,invert,pack,&value1); /* symbol to value1 */
 #else
  var object value;
  var uintBWL code = intern(string,invert,pack,&value); /* Symbol to value */
  value1 = value;
 #endif
  value2 = intern_result(code); mv_count=2; /* two values */
}

/* (INTERN string [package]), CLTL p. 184 */
LISPFUN(intern,seclass_default,1,1,norest,nokey,0,NIL) {
  do_intern(false);
}

/* (CS-COMMON-LISP:INTERN string [package]) */
LISPFUN(cs_intern,seclass_default,1,1,norest,nokey,0,NIL) {
  do_intern(true);
}

/* (FIND-SYMBOL string [package]) and its case-inverted variant */
local maygc Values do_find_symbol (bool invert) {
  test_intern_args(); /* test arguments */
  var object pack = popSTACK();
  var object string = popSTACK();
 #if !defined(VALUE1_EXTRA)
  var uintBWL code = find_symbol(string,invert,pack,&value1) & 3; /* symbol to value1 */
 #else
  var object value;
  var uintBWL code = find_symbol(string,invert,pack,&value) & 3; /* symbol to value */
  value1 = value;
 #endif
  value2 = intern_result(code); mv_count=2; /* two values */
}

/* (FIND-SYMBOL string [package]), CLTL p. 185 */
LISPFUN(find_symbol,seclass_read,1,1,norest,nokey,0,NIL)
{
  do_find_symbol(false);
}

/* (CS-COMMON-LISP:FIND-SYMBOL string [package]) */
LISPFUN(cs_find_symbol,seclass_read,1,1,norest,nokey,0,NIL)
{
  do_find_symbol(true);
}

/* (UNINTERN symbol [package]), CLTL p. 185 */
LISPFUN(unintern,seclass_default,1,1,norest,nokey,0,NIL) {
  /* test symbol: */
  STACK_1 = check_symbol(STACK_1);
  /* test package: */
  test_optional_package_arg();
  /* unintern: */
  VALUES1(unintern(&STACK_1,&STACK_0));
  skipSTACK(2);
}

/* UP: Dispatcher of a function like EXPORT, UNEXPORT, IMPORT, SHADOWING-IMPORT
 or SHADOW. tests, if the first argument is a symbol-list, if
 the second argument (default: *PACKAGE*) is a package, and applies the
 subroutine to each of the symbols. Return 1 value T.
 apply_symbols(&fun);
 specification of the subroutine fun:
   fun(&sym,&pack);
   > sym: symbol (in STACK)
   > pack: package (in STACK)
   < pack: package, EQ to the old one
   can trigger GC
 < STACK: cleaned up
 can trigger GC */
typedef maygc void sym_pack_function_t (const gcv_object_t* sym_, const gcv_object_t* pack_);
local maygc Values apply_symbols (sym_pack_function_t* fun) {
  { /* test, if the first argument is a symbol-list or a symbol: */
    var object symarg = STACK_1;
    /* test for symbol: */
    if (symbolp(symarg))
      goto ok;
    if ((fun == &shadow || fun == &cs_shadow)
        && (stringp(symarg) || charp(symarg)))
      goto ok;
    /* test for symbol-list: */
    while (consp(symarg)) { /* symarg loops over STACK_1 */
      if (!(symbolp(Car(symarg))
            || ((fun == &shadow || fun == &cs_shadow)
                && (stringp(Car(symarg)) || charp(Car(symarg))))))
        goto not_ok;
      symarg = Cdr(symarg);
    }
    if (!nullp(symarg))
      goto not_ok; /* list correctly finished? */
    goto ok; /* correct symbol-list */
  not_ok:
    pushSTACK(STACK_1); pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: argument should be a symbol or a list of symbols, not ~S"));
  ok: ;
  }
  /* test package: */
  test_optional_package_arg();
  /* stack-layout: symarg, pack. */
  /* apply fun to all symbols: */
  if (matomp(STACK_1)) {
    if (nullp(STACK_1)) {
      /* ANSI CL 11.1.1. says
         "Where an operator takes an argument that is either a symbol or a list of
          symbols, an argument of nil is treated as an empty list of symbols." */
    } else {
      /* single symbol */
      /* stack-layout: sym, pack. */
      (*fun)(&STACK_1,&STACK_0);
    }
    skipSTACK(2);
  } else {
    /* non-empty symbol-list */
    pushSTACK(NIL);
    do {
      var object symlistr = STACK_2;
      STACK_2 = Cdr(symlistr);
      STACK_0 = Car(symlistr); /* symbol */
      /* stack-layout: symlistr, pack, sym. */
      (*fun)(&STACK_0,&STACK_1);
    } while (!matomp(STACK_2));
    skipSTACK(3);
  }
  /* finish: */
  VALUES1(T);
}

/* (EXPORT symbols [package]), CLTL p. 186 */
LISPFUN(export,seclass_default,1,1,norest,nokey,0,NIL) {
  return_Values apply_symbols(&export);
}

/* (UNEXPORT symbols [package]), CLTL p. 186 */
LISPFUN(unexport,seclass_default,1,1,norest,nokey,0,NIL) {
  return_Values apply_symbols(&unexport);
}

/* (IMPORT symbols [package]), CLTL p. 186 */
LISPFUN(import,seclass_default,1,1,norest,nokey,0,NIL) {
  return_Values apply_symbols(&import);
}

/* (SHADOWING-IMPORT symbols [package]), CLTL p. 186 */
LISPFUN(shadowing_import,seclass_default,1,1,norest,nokey,0,NIL) {
  return_Values apply_symbols(&shadowing_import);
}

/* (SHADOW symbols [package]), CLTL p. 186 */
LISPFUN(shadow,seclass_default,1,1,norest,nokey,0,NIL) {
  return_Values apply_symbols(&shadow);
}

/* (CS-COMMON-LISP:SHADOW symbols [package]) */
LISPFUN(cs_shadow,seclass_default,1,1,norest,nokey,0,NIL) {
  return_Values apply_symbols(&cs_shadow);
}

/* UP: Preparation of the arguments of USE-PACKAGE and UNUSE-PACKAGE.
 The first argument STACK_1 is turned into a (newly created)
 list of packages, the second argument STACK_0 is checked.
 can trigger GC */
local maygc void prepare_use_package (void) {
  /* check second argument (package) : */
  test_optional_package_arg();
  { /* check first argument (package or package-list) : */
    var object packs_to_use = STACK_1;
    if (!listp(packs_to_use)) {
      /* packs_to_use not a list -> turn it into a one-element list: */
      pushSTACK(test_package_arg(packs_to_use)); /* single package */
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      STACK_1 = new_cons;
    } else { /* packs_to_use a list -> build up new package-list: */
      pushSTACK(NIL); /* start with NIL */
      while (mconsp(STACK_2)) {
        var object packlistr = STACK_2;
        STACK_2 = Cdr(packlistr);
        pushSTACK(test_package_arg(Car(packlistr))); /* next package */
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK();
        Cdr(new_cons) = STACK_0;
        STACK_0 = new_cons;
      }
      var object packlist = popSTACK(); /* new package-list */
      STACK_1 = packlist;
    }
  }
}

/* (USE-PACKAGE packs-to-use [package]), CLTL p. 187 */
LISPFUN(use_package,seclass_default,1,1,norest,nokey,0,NIL) {
  prepare_use_package();
  var object pack = popSTACK();
  var object packlist = popSTACK();
  use_package(packlist,pack);
  VALUES1(T);
}

/* (UNUSE-PACKAGE packs-to-use [package]), CLTL p. 187 */
LISPFUN(unuse_package,seclass_default,1,1,norest,nokey,0,NIL) {
  prepare_use_package();
  var object pack = popSTACK();
  var object packlist = popSTACK();
  unuse_package(packlist,pack);
  VALUES1(T);
}

/* UP: Corrects a package(nick)name.
 > name: Desired package-name (simple-string)
 > nickname_p: is this a name or a nickname
 < result: not yet existing package-name
           or NIL if CONTINUE restart is selected
 can trigger GC */
local maygc object correct_packname (object name, bool nickname_p) {
  var object pack;
  while (!nullp(pack=find_package(name))) {
    /* package with this name already exists */
    pushSTACK(NIL);             /* OPTIONS */
    pushSTACK(pack);            /* PACKAGE-ERROR slot package */
    pushSTACK(name); pushSTACK(TheSubr(subr_self)->name);
    /* fill OPTIONS */
    pushSTACK(S(continue));     /* restart name */
    pushSTACK(nickname_p ? CLSTEXT("discard this nickname")
              : CLSTEXT("return the existing package"));
    var object tmp = listof(2);
    pushSTACK(tmp);
    pushSTACK(S(read));         /* restart name */
    pushSTACK(nickname_p ? CLSTEXT("input another nickname")
              : CLSTEXT("input another name"));
    pushSTACK(S(prompt_for_new_value)); /* interactive function */
    pushSTACK(NIL); /* place */
    tmp = listof(4);
    pushSTACK(tmp);
    tmp = listof(2); STACK_3 = tmp; /* options list */
    correctable_error(package_error,GETTEXT("~S: a package with name ~S already exists."));
    if (nullp(value1)) return NIL; /* continue */
    name = test_stringsymchar_arg(value1,false);
  }
  return coerce_imm_ss(name);
}

/* UP for MAKE-PACKAGE and %IN-PACKAGE:
 Builds a new package and returns it as value.
 > STACK_4: name-argument
 > STACK_3: nicknames-argument
 > STACK_2: uselist-argument
 > STACK_1: case-sensitive-argument
 > STACK_0: case-inverted-argument
 removes the 5 STACK elements
 can trigger GC */
local maygc void in_make_package (bool case_inverted) {
  /* transform name into simple-string and
     nicknames into a new simple-string-list: */
  test_names_args();
  var object new_name = correct_packname(STACK_4,false);
  if (nullp(new_name)) {     /* CONTINUE: re-use the existing package */
    VALUES1(find_package(STACK_4));
    skipSTACK(5);
    return;
  } else                     /* corrected: replace */
    STACK_4 = new_name;
  /* check nicknames and maybe adjust: */
  pushSTACK(STACK_3);
  while (mconsp(STACK_0)) {
    var object correct_nick = correct_packname(Car(STACK_0),true);
    Car(STACK_0) = correct_nick;
    STACK_0 = Cdr(STACK_0);
  }
  skipSTACK(1);
  STACK_3 = deleteq(STACK_3,NIL);
  /* (DELETE-DUPLICATES NICKNAMES :TEST (FUNCTION STRING=)) */
  pushSTACK(STACK_3); pushSTACK(S(Ktest)); pushSTACK(L(string_gleich));
  funcall(L(delete_duplicates),3);
  STACK_3 = value1;
  /* create package: */
  STACK_4 = make_package(STACK_4,STACK_3,
                         boundp(STACK_1) ? !nullp(STACK_1) : case_inverted,
                         boundp(STACK_0) ? !nullp(STACK_0) : case_inverted);
  /* stack-layout: pack, nicknames, uselist, case-sensitive, case-inverted. */
  /* use default value for use-argument: */
  if (!boundp(STACK_2))
    STACK_2 = O(use_default);
  /* execute (USE-PACKAGE uselist newpackage) : */
  pushSTACK(STACK_2); /* uselist */
  pushSTACK(STACK_(4+1)); /* package */
  funcall(L(use_package),2);
  skipSTACK(4);
  VALUES1(popSTACK()); /* package as value */
}

/* (MAKE-PACKAGE name [:NICKNAMES nicknames] [:USE uselist]
                      [:CASE-SENSITIVE sensitivep] [:CASE-INVERTED invertedp]),
 CLTL p. 183 */
LISPFUN(make_package,seclass_default,1,0,norest,key,4,
        (kw(nicknames),kw(use),kw(case_sensitive),kw(case_inverted)) ) {
  in_make_package(false);
}

/* (CS-COMMON-LISP:MAKE-PACKAGE name [:NICKNAMES nicknames] [:USE uselist]
                                     [:CASE-SENSITIVE sensitivep] [:CASE-INVERTED invertedp]) */
LISPFUN(cs_make_package,seclass_default,1,0,norest,key,4,
        (kw(nicknames),kw(use),kw(case_sensitive),kw(case_inverted)) ) {
  in_make_package(true);
}

/* (SYSTEM::%IN-PACKAGE name [:NICKNAMES nicknames] [:USE uselist]
                        [:CASE-SENSITIVE sensitivep] [:CASE-INVERTED invertedp])
 is like (IN-PACKAGE name [:NICKNAMES nicknames] [:USE uselist]), CLTL p. 183,
 except that *PACKAGE* is not modified. */
LISPFUN(pin_package,seclass_default,1,0,norest,key,4,
        (kw(nicknames),kw(use),kw(case_sensitive),kw(case_inverted)) ) {
  /* check name and turn into string: */
  var object name = test_stringsymchar_arg(STACK_4,false);
  STACK_4 = name;
  /* find package with this name: */
  var object pack = find_package(name);
  if (nullp(pack)) { /* package not found, must create a new one */
    in_make_package(false);
  } else { /* package found */
    STACK_4 = pack; /* save pack */
    /* stack-layout: pack, nicknames, uselist, case-sensitive, case-inverted. */
    if (boundp(STACK_1)) { /* check the case-sensitivity: */
      var bool value = !nullp(STACK_1);
      if (!!pack_casesensitivep(pack) != value) {
        pushSTACK(pack); pushSTACK(pack);
        STACK_1 = CLSTEXT("One should not change the case sensitiveness of ~S.");
        funcall(S(warn),2);
        pack = STACK_4;         /* restore for GC-safety */
      }
      if (value) mark_pack_casesensitive(pack);
      else mark_pack_caseinsensitive(pack);
    }
    if (boundp(STACK_0)) { /* check the case-invertedness: */
      var bool value = !nullp(STACK_0);
      if (!!pack_caseinvertedp(pack) != value) {
        pushSTACK(pack); pushSTACK(pack);
        STACK_1 = CLSTEXT("One should not change the case inversion of ~S.");
        funcall(S(warn),2);
        pack = STACK_4;         /* restore for GC-safety */
      }
      if (value) mark_pack_caseinverted(pack);
      else mark_pack_casepreserved(pack);
    }
    /* adjust the nicknames: */
    if (boundp(STACK_3)) {
      /* install nicknames with RENAME-PACKAGE: */
      pushSTACK(pack); /* pack */
      pushSTACK(ThePackage(pack)->pack_name); /* (package-name pack) */
      pushSTACK(STACK_(3+2)); /* nicknames */
      /* (RENAME-PACKAGE pack (package-name pack) nicknames) */
      funcall(L(rename_package),3);
    }
    /* adjust the use-list: */
    if (boundp(STACK_2)) {
      /* extend use-list with USE-PACKAGE
         and shorten with UNUSE-PACKAGE: */
      STACK_1 = STACK_2; /* use-list as 1. argument for USE-PACKAGE */
      STACK_0 = STACK_4; /* pack as 2. argument for USE-PACKAGE */
      prepare_use_package(); /* check arguments STACK_1, STACK_0 */
      /* stack-layout: pack, nicknames, -, new use-list, pack. */
      { /* execute USE-PACKAGE (with copied use-list): */
        var object temp = reverse(STACK_1);
        use_package(temp,STACK_4);
      }
      /* All packages, that are still listed in the use-list of pack,
         but which do not occur in the uselist located in STACK_1,
         are removed with unuse_1package: */
      pack = STACK_4;
      { /* traverse use-list of pack */
        STACK_0 = ThePackage(pack)->pack_use_list;
        while (mconsp(STACK_0)) {
          var object qpack = Car(STACK_0);
          /* search in uselist: */
          if (nullp(memq(qpack,STACK_1)))
            /* not found in uselist */
            unuse_1package(STACK_4,qpack);
          STACK_0 = Cdr(STACK_0);
        }
      }
    }
    /* the use-list is adjusted correctly. */
    skipSTACK(4); /* forget uselist, nicknames etc. */
    VALUES1(popSTACK());
  }
}

local one_sym_function_t delete_package_aux;
/* (DELETE-PACKAGE package), CLTL2 p. 265-266 */
LISPFUNN(delete_package,1) {
  var object pack = popSTACK();
  if (packagep(pack)) {
    if (pack_deletedp(pack)) {
      VALUES1(NIL); return; /* already deleted -> 1 value NIL */
    }
  } else if (stringp(pack))
  string: { /* string -> find package with this name: */
    var object found = find_package(pack);
    if (nullp(found)) {
      /* raise Continuable Error: */
      pushSTACK(NIL); /* "Ignore." */
      pushSTACK(S(package_error)); /* PACKAGE-ERROR */
      pushSTACK(S(Kpackage)); /* :PACKAGE */
      pushSTACK(pack); /* package-name */
      pushSTACK(NIL); /* "~S: A package with name ~S does not exist." */
      pushSTACK(S(delete_package));
      pushSTACK(pack);
      STACK_6 = CLSTEXT("Ignore.");
      STACK_2 = CLSTEXT("~S: There is no package with name ~S.");
      /* (SYS::CERROR-OF-TYPE "..." 'PACKAGE-ERROR :PACKAGE pack "..."
                              'DELETE-PACKAGE pack) */
      funcall(L(cerror_of_type),7);
      VALUES1(NIL);
      return;
    }
    pack = found;
  } else if (symbolp(pack)) { /* symbol -> string */
    pack = Symbol_name(pack); goto string; /* use printname, no case-invert */
  } else if (charp(pack)) { /* character -> string */
    var object new_string = allocate_string(1);
    TheSnstring(new_string)->data[0] = char_code(pack);
    pack = new_string;
    goto string;
  } else
    pack = test_package_arg(pack); /* report error */
  pushSTACK(pack);
  if (!nullp(ThePackage(pack)->pack_used_by_list)) {
    /* raise Continuable Error: */
    pushSTACK(NIL); /* "~*Delete ~S anyway." */
    pushSTACK(S(package_error)); /* PACKAGE-ERROR */
    pushSTACK(S(Kpackage)); /* :PACKAGE */
    pushSTACK(pack); /* package */
    pushSTACK(NIL); /* "~S: ~S is used by ~{~S~^, ~}." */
    pushSTACK(S(delete_package));
    pushSTACK(pack);
    pushSTACK(ThePackage(pack)->pack_used_by_list);
    STACK_7 = CLSTEXT("~*Delete ~S anyway.");
    STACK_3 = CLSTEXT("~S: ~S is used by ~{~S~^, ~}.");
    /* (SYS::CERROR-OF-TYPE "..." 'PACKAGE-ERROR :PACKAGE pack "..."
                            'DELETE-PACKAGE pack used-by-list) */
    funcall(L(cerror_of_type),8);
  }
  /* execute (DOLIST (p used-py-list) (UNUSE-PACKAGE pack p)) : */
  set_break_sem_3();
  while ((pack = STACK_0, mconsp(ThePackage(pack)->pack_used_by_list))) {
    unuse_1package(Car(ThePackage(pack)->pack_used_by_list),pack);
  }
  clr_break_sem_3();
  /* execute (UNUSE-PACKAGE (package-use-list pack) pack) : */
  unuse_package(ThePackage(STACK_0)->pack_use_list,pack);
  /* apply delete_package_aux to the symbols present in pack: */
  map_symtab_c(&delete_package_aux,&STACK_0,
               ThePackage(STACK_0)->pack_external_symbols);
  map_symtab_c(&delete_package_aux,&STACK_0,
               ThePackage(STACK_0)->pack_internal_symbols);
  pack = popSTACK();
  /* remove pack from the list of all packages and mark as deleted: */
  set_break_sem_2();
  O(all_packages) = deleteq(O(all_packages),pack);
  mark_pack_deleted(pack);
  clr_break_sem_2();
  VALUES1(T);
}

/* UP: Auxiliary function for DELETE-PACKAGE:
 Remove the argument (a present symbol) from pack.
 can trigger GC */
local maygc void delete_package_aux (void* data, object sym) {
  var gcv_object_t* localptr = (gcv_object_t*)data; /* pointer to pack */
  pushSTACK(sym); unintern(&STACK_0,localptr); skipSTACK(1);
}

/* (FIND-ALL-SYMBOLS name) and its case-inverted variant */
local maygc Values do_find_all_symbols (bool invert) {
  STACK_0 = test_stringsymchar_arg(STACK_0,invert); /* name as string */
  pushSTACK(NIL); /* (so far empty) symbol-list */
  pushSTACK(O(all_packages)); /* traverse list of all packages */
  while (mconsp(STACK_0)) {
    var object pack = Car(STACK_0); /* next package */
    /* search in its internal and external symbols: */
    var object sym;
    if (package_lookup(STACK_2,invert,pack,&sym,)) {
      /* found: symbol sym is present in package pack,
         cons with (pushnew sym STACK_1 :test #'eq) on the symbol-list:
         Search, if the found symbol sym occurs in STACK_1: */
      if (nullp(memq(sym,STACK_1))) { /* not found, must cons: */
        pushSTACK(sym);
        {
          var object new_cons = allocate_cons();
          Car(new_cons) = popSTACK();
          Cdr(new_cons) = STACK_1;
          STACK_1 = new_cons;
        }
      }
    }
    STACK_0 = Cdr(STACK_0);
  }
  skipSTACK(1);
  VALUES1(popSTACK()); /* symbol-list as value */
  skipSTACK(1);
}

/* (FIND-ALL-SYMBOLS name), CLTL p. 187 */
LISPFUNNR(find_all_symbols,1)
{
  do_find_all_symbols(false);
}

/* (CS-COMMON-LISP:FIND-ALL-SYMBOLS name) */
LISPFUNNR(cs_find_all_symbols,1)
{
  do_find_all_symbols(true);
}

local one_sym_function_t map_symbols_aux;
/* (SYSTEM::MAP-SYMBOLS fun pack)
 applies the function fun to all accessible symbols in pack. Value NIL. */
LISPFUNN(map_symbols,2) {
  /* check second argument: */
  STACK_0 = test_package_arg(STACK_0);
  /* apply fun to all internal symbols: */
  map_symtab(STACK_1,ThePackage(STACK_0)->pack_internal_symbols);
  /* apply fun to all external symbols: */
  map_symtab(STACK_1,ThePackage(STACK_0)->pack_external_symbols);
  /* apply fun to all inherited symbols: */
  pushSTACK(ThePackage(STACK_0)->pack_use_list); /* traverse use-list */
  while (mconsp(STACK_0)) {
    var object usedpack = Car(STACK_0); /* next package from the use-list */
    STACK_0 = Cdr(STACK_0);
    map_symtab_c(&map_symbols_aux,&STACK_1,
                 ThePackage(usedpack)->pack_external_symbols);
  }
  skipSTACK(3);
  VALUES1(NIL);
}

/* UP: Auxiliary function for map_symbols:
 Test, if the argument is not shadowed in the given package, and
 then apply the given function.
 can trigger GC */
local maygc void map_symbols_aux (void* data, object sym) {
  var gcv_object_t* localptr = (gcv_object_t*)data;
  /* Pointer to local variables of map_symbols:
     *(localptr STACKop 1) = fun,
     *(localptr STACKop 0) = pack.
     The symbol STACK_0 is shadowed, if and only if a different
     symbol of the same name is located in the
     shadowing-list of pack. */
  var object shadowingsym;
  if (!(shadowing_lookup(Symbol_name(sym),false,*(localptr STACKop 0),&shadowingsym)
        && !eq(shadowingsym,sym))) {
    pushSTACK(sym); funcall(*(localptr STACKop 1),1);
  } else {
    /* symbol is shadowed in pack -> do not call function */
  }
}

/* (SYSTEM::MAP-EXTERNAL-SYMBOLS fun pack)
   applies the function fun to all external symbols in pack. Value NIL. */
LISPFUNN(map_external_symbols,2) {
  /* check second argument: */
  var object pack = test_package_arg(popSTACK());
  /* apply fun to all external symbols: */
  map_symtab(popSTACK(),ThePackage(pack)->pack_external_symbols);
  VALUES1(NIL);
}

/* (SYSTEM::MAP-ALL-SYMBOLS fun)
 applies the function fun to all symbols present in any package. */
LISPFUNN(map_all_symbols,1) {
  pushSTACK(O(all_packages)); /* traverse package-list */
  while (mconsp(STACK_0)) {
    var object pack = Car(STACK_0); /* next package */
    STACK_0 = Cdr(STACK_0);
    pushSTACK(pack); /* save */
    /* apply fun to all internal symbols: */
    map_symtab(STACK_2,ThePackage(pack)->pack_internal_symbols);
    pack = popSTACK();
    /* apply fun to all external symbols: */
    map_symtab(STACK_1,ThePackage(pack)->pack_external_symbols);
  }
  skipSTACK(2);
  VALUES1(NIL);
}

/* UP: Subroutine for EXT:RE-EXPORT.
 Exports a single symbol from TO-PACK. */
local void export_symbol_from (void *data, object sym) {
  var gcv_object_t* pack_ = (gcv_object_t*)data; /* points into the STACK */
  pushSTACK(sym);
  export(&STACK_0,pack_);
  skipSTACK(1);
}

/* (EXT:RE-EXPORT "FROM-PACK" "TO-PACK")
 export all external symbols in FROM-PACK from TO-PACK */
LISPFUNN(re_export,2) {
  STACK_1 = test_package_arg(STACK_1); /* FROM-PACK */
  STACK_0 = test_package_arg(STACK_0); /* TO-PACK */
  /* TO-PACK must be already using FROM-PACK */
  var object pack_u_l = ThePackage(STACK_0)->pack_use_list;
  if (nullp(memq(STACK_1,ThePackage(STACK_0)->pack_use_list))) {
    pushSTACK(STACK_0); /* TO-PACK: PACKAGE slot of PACKAGE-ERROR */
    pushSTACK(STACK_2); /* FROM-PACK */
    pushSTACK(STACK_1); /* TO-PACK */
    pushSTACK(S(re_export));
    fehler(package_error,GETTEXT("~S: ~S is not using ~S"));
  }
  map_symtab_c(&export_symbol_from,&STACK_0,
               ThePackage(STACK_1)->pack_external_symbols);
  VALUES1(NIL);
  skipSTACK(2);
}

/* Auxiliary functions for WITH-PACKAGE-ITERATOR, CLtL2 p. 275, and LOOP:
 (SYSTEM::PACKAGE-ITERATOR package flags) returns an internal state
 for iterating through the package.
 (SYSTEM::PACKAGE-ITERATE internal-state) iterates through a package by
 one, thereby changes the internal-state and returns: three values
 T, symbol, accessibility of the next symbols resp. 1 value NIL at the end. */

LISPFUNN(package_iterator,2) {
  STACK_1 = test_package_arg(STACK_1); /* check package-argument */
  /* An internal state consists of a vector
     #(entry index symtab inh-packages package flags)
     whereby flags is a sub-list of (:INTERNAL :EXTERNAL :INHERITED) ,
         package is the original package,
         inh-packages is a sub-list of (package-use-list package) ,
         symtab is a symbol-table or NIL,
         index is an Index in symtab,
         entry is the rest of an entry in symtab. */
  var object state = allocate_vector(6);
  /* TheSvector(state)->data[2] = NIL; */ /* invalid */
  TheSvector(state)->data[3] = ThePackage(STACK_1)->pack_use_list;
  TheSvector(state)->data[4] = STACK_1;
  TheSvector(state)->data[5] = STACK_0;
  VALUES1(state); skipSTACK(2); /* state as value */
}

LISPFUNN(package_iterate,1) {
  var object state = popSTACK(); /* internal state */
  /* hopefully a 6er-vector */
  if (simple_vector_p(state) && (Svector_length(state) == 6)) {
    /* state = #(entry index symtab inh-packages package flags) */
    var object symtab = TheSvector(state)->data[2];
    if (simple_vector_p(symtab)) {
      if (false) {
      search1:
        TheSvector(state)->data[2] = symtab;
        TheSvector(state)->data[1] = Symtab_size(symtab);
        TheSvector(state)->data[0] = NIL;
      }
    search2:
      {
        var object entry = TheSvector(state)->data[0];
      search3:
        /* continue search within entry: */
        if (consp(entry)) {
          TheSvector(state)->data[0] = Cdr(entry);
          value2 = Car(entry); goto found;
        } else if (!nullp(entry)) {
          TheSvector(state)->data[0] = NIL;
          value2 = entry; goto found;
        }
        if (false) {
        found:
          /* Found a symbol value.
             Verify that is it accessible in pack and, if :INHERITED
             is requested,
             1. not hidden by a different symbol (which must be on the
                shadowing-list of pack),
             2. itself not already present in pack (because in this case
                the accessibility would be :INTERNAL or :EXTERNAL). */
          {
            var object shadowingsym;
            if (!(eq(Car(TheSvector(state)->data[5]),S(Kinherited))
                  && (shadowing_lookup(Symbol_name(value2),false,
                                       TheSvector(state)->data[4],
                                       &shadowingsym)
                      || symtab_find(value2,
                                     ThePackage(TheSvector(state)->data[4])->
                                     pack_internal_symbols)
                      || symtab_find(value2,
                                     ThePackage(TheSvector(state)->data[4])->
                                     pack_external_symbols)))) {
              /* Symbol value2 is really accessible. */
              value1 = T; value3 = Car(TheSvector(state)->data[5]);
              mv_count=3; return;
            }
            goto search2;
          }
        }
        /* entry became =NIL -> go to next Index */
        {
          var uintL index = posfixnum_to_V(TheSvector(state)->data[1]);
          if (index > 0) {
            TheSvector(state)->data[1] = fixnum_inc(TheSvector(state)->
                                                    data[1],-1);
            index--;
            /* check index as a precaution */
            entry = (index < (uintL)posfixnum_to_V(Symtab_size(symtab))
                     ? (object)TheSvector(Symtab_table(symtab))->data[index]
                     : NIL);
            goto search3;
          }
        }
      }
      /* index became =0 -> go to next table */
      if (eq(Car(TheSvector(state)->data[5]),S(Kinherited))) {
      search4:
        if (mconsp(TheSvector(state)->data[3])) {
          /* go to next element of the list inh-packages */
          symtab = ThePackage(Car(TheSvector(state)->data[3]))->
            pack_external_symbols;
          TheSvector(state)->data[3] = Cdr(TheSvector(state)->data[3]);
          goto search1;
        }
      }
    search5:
      /* go to next element of flags */
      TheSvector(state)->data[5] = Cdr(TheSvector(state)->data[5]);
    }
    var object flags = TheSvector(state)->data[5];
    if (consp(flags)) {
      var object flag = Car(flags);
      if (eq(flag,S(Kinternal))) { /* :INTERNAL */
        symtab = ThePackage(TheSvector(state)->data[4])->
          pack_internal_symbols;
        goto search1;
      } else if (eq(flag,S(Kexternal))) { /* :EXTERNAL */
        symtab = ThePackage(TheSvector(state)->data[4])->
          pack_external_symbols;
        goto search1;
      }
      else if (eq(flag,S(Kinherited))) { /* :INHERITED */
        goto search4;
      }
      goto search5; /* skip invalid flag */
    }
  }
  VALUES1(NIL); return;
}

/* UP: initialize the package list
 init_packages();
 can trigger GC */
global maygc void init_packages (void) {
  O(all_packages) = NIL; /* ALL_PACKAGES := NIL */
  { /* #<PACKAGE CS-COMMON-LISP-USER>: */
    pushSTACK(coerce_imm_ss(ascii_to_string("CS-COMMON-LISP-USER")));
    pushSTACK(coerce_imm_ss(ascii_to_string("CS-CL-USER")));
    /* Provide nickname "CS-USER" for similarity with package "COMMON-LISP-USER". */
    pushSTACK(coerce_imm_ss(ascii_to_string("CS-USER")));
    var object nicks = listof(2); /* ("CS-CL-USER" "CS-USER") */
    make_package(popSTACK(),nicks,true,true); /* "CS-COMMON-LISP-USER" */
  }
  { /* #<PACKAGE CS-COMMON-LISP>: */
    pushSTACK(coerce_imm_ss(ascii_to_string("CS-COMMON-LISP")));
    pushSTACK(coerce_imm_ss(ascii_to_string("CS-CL")));
    /* Provide nickname "CS-LISP" for similarity with package "COMMON-LISP". */
    pushSTACK(coerce_imm_ss(ascii_to_string("CS-LISP")));
    var object nicks = listof(2); /* ("CS-CL" "CS-LISP") */
    make_package(popSTACK(),nicks,true,true); /* "CS-COMMON-LISP" */
  }
  /* #<PACKAGE CHARSET>: */
  pushSTACK(coerce_imm_ss(ascii_to_string("CHARSET")));
  O(charset_package) = make_package(popSTACK(),NIL,false,false); /* "CHARSET",() */
  /* #<PACKAGE KEYWORD>: */
  pushSTACK(coerce_imm_ss(ascii_to_string("KEYWORD")));
  O(keyword_package) = make_package(popSTACK(),NIL,false,false); /* "KEYWORD" */
  { /* #<PACKAGE SYSTEM>: */
    pushSTACK(coerce_imm_ss(ascii_to_string("SYSTEM")));
    pushSTACK(coerce_imm_ss(ascii_to_string("COMPILER")));
    pushSTACK(coerce_imm_ss(ascii_to_string("SYS")));
    var object nicks = listof(2); /* ("COMPILER" "SYS") */
    make_package(popSTACK(),nicks,false,false); /* "SYSTEM" */
  }
  { /* #<PACKAGE COMMON-LISP-USER>: */
    pushSTACK(coerce_imm_ss(ascii_to_string("COMMON-LISP-USER")));
    pushSTACK(coerce_imm_ss(ascii_to_string("CL-USER")));
    pushSTACK(coerce_imm_ss(ascii_to_string("USER")));
    var object nicks = listof(2); /* ("CL-USER" "USER") */
    make_package(popSTACK(),nicks,false,false); /* "COMMON-LISP-USER" */
  }
  { /* #<PACKAGE COMMON-LISP>: */
    pushSTACK(coerce_imm_ss(ascii_to_string("COMMON-LISP")));
    pushSTACK(coerce_imm_ss(ascii_to_string("LISP")));
    pushSTACK(coerce_imm_ss(ascii_to_string("CL")));
    var object nicks = listof(2); /* ("LISP" "CL") */
    O(default_package) = make_package(popSTACK(),nicks,false,false); /* "COMMON-LISP" */
  }
  /* Created all basic packages.
     Now append all further packages to the end of O(all_packages). */
  nreverse(O(all_packages));
 #define LISPPACK  LISPPACK_B
  #include "constpack.c"
 #undef LISPPACK
  nreverse(O(all_packages));
}
