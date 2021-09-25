/** RoseJSON: a simple, Yojson-like library for ReasonML */

type t = 
    | RoseAssoc(list((string, t)))
    | RoseArray(list(t))
    | RoseInt(int)
    | RoseFloat(float)
    | RoseNull
    | RoseBool(bool)
    | RoseString(string)

let member = (needle:string, src:t):t => {
    let rec member_assoc = (s:list((string, t))):t => {
        switch(s) {
            | [(name, data), ..._] when name == needle => data
            | [(_, _), ...x] => member_assoc(x)
            | [] => RoseNull
        }
    }
    switch(src) {
        | RoseAssoc(data) => member_assoc(data)
        | _ => RoseNull
    }
}

let rec stringify = (src:t): string => {
    switch(src) {
        | RoseInt(x) => x |> string_of_int
        | RoseFloat(x) => x |> string_of_float
        | RoseNull => "null"
        | RoseBool(x) => x |> string_of_bool
        | RoseString(x) => x
        | RoseArray(l) => "[" ++ String.concat(", ", List.map(stringify, l)) ++ "]"
        | RoseAssoc(a) => "{" ++ String.concat(", ", List.map(((x, y)) => "\"" ++ x ++ "\":" ++ stringify(y), a)) ++ "}"
    }
}

/*
 * I'll implement all of the Yojson functions, but
 * I probably should wrap them in a module...
 */

let typeof = fun
    | RoseInt(_) => "int"
    | RoseFloat(_) => "float"
    | RoseNull => "null"
    | RoseBool(_) => "bool"
    | RoseString(_) => "string"
    | RoseArray(_) => "array"
    | RoseAssoc(_) => "object"

let keys = fun
    | RoseAssoc(l) => List.map(fst, l)
    | _ => [""]

let values = fun
    | RoseAssoc(l) => List.map(snd, l)
    | _ => [RoseNull]

let index_option = (src:t, offset:int):option(t) => {
    switch(src) {
        | RoseArray(l) when offset >= 0 => Some(List.nth(l, offset))
        | RoseArray(l) when offset < 0 => Some(List.nth(l, List.length(l) - offset))
        | _ => None
        | exception Failure(_) => None
        | exception Invalid_argument(_) => None
    }
}

let index = (src:t, offset:int):t => {
    switch(src) {
        | RoseArray(l) when offset >= 0 => List.nth(l, offset)
        | RoseArray(l) when offset < 0 => List.nth(l, List.length(l) - offset)
        | _ => raise(Invalid_argument("RoseJSON.index may only be called on RoseJSON.RoseArray objects"))
    }
}

/* currently have to use `to_assoc` in the pipeline here, but it's not
 * terrible; may add this directly as well tho
 */
let contains_member = (mvalue, l):bool => {
    let f = (found, elem) => {found || fst(elem) == mvalue};
    List.fold_left(f, false, l);
}

let contains_member_optional = (mvalue, ol):bool => {
    let f = (found, elem) => {found || fst(elem) == mvalue};
    switch(ol) {
        | Some(l) => List.fold_left(f, false, l)
        | None => false
    }
}

let contains = (value, l):bool => {
    let f = (found, elem) => { found || elem == value};
    List.fold_left(f, false, l);
}

let contains_optional = (value, ol):bool => {
    let f = (found, elem) => { found || elem == value};
    switch(ol) {
        | Some(l) => List.fold_left(f, false, l);
        | None => false
    }
}
/*
 * I generated these with:
 *
 * [source, reasonml]
 * ----
 * let format_lambda = (x:string):string => {
 *     let capx = String.capitalize_ascii(x)
 *     Printf.sprintf({|
 * let to_%s = fun
 *     | Rose%s(tmp) => tmp
 *     | _ => raise(Invalid_argument("expected type of Rose%s"))
 *
 * let to_%s_optional = fun
 *    | Rose%s(tmp) => Some(tmp)
 *    | _ => None
 * |}, x, capx, capx, x, capx)
 * }
 * ;
 * let types = [|"int", "float", "assoc", "bool", "array", "string"|];
 * Array.iter((x) => {print_endline(format_lambda(x))}, types);
 */

let to_int = fun
    | RoseInt(tmp) => tmp
    | _ => raise(Invalid_argument("expected type of RoseInt"))

let to_int_optional = fun
    | RoseInt(tmp) => Some(tmp)
    | _ => None

let to_float = fun
    | RoseFloat(tmp) => tmp
    | _ => raise(Invalid_argument("expected type of RoseFloat"))

let to_float_optional = fun
    | RoseFloat(tmp) => Some(tmp)
    | _ => None

let to_string = fun
    | RoseString(tmp) => tmp
    | _ => raise(Invalid_argument("expected type of RoseString"))

let to_string_optional = fun
    | RoseString(tmp) => Some(tmp)
    | _ => None

let to_bool = fun
    | RoseBool(tmp) => tmp
    | _ => raise(Invalid_argument("expected type of RoseBool"))

let to_bool_optional = fun
    | RoseBool(tmp) => Some(tmp)
    | _ => None

let to_assoc = fun
    | RoseAssoc(tmp) => tmp
    | _ => raise(Invalid_argument("expected type of RoseAssoc"))

let to_assoc_optional = fun
    | RoseAssoc(tmp) => Some(tmp)
    | _ => None

let to_array = fun
    | RoseArray(tmp) => tmp
    | _ => raise(Invalid_argument("expected type of RoseArray"))

let to_array_optional = fun
    | RoseArray(tmp) => Some(tmp)
    | _ => None

/*
 * Example of the above:
 *
 * [source,reasonml]
 * ----
 * let jsond = RoseAssoc([("test", RoseInt(1)), ("this", RoseInt(2))]);
 * member("test", jsond) |> stringify; // <1>
 * ----
 * <1>: will print `+- : string = "1"+`
 *
 * This is basically the "safe" module below, I would like to make a
 * "basic" version that throws exceptions, but here we basically default
 * to "null" for anything that we can't parse...
 */

type lex_state = 
    | LexInt(string, int, int)
    | LexFloat(string, int, int)
    | LexNullLiteral(int, int)
    | LexNull
    | LexEndOfLine
    | LexTrueLiteral(int, int)
    | LexFalseLiteral(int, int)
    | LexString(string, int, int)
    | LexArrayStart(int, int)
    | LexArrayEnd(int, int)
    /*
     * used to roll up the lexemes that make up a
     * list into one *lexer level* array object,
     * which we can just do one last map over
     * to turn into an actual RoseArray. The
     * same idea goes for LexObject below
     */
    | LexArray(list(lex_state), int, int)
    | LexComma(int, int)
    | LexColon(int, int)
    | LexObjectStart(int, int)
    | LexObjectEnd(int, int)
    | LexObject(list((string, lex_state)), int, int)
    | LexError(char, int)
    | LexBadIdent(string, int, int)

let is_numeric = (c:char):bool => {
    let r = Char.compare('0', c);
    r >= -9 && r <= 0
}

let is_whitespace = (c:char):bool => {
    Char.compare(c, ' ') == 0 || Char.compare(c, '\t') == 0 || Char.compare(c, '\n') == 0 || Char.compare(c, '\r') == 0
}

let is_bracket = (c:char):bool => {
    Char.compare(c, '{') == 0 || Char.compare(c, '}') == 0 || Char.compare(c, '[') == 0 || Char.compare(c, ']') == 0
}

let is_break = (c:char):bool => {
    Char.compare(c, ',') == 0 || Char.compare(c, ':') == 0 || is_whitespace(c) || is_bracket(c)
}

/*
 * this is really forgiving, and needs some work...
 * roughly works tho, can lex integers & floats, strings
 * work as well, but they they throw exceptions...
 */
let rec take_while_numeric = (src:string, start:int, offset:int, is_float:bool): lex_state => {
    switch(String.get(src, offset)) {
        | n when is_numeric(n) => take_while_numeric(src, start, offset + 1, is_float)
        | '.' when is_float => LexNull
        | '.' => take_while_numeric(src, start, offset + 1, true)
        | 'E' => take_while_numeric(src, start, offset + 1, true)
        | 'e' => take_while_numeric(src, start, offset + 1, true)
        | _ => if(is_float) {
            LexFloat(String.sub(src, start, offset - start), start, offset)
        } else {
            LexInt(String.sub(src, start, offset - start), start, offset)
        }
    }
}

let rec take_while_string = (src:string, start:int, offset:int, skip_escape:bool): lex_state => {
    switch(String.get(src, offset)) {
        | _ when skip_escape => take_while_string(src, start, offset + 1, false)
        | n when n == '"' => LexString(String.sub(src, start + 1, offset - start - 1), start, offset + 1)
        | e when e == '\\' => take_while_string(src, start, offset + 1, true)
        | _ => take_while_string(src, start, offset + 1, false)
    }
}

let rec take_until_not_white = (src:string, offset:int):int => {
    switch(String.get(src, offset)) {
        | n when is_whitespace(n) => take_until_not_white(src, offset + 1)
        | _ => offset
    }
}

/*
 * A somewhat basic state machine for parsing literals in JSON;
 * I could probably hide some of the details in an inner lambda,
 * but this works nicely enough. Additionally, below in the
 * `next` lambda, I could have started the initial state off
 * into this one, but didn't so as to not expose that leak and
 * make the offsets easier to calculate. However, that is
 * fully supported here
 */
let rec take_ident = (~state:int=0, src:string, start:int, offset:int): lex_state => {
    switch(String.get(src, offset)) {
        | 'n' when state == 0 => take_ident(~state = 1, src, start, offset + 1)
        | 't' when state == 0 => take_ident(~state = 5, src, start, offset + 1)
        | 'f' when state == 0 => take_ident(~state = 9, src, start, offset + 1)
        | 'u' when state == 1 => take_ident(~state = 2, src, start, offset + 1)
        | 'l' when state == 2 => take_ident(~state = 3, src, start, offset + 1)
        | 'l' when state == 3 => take_ident(~state = 4, src, start, offset + 1)
        | c when state == 4 && is_break(c) => LexNullLiteral(start, offset)
        | 'r' when state == 5 => take_ident(~state = 6, src, start, offset + 1)
        | 'u' when state == 6 => take_ident(~state = 7, src, start, offset + 1)
        | 'e' when state == 7 => take_ident(~state = 8, src, start, offset + 1)
        | d when state == 8 && is_break(d) => LexTrueLiteral(start, offset)
        | 'a' when state == 9 => take_ident(~state = 10, src, start, offset + 1)
        | 'l' when state == 10 => take_ident(~state = 11, src, start, offset + 1)
        | 's' when state == 11 => take_ident(~state = 12, src, start, offset + 1)
        | 'e' when state == 12 => take_ident(~state = 13, src, start, offset + 1)
        | e when state == 13 && is_break(e) => LexFalseLiteral(start, offset)
        | _ => LexError(String.get(src, offset), offset)
    }
}

let rec next = (src:string, offset:int):lex_state => {
    switch(String.get(src, offset)) {
        | '"' => take_while_string(src, offset, offset + 1, false)
        | n when is_numeric(n) => take_while_numeric(src, offset, offset, false)
        | s when is_whitespace(s) => next(src, take_until_not_white(src, offset))
        | ']' => LexArrayEnd(offset, offset + 1)
        | '}' => LexObjectEnd(offset, offset + 1)
        | '[' => LexArrayStart(offset, offset + 1)
        | '{' => LexObjectStart(offset, offset + 1)
        | ',' => LexComma(offset, offset + 1)
        | ':' => LexColon(offset, offset + 1)
        | 'n' => take_ident(src, offset, offset)
        | 'f' => take_ident(src, offset, offset)
        | 't' => take_ident(src, offset, offset)
        | _ => LexError(String.get(src, offset), offset)
    }
}

/*
 * NOTE: I exposed these so that I can test them from rtop,
 * should either move them back under from_string or add them
 * to a private lexing utils module.
 *
 * these inner functions are meant to hold state without
 * letting too much leak outside of `from_string`; originally
 * I was going to just use a start parameter and what not, but
 * that becomes too leaky too fast (although I will leave the
 * `start` parameter so folks can use that).
 *
 * I'm currently using `lex_state` to act like the lexer state
 * buffer in Yojson proper, without maintaining current stream
 * pointer separately, but that might get messy below. Eventually
 * I probably want to switch that over...
 */
let rec read_array = (src:string, start:int):(int, lex_state) => {
    let res = ref([])
    let cont = ref(true)
    let offset = ref(start)
    while(cont^) {
        switch(read_json(src, offset^)) {
            | (n, LexArrayEnd(_, _)) => {
                cont := false
                offset := n
            }
            /*
             * NOTE: *gah* you can't use `as` to
             * select portions of a tuple apparently?
             * the documentation says you can, but it's
             * a syntax error in rtop
             */
            | (n, LexError(c, e)) => {
                cont := false
                res := [LexError(c, e)]
                offset := n
            }
            /*
             * NOTE: this isn't particularly strict in
             * enforcing the order of commas, we're just
             * handling them. Would be a good place
             * to refactor correctly
             */
            | (n, LexComma(_, _)) => {
                offset := n
            }
            | (n, data) => {
                res := List.append(res^, [data])
                offset := n
            }
        }
    };
    (offset^, LexArray(res^, start, offset^))
} and read_object = (src:string, start:int):(int, lex_state) => {
    let cont = ref(true)
    let rc = ref(LexNull)
    let name = ref("")
    let obj = ref(LexNull)
    let res = ref([])
    let offset = ref(start)
    /*
     * this could be really nice with a state machine here too...
     */
    while(cont^) {
        rc := next(src, offset^)
        switch(rc^) {
            | LexString(n, _, endoffset) => {
                offset := endoffset
                name := n
                rc := next(src, offset^)
                switch(rc^) {
                    | LexColon(_, endoffset) => {
                        let tmp = read_json(src, endoffset)
                        offset := fst(tmp)
                        obj := snd(tmp)
                        res := List.append(res^, [(name^, obj^)])
                        rc := next(src, offset^)
                        switch(rc^) {
                            | LexComma(_, n) => { offset := n }
                            | LexObjectEnd(_, n) => {
                                offset := n
                                cont := false
                            }
                            | _ => {
                                cont := false
                            }
                        }
                    }
                    | _ => {
                        cont := false
                    }
                }
            }
            | LexObjectEnd(_, endoffset) => {
                offset := endoffset
                cont := false
            }
            | _ => {
                cont := false
            }
        }
    };
    (offset^, LexObject(res^, start, offset^))
} and read_json = (src:string, start:int):(int, lex_state) => {
    switch(next(src, start)) {
        /*
         * NOTE: this makes me realize that having
         * universal accessors for ADTs would be
         * really helpful in some situations,
         * such as catching the offset of all
         * types we *don't* care about, and only
         * needing to match on those we do...
         * good idea for carML here
         *
         * match(next(src, start)) with
         *     Error => (start, Error)
         *     d => ((_offset d), d)
         * end
         */
        | LexInt(_, _, offset) as l => (offset, l)
        | LexFloat(_, _, offset) as l => (offset, l)
        | LexTrueLiteral(_, offset) as l => (offset, l)
        | LexFalseLiteral(_, offset) as l => (offset, l)
        | LexNullLiteral(_, offset) as l => (offset, l)
        | LexString(_, _, offset) as l => (offset, l)
        | LexArrayEnd(_, offset) as l => (offset, l)
        | LexArrayStart(_, offset) => read_array(src, offset)
        | LexObjectEnd(_, offset) as l => (offset, l)
        | LexObjectStart(_, offset) => read_object(src, offset)
        | LexError(_, offset) as l => (offset, l)
        | LexComma(_, offset) as l => (offset, l)
        | LexColon(_, offset) as l => (offset, l)
        | data => (start, data)
    }
} and roll_up_object = (objsrc:(string, lex_state)):(string, t) => {
    (fst(objsrc), roll_up_json(snd(objsrc)))
} and roll_up_json = (lexsrc:lex_state):t => {
    switch(lexsrc) {
        | LexInt(n, _, _) => RoseInt(int_of_string(n))
        | LexFloat(f, _, _) => RoseFloat(float_of_string(f))
        | LexTrueLiteral(_, _) => RoseBool(true)
        | LexFalseLiteral(_, _) => RoseBool(false)
        | LexNullLiteral(_, _) => RoseNull
        | LexString(s, _, _) => RoseString(s)
        | LexArray(l, _, _) => RoseArray(List.map(roll_up_json, l))
        | LexObject(o, _, _) => RoseAssoc(List.map(roll_up_object, o))
        | _ => RoseNull
    }
}

let from_string = (~start:int=0, src:string):t => {
    switch(next(src, start)) {
        /*
         * I don't know why, but both bsc and rtop are giving
         * me errors with the constructors in pipelines here...
         */
        | LexInt(n, _, _) => RoseInt(int_of_string(n))
        | LexFloat(f, _, _) => RoseFloat(float_of_string(f))
        | LexTrueLiteral(_, _) => RoseBool(true)
        | LexFalseLiteral(_, _) => RoseBool(false)
        | LexNullLiteral(_, _) => RoseNull
        | LexString(s, _, _) => RoseString(s)
        | LexNull => RoseNull
        | LexArrayStart(_, offset) => roll_up_json(snd(read_array(src, offset)))
        | LexObjectStart(_, offset) => roll_up_json(snd(read_object(src, offset)))
        | _ => RoseNull
    }
}
