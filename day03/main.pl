main :-
    open('input', read, FileStream),
    read_lines(FileStream, CharcodeLines),
    close(FileStream), !,
    writeln('---'),
    maplist(convert_to_values, CharcodeLines, ValueLines),
    
    % forall(member(X, ValueLines), writeln(X)),

    part1(ValueLines, Part1),
    part2(ValueLines, Part2),

    write('Part 1: '), writeln(Part1),
    write('Part 2: '), writeln(Part2).

%!  part1(+ValueLines:list(list(Int)), Result: Int) is deterministic.
part1(ValueLines, Result) :- Result = 'None'.

%!  part2(+ValueLines:list(list(Int)), Result: Int) is deterministic.
part2(ValueLines, Result) :- Result = 'None'.

%!  read_lines(+Stream:stream, -Lines:list(list(Int)))) is deterministic.
read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [H|T]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, LineStr),
    string_codes(LineStr, H),
    read_lines(Stream, T).

%!  convert_to_values(+String:list(Int), -Values:list(Int)) is semideterministic.
convert_to_values(String, Values) :-
    maplist(convert_charcode_to_value, String, Values).

%!  convert_charcode_to_value(+Charcode:Int, -Value:Int) is semitdeterministic.
convert_charcode_to_value(Charcode, Value) :-
    char_code('a', CharA), char_code('z', CharZ),
    (Charcode >= CharA, Charcode =< CharZ) -> (Value is Charcode-CharA+1, !) ; fail.
convert_charcode_to_value(Charcode, Value) :-
    char_code('A', CharA), char_code('Z', CharZ),
    (Charcode >= CharA, Charcode =< CharZ) -> (Value is Charcode-CharA+27, !) ; fail.
