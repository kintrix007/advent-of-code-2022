main :-
    open('input', read, FileStream),
    read_lines(FileStream, CharcodeLines),
    close(FileStream), !,
    maplist(convert_to_values, CharcodeLines, Lines),
    
    % forall(member(X, Lines), writeln(X)),

    part1(Lines, Part1),
    part2(Lines, Part2),

    write('Part 1: '), writeln(Part1),
    write('Part 2: '), writeln(Part2), !.

%!  part1(+Lines:list(list(Int)), -Result: Int) is deterministic.
part1(Lines, Result) :-
    maplist(part1_line, Lines, Duplicates),
    sum_list(Duplicates, Result), !.
part1(_, -1).

%!  part1_line(+Values:list(Int), -Result:Int) is semideterministic
part1_line(Values, Result) :-
    length(Values, Len),
    Middle is Len/2,
    split_at(Middle, Values, First, Second),
    % write(First), write(' | '), writeln(Second),
    member(Result, First), member(Result, Second), !.

%!  part2(+Lines:list(list(Int)), -Result: Int) is deterministic.
part2(Lines, Result) :-
    groups_of(3, Lines, GroupsOf3),
    % forall(member(X, GroupsOf3), (length(X, L), write(L), write(' - '), writeln(X))),
    maplist(part2_group, GroupsOf3, Badges),
    sum_list(Badges, Result), !.
part2(_, -1).

%!  part2_group(+GroupOf3:list(list(Int)), -Result:Int) is semideterministic.
part2_group(GroupOf3, Result) :-
    [First, Second, Third | []] = GroupOf3,
    member(Result, First), member(Result, Second), member(Result, Third), !.

groups_of(N, List, Groups) :- length(List, Len), Len < N, !, Groups = List.
groups_of(N, List, Groups) :-
    split_at(N, List, Init, Tail),
    Groups = [Init|Rest],
    groups_of(N, Tail, Rest).

%!  split_at(+N:Int, +List:list, -Init:list, -Tail:list) is semideterministic.
split_at(N, List, Init, Tail) :- N =< 0, !, N =:= 0, Init = [], Tail = List.
split_at(N, [H|Rest], Init, Tail) :-
    Rem is N-1,
    Init = [H|Following],
    split_at(Rem, Rest, Following, Tail).

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
