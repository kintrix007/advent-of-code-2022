namespace SignalProcessing {
    int main(string[] args) {
        var file = GLib.FileStream.open("input", "r");
        var cont = file.read_line();
        
        print(@"Part 1: $(part1(cont))\n");
        print(@"Part 2: $(part2(cont))\n");

        return 0;
    }

    int part1(string cont) {
        var prev = new Gee.ArrayQueue<char>();
        int i;
        for (i = 0; i < cont.length; i++) {
            var ch = cont[i];
            prev.add(ch);

            if (prev.size < 4) continue;
            if (prev.size > 4) prev.poll();

            var start_found = prev
                .all_match(x => count(prev, x) == 1);
            if (start_found) break;
        }

        return i+1;
    }

    int part2(string cont) {
        var prev = new Gee.ArrayQueue<char>();
        int i;
        for (i = 0; i < cont.length; i++) {
            var ch = cont[i];
            prev.add(ch);

            if (prev.size < 14) continue;
            if (prev.size > 14) prev.poll();

            var start_found = prev
                .all_match(x => count(prev, x) == 1);
            if (start_found) break;
        }

        return i + 1;
    }

    int count<T>(Gee.Iterable<T> iter, T elem) {
        int total = 0;
        iter.foreach(x => {
            if (elem == x) total++;
            return true;
        });
        return total;
    }
}
