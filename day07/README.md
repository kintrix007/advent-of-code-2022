# Filesystem

First build the filesystem by running
```bash
./make-fs.sh
```

Then execute the solutions by running 
```bash
./part1.sh
```
```bash
./part2.sh
```

The file [`naive-part1.sh`](naive-part1.sh) has a really simple implementation for part 1, which could have worked if the directories did not take up 4K (or 4096B) space in the filesystem (ext4, Linux), which does not align with this day's problem... 
