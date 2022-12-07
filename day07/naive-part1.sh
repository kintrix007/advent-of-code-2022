#!/bin/bash

# This would have worked if directories did not have a size
# I hate this. I absolutely despise that I could not just do this.
cut -f1 <(du -b -t -100000 fs) | paste -s -d+ | bc
