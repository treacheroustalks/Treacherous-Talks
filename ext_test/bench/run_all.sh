#!/bin/bash


# 1 PC:  ~ 72 req/sec
./concurrent.sh machines="1" concurrent="`seq 10 14`" mode="{rate, 6}"
#                                       = [60, 66, 72, 78, 84] req/sec

# 2 PCs: ~ 103 req/sec
./concurrent.sh machines="2" concurrent="`seq 15 19`" mode="{rate, 6}"
#                                       = [90, 96, 102, 108, 114] req/sec

# 3 PCs: ~ 168 req/sec
./concurrent.sh machines="3" concurrent="`seq 25 30`" mode="{rate, 6}"
#                                       = [150,156,162,168,174,180] req/sec

# 4 PCs: ~ 205 req/sec
./concurrent.sh machines="4" concurrent="`seq 32 36`" mode="{rate, 6}"
#                                       = [192, 198, 204, 210, 216] req/sec

# 5 PCs: ~ 208 req/sec
./concurrent.sh machines="5" concurrent="`seq 34 38`" mode="{rate, 6}"
#                                       = [204, 210, 216, 222, 228] req/sec

# 6 PCs: ~ 191 req/sec => o.0
./concurrent.sh machines="6" concurrent="`seq 32 36`" mode="{rate, 6}"
#                                       = [192, 198, 204, 210, 216] req/sec

