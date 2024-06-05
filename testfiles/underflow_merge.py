""" 
File tests_<operation>_before.txt contains vectors with tininess detection BEFORE rounding
File tests_<operation>_after.txt contains vectors with tininess detection AFTER rounding

Example: python3 underflow_merge.py add32
"""

import sys

test_file = sys.argv[1]
def to_bin(hex_s):
  return "{0:08b}".format(int(hex_s, 16)) 


def add_underflow_before(hex_s):
  bin_s = to_bin(hex_s)
  tmp = list(bin_s)
  tmp[2] = "1"
  bin_result = "".join(tmp)
  return "{0:02x}".format(int(bin_result, 2))

def get_flags(test):
  parts = test.split(" ")
  return parts[-1]

def has_underflow(flags):
  bin_s = to_bin(flags)
  return bin_s[6] == "1"


with open(f"tests_{test_file}_before.txt", "r") as f:
  lines = list(map(str.strip, f.readlines()))
  uflow_index = []
  for i, line in enumerate(lines):
    flags = get_flags(line)
    if has_underflow(flags):
      uflow_index.append(i)

with open(f"tests_{test_file}_after.txt", "r") as f:
  with open(f"tests_{test_file}.txt", "w") as wf:
    lines = f.readlines()
    for i, line in enumerate(lines):
      data = line.split(" ")
      flags = data[-1]
      if i in uflow_index:
        data[-1] = add_underflow_before(flags) + "\n"
      wf.write(" ".join(data))