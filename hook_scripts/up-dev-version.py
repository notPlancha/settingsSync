from expression import pipe
from expression.collections import seq
from icecream import ic
import sys
from warnings import warn
import subprocess
# ic.disable()
version = None
with open('DESCRIPTION', 'r') as f:
  lines = f.readlines() # to later write back to file
  for line_number, line in enumerate(lines):
    if 'Version:' in line:
      version = ic(line.split(":")[1])
      version_line = ic(line_number)
      break
if version is None:
  raise ValueError("Version not found in DESCRIPTION file")

try:
  not_dev, dev = pipe(version.split("-"), seq.map(lambda x: x.strip()))
except ValueError:
  warn("Version not found in DESCRIPTION file, assuming release version")
  sys.exit(0)
ic(not_dev, dev)
print("Upping dev version before pushing")
version = pipe(
  dev,
  int,
  lambda x: x + 1,
  str,
  lambda x: f"{not_dev}-{x}"
)
ic(version)
lines[version_line] = ic(f"Version: {version}\n")

with open('DESCRIPTION', 'w') as f:
  f.writelines(lines)

subprocess.run(["git", "add", "DESCRIPTION"])
subprocess.run(["git", "commit", "-m", f"Version {version}"])