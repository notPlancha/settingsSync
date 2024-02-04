from expression import pipe             # functional programming
from expression.collections import seq
from icecream import ic                 # debugging
import sys
from warnings import warn
import subprocess                       # to run git commands
import click                            # to get user input
from pyprojroot.here import here        # to get the root of the project
# TODO make it not change if R didn't change from origin/main 
# Or if last commit was a version change
desc = here("DESCRIPTION")
if not ("--debug" in sys.argv or "-d" in sys.argv): ic.disable()
# input just to make sure
"""
try:
  result = click.confirm("Do you want to up the dev version?", default=True)
  if not result:
    sys.exit(0)
except click.Abort:
  sys.exit(1)
"""
version = None
with open(desc, 'r') as f:
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

version = pipe(
  dev,
  int,
  lambda x: x + 1,
  str,
  lambda x: f"{not_dev}-{x}"
)
ic(version)
lines[version_line] = ic(f"Version: {version}\n")

with open(desc, 'w') as f:
  f.writelines(lines)
if "--dry-run" in sys.argv:
  sys.exit(0)
subprocess.run(["git", "add", "DESCRIPTION"])
subprocess.run(["git", "commit", "-m", f"Version {version}"])
click.echo(f"Version {version} committed")