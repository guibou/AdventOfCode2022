import subprocess
import re

p = subprocess.run(["cabal", "run", "alltests", "--", "-m", "works"], capture_output=True, encoding="utf8")

for line in re.sub(r"Day(..).*?Finished", "\\1", p.stdout, flags=re.S).split('\n'):
    if "seconds" in line:
        print(line)
