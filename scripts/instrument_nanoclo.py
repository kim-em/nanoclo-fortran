"""Create a pinned Rust baseline with all 26 counters emitted per declaration.

Only reporting changes: checker control flow and counter updates remain intact.
The original checkout is read-only. The exact instrumentation patch is saved.
"""
import argparse
from pathlib import Path
import subprocess
import tarfile
import io
import difflib

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('source', type=Path)
p.add_argument('destination', type=Path)
args = p.parse_args()
revision = '4cdd12f6283ee236afc653b353566d5df59d36fe'
if args.destination.exists():
    raise SystemExit('destination already exists')
archive = subprocess.check_output(['git', '-C', str(args.source), 'archive', revision])
args.destination.mkdir(parents=True)
with tarfile.open(fileobj=io.BytesIO(archive)) as tar:
    tar.extractall(args.destination, filter='data')
path = args.destination / 'src/tc.rs'
old = path.read_text()
anchor = '\n                            ctx.rp.flush_ctrs();'
assert old.count(anchor) == 1
report = '''                            if std::env::var("NANOCLO_DECLCTRS_ALL").is_ok() {
                                let name = self.with_ctx(|cc| format!("{:?}", cc.debug_print(declar.info().name)));
                                eprint!("DC26\\t{}\\t{}", i, name);
                                for count in ctx.rp.ctrs.iter() {
                                    eprint!("\\t{}", count);
                                }
                                eprintln!();
                            }
'''
new = old.replace(anchor, '\n' + report + anchor)
path.write_text(new)
patch = ''.join(difflib.unified_diff(old.splitlines(True), new.splitlines(True),
                                     fromfile='a/src/tc.rs', tofile='b/src/tc.rs'))
(args.destination / 'counters.patch').write_text(patch)
(args.destination / 'PINNED_REVISION').write_text(revision + '\n')
print(args.destination)
