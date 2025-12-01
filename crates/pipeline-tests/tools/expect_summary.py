#!/usr/bin/env python3
import argparse
import pathlib
import re
from collections import Counter, defaultdict

STAGES = ("lex", "parse", "sema")
STAGE_W = max(len(s) for s in STAGES)

TAG_MAP = {
    "ok": "ok",
    "pass": "ok",
    "diag": "diag",
    "diagnostic": "diag",
    "todo": "todo",
    "panic": "panic",
    "fail": "panic",
    "skip": "skip",
}


DIR_RE = re.compile(r'^\s*//\s*(lex|parse|sema|analy[sz]e)\s*:\s*([A-Za-z]+)\s*$', re.I)

def parse_expectations(path: pathlib.Path, head_lines: int = 16):
    lex = parse = sema = None
    try:
        with path.open("r", encoding="utf-8") as f:
            for i, line in enumerate(f):
                if i >= head_lines:
                    break
                m = DIR_RE.match(line)
                if not m:
                    continue
                stage = m.group(1).lower()
                if stage.startswith("analy"):
                    stage = "sema"
                tag = TAG_MAP.get(m.group(2).lower())
                if tag is None:
                    continue
                if stage == "lex":   lex = tag
                if stage == "parse": parse = tag
                if stage == "sema":  sema = tag
    except Exception:
        pass
    return {"lex": lex, "parse": parse, "sema": sema}

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("root", nargs="?", default="tests/snippets",
                    help="root directory to scan (default: tests/snippets)")
    ap.add_argument("--summary-only", action="store_true",
                    help="only print totals per stage/tag")
    args = ap.parse_args()

    root = pathlib.Path(args.root)
    files = sorted(root.rglob("*.qasm"))
    if not files:
        print(f"No .qasm under {root}")
        return 1

    # Counters per stage
    counts = {st: Counter() for st in STAGES}

    rows = []
    for p in files:
        exp = parse_expectations(p)
        rows.append((p.relative_to(root), exp))
        for st in STAGES:
            counts[st][exp[st] or "unset"] += 1

    if not args.summary_only:
        # Column widths
        w_path = max(len(str(r[0])) for r in rows + [(pathlib.Path("path"), {"lex":"","parse":"","sema":""})])
        print(f"{'path'.ljust(w_path)}  lex   parse sema")
        for rel, exp in rows:
            print(f"{str(rel).ljust(w_path)}  {exp['lex'] or '-':<5} {exp['parse'] or '-':<5} {exp['sema'] or '-':<5}")

    print("\nTotals:")
    order = ["ok", "diag", "todo", "panic", "skip", "unset"]
    tag_w = max(len(k) for k in order)
    for st in STAGES:
        c = counts[st]
        # Print in a fixed order; include unset if present
#        parts = [f"{k}={c[k]}" for k in order if c[k] > 0]
        parts = [f"{k}={c[k]:<3}" for k in order if c[k] > 0]
        total = sum(c.values())
        print(f"- {st:<{STAGE_W}}: total={total:<3}; " + ", ".join(parts))

    return 0

if __name__ == "__main__":
    raise SystemExit(main())
