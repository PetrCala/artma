#!/usr/bin/env python3
"""Watch artma's r-universe checks and keep a GitHub issue in sync with them.

Fetches https://petrcala.r-universe.dev/api/packages/artma (unauthenticated,
latest build only), reads the check matrix, and:

- when any config reports ERROR or WARNING, files an issue labelled
  'r-universe-failure' listing the failing configs and their build logs,
  unless an open issue already covers the exact same failure set (matched
  via a fingerprint embedded in the issue body as an HTML comment);
- when a new failure set replaces an older one, closes the superseded issue;
- when everything is green, closes any open 'r-universe-failure' issues.

The wasm build reports a null check status; null is not a failure.

Environment:
  GH_TOKEN / GITHUB_TOKEN  token for the gh CLI (required unless dry run)
  GITHUB_REPOSITORY        owner/repo to file issues in (default PetrCala/artma)
  GITHUB_OUTPUT            when set, receives 'issue_number' (empty when no
                           new issue was filed) for downstream jobs
  DRY_RUN=1 (or --dry-run) print the planned gh calls instead of running them
"""

import datetime
import json
import os
import re
import subprocess
import sys
import urllib.request

API_URL = "https://petrcala.r-universe.dev/api/packages/artma"
DASHBOARD_URL = "https://petrcala.r-universe.dev/artma"
LABEL = "r-universe-failure"
FINGERPRINT_RE = re.compile(r"<!-- r-universe-monitor-fingerprint: (.*?) -->")
FAIL_STATUSES = {"ERROR", "WARNING"}


def repo():
    return os.environ.get("GITHUB_REPOSITORY", "PetrCala/artma")


def fetch_api():
    request = urllib.request.Request(API_URL, headers={"Accept": "application/json"})
    with urllib.request.urlopen(request, timeout=60) as response:
        return json.load(response)


def check_status(entry):
    return (entry.get("check") or "").upper()


def failing_jobs(data):
    return [job for job in data.get("_jobs") or [] if check_status(job) in FAIL_STATUSES]


def build_urls(data):
    urls = []
    for binary in data.get("_binaries") or []:
        url = binary.get("buildurl")
        if url and url not in urls:
            urls.append(url)
    return urls


def fingerprint(version, failures):
    pairs = sorted(f"{job.get('config')}:{check_status(job)}" for job in failures)
    return f"{version}|{','.join(pairs)}"


def gh(args, dry_run):
    cmd = ["gh"] + args
    if dry_run:
        printable = " ".join(arg if len(arg) < 80 else arg[:77] + "..." for arg in cmd)
        print(f"[dry run] would run: {printable}")
        return ""
    result = subprocess.run(cmd, check=True, capture_output=True, text=True)
    return (result.stdout or "").strip()


def open_issues(dry_run):
    if dry_run:
        print(f"[dry run] would list open issues labelled '{LABEL}'")
        return []
    out = gh(
        [
            "issue", "list", "--repo", repo(), "--label", LABEL,
            "--state", "open", "--json", "number,body",
        ],
        dry_run,
    )
    return json.loads(out or "[]")


def issue_fingerprints(issue):
    return FINGERPRINT_RE.findall(issue.get("body") or "")


def issue_body(version, failures, urls, fp):
    run_url = urls[0] if len(urls) == 1 else None
    lines = [
        f"The latest r-universe build of artma {version} reports failing checks.",
        "",
        "| config | R | check | logs |",
        "| --- | --- | --- | --- |",
    ]
    for job in sorted(failures, key=lambda job: job.get("config") or ""):
        if run_url and job.get("job"):
            logs = f"[job log]({run_url}/job/{job['job']})"
        else:
            logs = ""
        lines.append(
            f"| {job.get('config')} | {job.get('r')} | {check_status(job)} | {logs} |"
        )
    lines.append("")
    for url in urls:
        lines.append(f"Build run: {url}")
    lines.append(f"Dashboard: {DASHBOARD_URL}")
    lines.append("")
    lines.append(
        "r-universe rebuilds from master, so merging a fix to master is the release."
    )
    lines.append("")
    lines.append(f"<!-- r-universe-monitor-fingerprint: {fp} -->")
    return "\n".join(lines)


def set_output(name, value):
    path = os.environ.get("GITHUB_OUTPUT")
    if path:
        with open(path, "a", encoding="utf-8") as handle:
            handle.write(f"{name}={value}\n")
    else:
        print(f"[output] {name}={value}")


def close_issue(issue, comment, dry_run):
    number = str(issue["number"])
    gh(["issue", "comment", number, "--repo", repo(), "--body", comment], dry_run)
    gh(["issue", "close", number, "--repo", repo()], dry_run)


def main():
    dry_run = "--dry-run" in sys.argv[1:] or os.environ.get("DRY_RUN") == "1"
    today = datetime.date.today().isoformat()

    data = fetch_api()
    version = data.get("Version", "unknown")
    failures = failing_jobs(data)
    issues = open_issues(dry_run)

    if not failures:
        print(f"All r-universe checks OK for artma {version}.")
        for issue in issues:
            print(f"Closing stale issue #{issue['number']}.")
            close_issue(
                issue,
                f"r-universe checks are green again as of artma {version} ({today}). Closing.",
                dry_run,
            )
        set_output("issue_number", "")
        return

    fp = fingerprint(version, failures)
    configs = ", ".join(sorted(str(job.get("config")) for job in failures))
    print(f"Failing configs for artma {version}: {configs}")
    print(f"Fingerprint: {fp}")

    duplicate = next((i for i in issues if fp in issue_fingerprints(i)), None)
    if duplicate:
        print(f"Open issue #{duplicate['number']} already covers this failure set.")
        set_output("issue_number", "")
        return

    title = f"r-universe checks failing: {configs}"
    body = issue_body(version, failures, build_urls(data), fp)

    if dry_run:
        print(f"[dry run] would ensure label '{LABEL}' exists")
        print(f"[dry run] would open issue titled: {title}")
        print("[dry run] issue body:")
        print(body)
        set_output("issue_number", "")
        return

    gh(
        [
            "label", "create", LABEL, "--repo", repo(), "--force",
            "--color", "d93f0b",
            "--description", "Filed by the r-universe monitor workflow",
        ],
        dry_run,
    )
    url = gh(
        [
            "issue", "create", "--repo", repo(), "--title", title,
            "--label", LABEL, "--body", body,
        ],
        dry_run,
    )
    number = url.rstrip("/").rsplit("/", 1)[-1]
    print(f"Opened issue #{number}: {url}")

    for issue in issues:
        print(f"Closing superseded issue #{issue['number']}.")
        close_issue(
            issue,
            f"The failure set changed; superseded by #{number}.",
            dry_run,
        )

    set_output("issue_number", number)


if __name__ == "__main__":
    main()
