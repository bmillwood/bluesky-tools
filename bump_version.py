#!/usr/bin/env python

import datetime
import subprocess

CABAL_FILE = "bluesky-tools.cabal"


def get_cabal_version() -> str:
    for line in open(CABAL_FILE):
        if line.startswith("version:"):
            return line[len("version:"):].lstrip()
    else:
        raise ValueError(f"failed to find version line in {CABAL_FILE}")


def map_lines(path: str, f, require_modification: bool = True) -> None:
    lines = open(path).readlines()
    modified = False
    for lineno, line in enumerate(lines):
        new = f(line)
        if new is not None:
            lines[lineno] = new
            modified = True

    if modified:
        open(path, "w").write("".join(lines))
    elif require_modification:
        raise ValueError(f"{path} was not updated")


def set_cabal_version(version: str) -> None:
    def f(line: str) -> str | None:
        if not line.startswith("version:"):
            return None

        for i in range(len(line)):
            if line[-i - 1] in {" ", ":"}:
                break
        else:
            assert False, line
        return f"{line[:-i]}{version}\n"

    map_lines(CABAL_FILE, f)


def set_nix_version(version: str) -> None:
    def f(line: str) -> str | None:
        if not line.startswith("  version = "):
            return None

        return f'  version = "{version}";\n'

    map_lines("bluesky-tools.nix", f)


def update_changelog(old_version: str, version: str) -> None:
    def f(line: str) -> str | None:
        if not line.startswith(f"## {old_version}"):
            return None

        return f'## {version} -- {datetime.date.today().isoformat()}\n'

    map_lines("CHANGELOG.md", f)


def bump_to_release_version(version: str) -> str:
    components = version.split(".")

    for i, c in enumerate(components):
        if int(c) % 2 == 1:
            break
    else:
        raise ValueError("already a release version")

    components[i] = str(int(c) + 1)

    for j in range(i + 1, len(components)):
        components[j] = "0"

    return ".".join(components)


def main():
    subprocess.run(
        ["git", "diff-index", "--exit-code", "HEAD"],
        check=True,
    )
    old_version = get_cabal_version()
    version = bump_to_release_version(version=old_version)
    set_cabal_version(version=version)
    set_nix_version(version=version)
    update_changelog(old_version=old_version, version=version)
    subprocess.run(
        ["git", "add", "."],
        check=True,
    )
    subprocess.run(
        ["git", "commit", "-m", version],
        check=True,
    )


if __name__ == "__main__":
    main()
