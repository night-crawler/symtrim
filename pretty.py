def pretty_rust_path(s: str, indent: str = "    ") -> str:
    """
    Add line breaks/indentation to very long Rust-like type / path definitions.

    It handles:
    - angle brackets: <...>
    - parentheses: (...)
    - square brackets: [...]
    - commas
    - top-level :: path separators

    It is intentionally heuristic, but works well for huge rustc-style names.
    """

    OPEN_TO_CLOSE = {"<": ">", "(": ")", "[": "]"}
    CLOSE_TO_OPEN = {v: k for k, v in OPEN_TO_CLOSE.items()}

    out = []
    stack = []
    i = 0
    n = len(s)

    def current_indent(extra: int = 0) -> str:
        return indent * (len(stack) + extra)

    while i < n:
        ch = s[i]

        # Keep Rust path separators together
        if s.startswith("::", i):
            # Break before top-level :: for readability
            if not stack:
                out.append("\n")
                out.append(current_indent())
            out.append("::")
            i += 2
            continue

        if ch in OPEN_TO_CLOSE:
            out.append(ch)
            stack.append(ch)

            # Newline after opening delimiter unless it's empty
            j = i + 1
            while j < n and s[j].isspace():
                j += 1

            if j < n and s[j] != OPEN_TO_CLOSE[ch]:
                out.append("\n")
                out.append(current_indent())
            i += 1
            continue

        if ch in CLOSE_TO_OPEN:
            if stack and stack[-1] == CLOSE_TO_OPEN[ch]:
                stack.pop()

            # Put closing delimiter on its own indented line
            if out and out[-1] != "\n":
                out.append("\n")
            out.append(current_indent())
            out.append(ch)
            i += 1
            continue

        if ch == ",":
            out.append(",")
            out.append("\n")
            out.append(current_indent())
            i += 1
            continue

        out.append(ch)
        i += 1

    # Cleanup: remove trailing whitespace on lines and collapse excess blank lines
    lines = ["".join(line).rstrip() for line in "".join(out).splitlines()]
    cleaned = []
    prev_blank = False
    for line in lines:
        blank = not line.strip()
        if blank and prev_blank:
            continue
        cleaned.append(line)
        prev_blank = blank

    return "\n".join(cleaned).strip()


if __name__ == "__main__":
    s = r"""
    <gimli::read::unit::EntriesCursor<gimli::read::endian_slice::EndianSlice<gimli::endianity::LittleEndian>>>::next_entry
    """

    print(pretty_rust_path(s))