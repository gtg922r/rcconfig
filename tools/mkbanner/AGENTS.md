# AGENTS.md - Technical Notes

Implementation details and gotchas for the `mkbanner` script.

## Architecture

Single bash file, no dependencies beyond `figlet`. Generates ASCII art via figlet,
applies per-line ANSI color codes, and centers an optional subtitle.

## Key Functions

| Function | Purpose |
|----------|--------|
| `parse_rgb` | Validate R,G,B format |
| `rgb_to_ansi` | Convert RGB to `\033[38;2;r;g;bm` |
| `lerp_color` | Linear interpolation between two colors |
| `make_gradient` | Generate N colors between start/end |
| `gradient_from_middle` | Auto-lighten/darken from middle color |
| `display_width` | Calculate display width (handles Unicode) |
| `clamp` | Constrain value to 0-255 |

## Critical: Locale Handling

The script **must** set `LC_ALL=C.UTF-8` at the start.

**Why:** SSH sessions often inherit `LC_CTYPE=POSIX` from the client, which causes
`${#var}` to count **bytes** instead of **characters** for UTF-8 strings.

```bash
# With LC_CTYPE=POSIX:
line="████"
echo ${#line}  # Returns 12 (bytes), not 4 (chars)

# With LC_ALL=C.UTF-8:
echo ${#line}  # Returns 4 (correct)
```

This broke subtitle centering calculations until we added the locale override.

## Gotchas

### 1. Arithmetic Exit Codes

`(( x++ ))` returns exit code 1 when x=0 (the old value is falsy). With `set -e`,
this exits the script. Use `(( ++x ))` instead (returns new value).

### 2. ANSI Code Storage

Use `$'\033[0m'` syntax to store evaluated escape sequences:

```bash
# Good: escape is evaluated at assignment
readonly RESET=$'\033[0m'

# Bad: escape stored as literal, needs printf %b
RESET='\033[0m'
```

### 3. Figlet Trailing Lines

Figlet adds a trailing blank line. Remove it before processing:

```bash
while (( ${#lines[@]} )) && [[ -z "${lines[-1]// /}" ]]; do
    unset 'lines[-1]'
done
```

### 4. Display Width vs Character Count

Unicode box-drawing chars may render as double-width in some terminals.
The `display_width` function handles this via `MKBANNER_CHAR_WIDTH` env var.
Default is single-width (1) which works for most terminals including iTerm2 via SSH.

## Testing

```bash
# Basic
./mkbanner TEST
./mkbanner TEST "subtitle"

# Gradients
./mkbanner -m 150,150,200 MIDDLE
./mkbanner -s 255,0,0 -e 0,0,255 GRADIENT
./mkbanner -g "255,0,0:0,255,0:0,0,255" EXPLICIT

# Options
./mkbanner -p 4 PADDED
./mkbanner -B NOBOLD

# Verify subtitle centering
./mkbanner TEST "Sub" | tail -1 | sed 's/\x1b\[[0-9;]*m//g' | cat -A
```

## Font

The ANSI Shadow font (`ansi_shadow.flf`) uses Unicode box-drawing characters:
- `█` (U+2588) Full block
- `║═╔╗╚╝╠╣╦╩` Box-drawing pieces

Download: https://raw.githubusercontent.com/xero/figlet-fonts/master/ANSI%20Shadow.flf
