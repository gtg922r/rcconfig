# mkbanner

Generate gradient ASCII art banners with centered subtitles using the **ANSI Shadow** figlet font—the same font used by [manytools.org/hacker-tools/ascii-banner](https://manytools.org/hacker-tools/ascii-banner/).

```
██████╗ ██╗   ██╗██████╗  ██████╗ ███╗   ██╗██╗ ██████╗
██╔══██╗╚██╗ ██╔╝██╔══██╗██╔═══██╗████╗  ██║██║██╔════╝
██████╔╝ ╚████╔╝ ██████╔╝██║   ██║██╔██╗ ██║██║██║     
██╔═══╝   ╚██╔╝  ██╔══██╗██║   ██║██║╚██╗██║██║██║     
██║        ██║   ██║  ██║╚██████╔╝██║ ╚████║██║╚██████╗
╚═╝        ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝ ╚═════╝
                exe.dev development VM
```

## Features

- **True-color gradients** — 24-bit RGB color with smooth line-by-line transitions
- **Multiple gradient modes** — start/end, middle color, or explicit color array
- **Centered subtitles** — taglines auto-centered beneath the banner
- **Left padding** — align banners in login scripts
- **Portable** — pure bash, only requires `figlet`

## Installation

```bash
# Install figlet
sudo apt install figlet    # Debian/Ubuntu
brew install figlet        # macOS

# Clone and setup
git clone <repo> && cd rcconfig/banner
chmod +x mkbanner

# Optional: install system-wide
sudo cp ansi_shadow.flf /usr/share/figlet/
sudo cp mkbanner /usr/local/bin/
```

## Usage

```
mkbanner [OPTIONS] <TITLE> [SUBTITLE]
```

### Examples

```bash
# Simple banner
mkbanner HELLO

# With subtitle
mkbanner HELLO "world"

# Lavender gradient (like PYRONIC)
mkbanner -s 200,170,220 -e 125,100,195 PYRONIC "exe.dev development VM"

# Nord blue gradient (like ATLAS)
mkbanner -s 136,192,208 -e 56,112,148 -p 8 ATLAS "autonomous total life assistant system"

# Auto-gradient from middle color
mkbanner -m 100,180,180 AQUA

# With left padding
mkbanner -p 4 PADDED "for login scripts"
```

### Options

| Option | Description |
|--------|-------------|
| `-m, --middle R,G,B` | Single color; auto-generates lighter→darker gradient |
| `-s, --start R,G,B` | Start color (requires `--end`) |
| `-e, --end R,G,B` | End color (requires `--start`) |
| `-g, --gradient C1:C2:...` | Explicit colors (colon-separated R,G,B) |
| `-p, --padding N` | Left padding in spaces (default: 0) |
| `-B, --no-bold` | Disable bold text |
| `-h, --help` | Show help |

### Color Presets

| Name | Start | End |
|------|-------|-----|
| Lavender | `200,170,220` | `125,100,195` |
| Nord Blue | `136,192,208` | `56,112,148` |
| Sunset | `255,150,100` | `200,80,80` |
| Forest | `150,220,150` | `50,120,50` |

## Login Banner Example

```bash
# ~/.bash_banner
mkbanner -s 200,170,220 -e 125,100,195 -p 2 PYRONIC "exe.dev development VM"
echo
printf "  HOST: %s\n" "$(hostname -f)"
printf "  DATE: %s\n" "$(date '+%A, %B %d %Y')"
```

Source from `~/.bashrc`:
```bash
[[ -f ~/.bash_banner ]] && source ~/.bash_banner
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `MKBANNER_CHAR_WIDTH` | Set to `2` if your terminal renders box-drawing chars as double-width |

## Requirements

- **bash** 4.0+
- **figlet**
- Terminal with **true-color (24-bit)** support

## Files

```
banner/
├── mkbanner          # Main script
├── ansi_shadow.flf   # ANSI Shadow font
├── README.md
└── AGENTS.md
```

## License

MIT
