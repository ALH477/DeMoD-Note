# DeMoD-Note

**Deterministic Monophonic Note Detector**

DeMoD-Note is a production-grade, ultra-low-latency real-time audio processor written in Haskell. It performs hybrid pitch detection from live audio input with deterministic timing guarantees optimized for real-time kernels. Outputs MIDI events over JACK to drive FluidSynth or any MIDI synthesizer with sub-3ms latency for high frequencies and intelligent bass handling.

## Key Features

- **Triple-Path Hybrid Detection:**
  - **Fast Path (2.66ms):** Zero-crossing PLL for 200Hz+ → 98% accuracy
  - **Medium Path (12ms):** Autocorrelation for 80-200Hz → 85% confidence
  - **Slow Path (30ms):** YIN validation for 30-80Hz bass → Pitch bend correction
  
- **Distinct Onset Detection:** Multi-feature onset detection (spectral flux, energy transient, phase deviation) with 0.67 threshold and 1.33ms quantization

- **Deterministic Architecture:**
  - Single-core dual-thread design (SCHED_FIFO priority 99/98)
  - Lock-free ring buffer with cache-line alignment
  - Zero allocation in hot path
  - Monophonic root-note optimization
  - 16-bit 96kHz audio processing

- **Smart Bass Handling:**
  - 12ms speculative output with ±2 semitone pitch bend correction
  - 30ms validation for 30-80Hz range
  - Smooth 2.66ms pitch transitions
  - Finish-first note policy

- **JACK Integration:**
  - Native JACK backend (128-sample buffers @ 96kHz)
  - Real-time safe processing
  - MIDI output with timing compensation
  - OSC control server (UDP 57120)
  - Web dashboard (/status endpoint)

## Quick Start (Nix)

```bash
git clone https://github.com/ALH477/DeMoD-Note.git
cd DeMoD-Note
nix develop
cabal run DeMoD-Note -- --help
```

Start JACK with 96kHz sample rate and 128-sample buffers, then run the binary:
```bash
jackd -d alsa -r 96000 -p 128 &
cabal run DeMoD-Note
```

Connect your instrument to `DeMoD-Note:input` and `DeMoD-Note:midi_out` to your synthesizer.

## Building All Outputs

```bash
# AppImage (portable Linux executable)
nix build .#appimage

# Windows executable (.exe)
nix build .#windows

# Static Linux binary
nix build .#static

# Default package
nix build
```

## Configuration

Copy the example configuration and customize:
```bash
cp config.example.toml ~/.config/demod-note/config.toml
```

Key settings in `config.toml`:
- `detection.onsetThreshDb`: Onset detection threshold (-40.0 dB default)
- `detection.fastValidationMs`: 2.66ms for high frequencies
- `detection.bassFastMs`: 12ms speculative bass output
- `detection.bassConfirmMs`: 30ms bass validation
- `onset.threshold`: 0.67 (onset sensitivity)
- `rt.*`: Real-time threading configuration

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    JACK Audio Thread                      │
│  (Priority 99, SCHED_FIFO)                               │
│  └─> Capture 128 samples (1.33ms @ 96kHz)               │
│  └─> Push to lock-free ring buffer                       │
└─────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│                   Detector Thread                         │
│  (Priority 98, SCHED_FIFO)                               │
│  ├─> Onset detection (spectral + energy + phase)        │
│  ├─> Fast path: Zero-crossing PLL (2.66ms)              │
│  ├─> Medium path: Autocorrelation (12ms)                │
│  ├─> Slow path: YIN validation (30ms)                   │
│  └─> MIDI output with pitch bend correction             │
└─────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│                    FluidSynth                             │
│              (MIDI → Audio Synthesis)                     │
└─────────────────────────────────────────────────────────┘
```

## Performance Specifications

| Frequency Range | Latency | Method | Accuracy |
|----------------|---------|---------|----------|
| 200Hz - 4kHz | 2.66ms | Zero-crossing PLL | 98% |
| 80Hz - 200Hz | 12ms speculative<br>30ms validated | Autocorrelation + YIN | 85% → 95% |
| 30Hz - 80Hz | 30ms | YIN validation | 95% |

- **Buffer size:** 128 samples (1.33ms @ 96kHz)
- **Jitter:** <0.5ms guaranteed
- **CPU usage:** <5% on modern x86_64
- **Memory:** ~256KB locked RAM

## Real-Time System Requirements

For optimal performance, configure your system:

```bash
# Kernel boot parameters
isolcpus=2,3 irqaffinity=0,1 rcu_nocbs=2,3 nosmt

# Real-time settings
echo 99 > /proc/sys/kernel/sched_rt_runtime_us  # Disable RT throttling
echo performance | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
```

## NixOS Integration

```nix
{
  inputs.demod-note.url = "github:ALH477/DeMoD-Note";
  
  outputs = { self, nixpkgs, demod-note }: {
    nixosConfigurations.myMachine = nixpkgs.lib.nixosSystem {
      modules = [
        demod-note.nixosModules.default
        {
          services.demod-note = {
            enable = true;
            user = "youruser";
            configFile = "/etc/demod-note.toml";
          };
        }
      ];
    };
  };
}
```

## Modules

- `DeMoDNote.Types` – Core type definitions and state machines
- `DeMoDNote.Config` – TOML configuration parsing
- `DeMoDNote.Detector` – Triple-path pitch detection engine
- `DeMoDNote.Backend` – JACK audio and MIDI backend
- `DeMoDNote.OSC` – OSC control server
- `DeMoDNote.Monitor` – Web dashboard
- `DeMoDNote.Scale` – Musical scales and note handling
- `DeMoDNote.Arpeggio` – Arpeggio patterns
- `DeMoDNote.BPM` – Tempo and time handling
- `DeMoDNote.Preset` – Preset management
- `DeMoDNote.SoundFont` – FluidSynth integration
- `DeMoDNote.TUI` – Terminal user interface

## Contributing

Contributions welcome! Please ensure:
- Code compiles with `-Wall -Werror`
- No allocations in hot paths
- Deterministic timing maintained
- Tests pass: `cabal test`

## License

MIT License - See LICENSE file

Copyright (c) 2026 DeMoD-Note Project

## Acknowledgments

- Built with Haskell, JACK, and Nix
- FFT via vector-fftw
- OSC via hosc
- Inspired by aubio, YIN, and real-time audio research

## Contact

- GitHub: https://github.com/ALH477/DeMoD-Note
- Issues: https://github.com/ALH477/DeMoD-Note/issues
