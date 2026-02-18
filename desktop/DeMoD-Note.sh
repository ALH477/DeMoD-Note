#!/usr/bin/env bash
# DeMoD-Note Desktop Application Launcher
# Starts all required processes for DeMoD-Note

set -e

APP_NAME="DeMoD-Note"
LOG_DIR="$HOME/.local/share/DeMoD-Note/logs"
PID_DIR="$HOME/.local/share/DeMoD-Note/pids"

# Create directories
mkdir -p "$LOG_DIR"
mkdir -p "$PID_DIR"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    notify-send -i audio-x-generic "$APP_NAME" "$1" 2>/dev/null || true
}

cleanup() {
    log "Stopping DeMoD-Note..."
    
    # Kill child processes
    if [ -f "$PID_DIR/jackd.pid" ]; then
        kill $(cat "$PID_DIR/jackd.pid") 2>/dev/null || true
    fi
    if [ -f "$PID_DIR/bridge.pid" ]; then
        kill $(cat "$PID_DIR/bridge.pid") 2>/dev/null || true
    fi
    if [ -f "$PID_DIR/demod.pid" ]; then
        kill $(cat "$PID_DIR/demod.pid") 2>/dev/null || true
    fi
    
    log "DeMoD-Note stopped"
    exit 0
}

trap cleanup SIGINT SIGTERM

# Check if JACK is already running
if ! jack_lsp >/dev/null 2>&1; then
    log "Starting JACK audio server..."
    
    # Start JACK with dummy driver (works without hardware)
    jackd -d dummy -r 44100 -p 256 &
    JACK_PID=$!
    echo $JACK_PID > "$PID_DIR/jackd.pid"
    
    # Wait for JACK to start
    sleep 2
    
    if jack_lsp >/dev/null 2>&1; then
        log "JACK started successfully"
    else
        log "Failed to start JACK"
        exit 1
    fi
else
    log "JACK already running"
fi

# Start OSC-MIDI Bridge
log "Starting OSC-MIDI bridge..."
cd "$(dirname "$0")"
python3 tools/osc-midi-bridge.py > "$LOG_DIR/bridge.log" 2>&1 &
BRIDGE_PID=$!
echo $BRIDGE_PID > "$PID_DIR/bridge.pid"
log "OSC-MIDI bridge started (PID: $BRIDGE_PID)"

# Start DeMoD-Note
log "Starting DeMoD-Note..."
cabal run -- run > "$LOG_DIR/demod.log" 2>&1 &
DEMOD_PID=$!
echo $DEMOD_PID > "$PID_DIR/demod.pid"
log "DeMoD-Note started (PID: $DEMOD_PID)"

# Wait a moment for everything to initialize
sleep 3

log "DeMoD-Note is running!"
log "Connect your audio: microphone → DeMoD-Note:input"
log "Connect MIDI: DeMoD-Note → your synth"

# Show connection info
notify-send -i audio-x-generic "$APP_NAME" "DeMoD-Note is running!

Audio: Connect microphone to DeMoD-Note:input
MIDI: Connect DeMoD-Note port to your synth

Press Ctrl+C in this terminal to stop."

# Keep running and show status
echo ""
echo "=========================================="
echo "  DeMoD-Note Desktop Application"
echo "=========================================="
echo ""
echo "Status:"
echo "  - JACK: $(jack_lsp >/dev/null 2>&1 && echo 'Running' || echo 'Stopped')"
echo "  - Bridge: Running (PID $(cat $PID_DIR/bridge.pid))"
echo "  - DeMoD-Note: Running (PID $(cat $PID_DIR/demod.pid))"
echo ""
echo "Connections to make (in qjackctl):"
echo "  Audio: microphone → DeMoD-Note:input"
echo "  MIDI: DeMoD-Note → your synth"
echo ""
echo "Press Ctrl+C to stop all services"
echo "=========================================="

# Wait forever
wait
