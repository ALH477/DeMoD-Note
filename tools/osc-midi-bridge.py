#!/usr/bin/env python3
"""OSC to MIDI bridge for DeMoD-Note"""

import sys
from pythonosc.dispatcher import Dispatcher
from pythonosc.osc_server import ThreadingOSCUDPServer
import mido
from mido import Message

OSC_PORT = 57120
MIDI_PORT_NAME = "DeMoD-Note"

midi_port = None

def note_trigger(unused_addr, args, note, vel):
    """Handle /demod/note/trigger"""
    msg = Message("note_on", note=int(note), velocity=int(vel))
    midi_port.send(msg)
    print(f"MIDI Note On: {note} vel={vel}")

def note_off(unused_addr, args, note):
    """Handle /demod/note/off"""
    msg = Message("note_off", note=int(note), velocity=0)
    midi_port.send(msg)
    print(f"MIDI Note Off: {note}")

def main():
    global midi_port
    
    midi_port = mido.open_output(MIDI_PORT_NAME, virtual=True)
    print(f"Created virtual MIDI port: {MIDI_PORT_NAME}")
    
    dispatcher = Dispatcher()
    dispatcher.map("/demod/note/trigger", note_trigger)
    dispatcher.map("/demod/note/off", note_off)
    
    server = ThreadingOSCUDPServer(("127.0.0.1", OSC_PORT), dispatcher)
    print(f"OSC server listening on port {OSC_PORT}")
    print("Bridge ready. Press Ctrl+C to stop.")
    
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
        midi_port.close()

if __name__ == "__main__":
    main()
