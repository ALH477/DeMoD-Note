#!/usr/bin/env python3
"""
SoundFont search and download tool for DeMoD-Note
Uses the musical-artifacts.com API to search for SoundFont files.

Please consider donating to musical-artifacts.com to support their service!
https://musical-artifacts.com
"""

import sys
import json
import os
import urllib.request
import urllib.parse
import urllib.error
from typing import Optional, List, Dict, Any

API_BASE = "https://musical-artifacts.com/api/v1"
DONATION_URL = "https://musical-artifacts.com"

# Default download directory (user-specific, no sudo required)
DEFAULT_DOWNLOAD_DIR = os.path.expanduser("~/.local/share/soundfonts")
# System-wide directory (requires sudo)
SYSTEM_DOWNLOAD_DIR = "/etc/demod/sf"


class SoundFont:
    """Represents a SoundFont artifact from musical-artifacts.com"""
    def __init__(self, data: Dict[str, Any]):
        self.id = data.get("id", "")
        self.name = data.get("name", "Unknown")
        self.author = data.get("author", "Unknown")
        self.description = data.get("description", "")
        self.file_size = data.get("file_size", 0)
        self.downloads = data.get("downloads", 0)
        self.rating = data.get("rating", 0.0)
        self.download_url = data.get("download_url", "")
        self.formats = data.get("formats", [])
        self.tags = data.get("tags", [])
        self.source_url = data.get("source_url", "")
        
    def size_mb(self) -> float:
        """Return file size in MB"""
        return self.file_size / (1024 * 1024)
    
    def __str__(self) -> str:
        rating_str = f" ★{self.rating:.1f}" if self.rating > 0 else ""
        return f"{self.name} ({self.size_mb():.1f}MB){rating_str}"
    
    def to_json(self) -> str:
        """Serialize to JSON for inter-process communication"""
        return json.dumps({
            "id": self.id,
            "name": self.name,
            "author": self.author,
            "description": self.description,
            "file_size": self.file_size,
            "downloads": self.downloads,
            "rating": self.rating,
            "download_url": self.download_url,
            "formats": self.formats,
            "tags": self.tags
        })


def search_soundfonts(query: str, limit: int = 20) -> List[SoundFont]:
    """Search for SoundFonts on musical-artifacts.com"""
    params = {
        "format": "sf2",  # SoundFont2 format
        "search": query,
        "limit": limit
    }
    
    url = f"{API_BASE}/artifacts.json?{urllib.parse.urlencode(params)}"
    
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "DeMoD-Note/1.0"})
        with urllib.request.urlopen(req, timeout=30) as response:
            data = json.loads(response.read().decode("utf-8"))
            return [SoundFont(item) for item in data.get("artifacts", data) if isinstance(item, dict)]
    except urllib.error.URLError as e:
        print(f"ERROR: Network error: {e}", file=sys.stderr)
        return []
    except json.JSONDecodeError as e:
        print(f"ERROR: Invalid JSON response: {e}", file=sys.stderr)
        return []


def download_soundfont(sf: SoundFont, dest_dir: str = DEFAULT_DOWNLOAD_DIR, 
                       progress_callback=None) -> Optional[str]:
    """Download a SoundFont file to the specified directory.
    
    Returns the path to the downloaded file, or None on error.
    """
    if not sf.download_url:
        print(f"ERROR: No download URL for {sf.name}", file=sys.stderr)
        return None
    
    # Ensure destination directory exists
    os.makedirs(dest_dir, exist_ok=True)
    
    # Determine filename
    filename = sf.name.replace(" ", "_").replace("/", "_")
    if not filename.endswith(".sf2"):
        filename += ".sf2"
    dest_path = os.path.join(dest_dir, filename)
    
    try:
        req = urllib.request.Request(sf.download_url, headers={"User-Agent": "DeMoD-Note/1.0"})
        with urllib.request.urlopen(req, timeout=300) as response:
            total_size = int(response.headers.get("Content-Length", 0))
            downloaded = 0
            chunk_size = 8192
            
            with open(dest_path, "wb") as f:
                while True:
                    chunk = response.read(chunk_size)
                    if not chunk:
                        break
                    f.write(chunk)
                    downloaded += len(chunk)
                    if progress_callback and total_size > 0:
                        progress_callback(downloaded, total_size)
            
        return dest_path
    except urllib.error.URLError as e:
        print(f"ERROR: Download failed: {e}", file=sys.stderr)
        return None
    except IOError as e:
        print(f"ERROR: File write error: {e}", file=sys.stderr)
        return None


def format_size(size_bytes: int) -> str:
    """Format file size in human-readable format"""
    if size_bytes < 1024:
        return f"{size_bytes}B"
    elif size_bytes < 1024 * 1024:
        return f"{size_bytes / 1024:.1f}KB"
    elif size_bytes < 1024 * 1024 * 1024:
        return f"{size_bytes / (1024 * 1024):.1f}MB"
    else:
        return f"{size_bytes / (1024 * 1024 * 1024):.1f}GB"


def print_donation_banner():
    """Print a donation reminder banner"""
    print("\n" + "═" * 60)
    print("  ★  PLEASE SUPPORT MUSICAL-ARTIFACTS.COM  ★")
    print("═" * 60)
    print("\n  This service is provided free by volunteers.")
    print("  Please consider donating to keep it running!\n")
    print(f"  → {DONATION_URL}\n")
    print("═" * 60 + "\n")


def interactive_mode():
    """Run in interactive mode for command-line use"""
    print_donation_banner()
    
    query = input("Search for SoundFonts: ").strip()
    if not query:
        print("No search query provided.")
        return
    
    print(f"\nSearching for '{query}'...\n")
    results = search_soundfonts(query)
    
    if not results:
        print("No results found. Try a different search term.")
        return
    
    print(f"Found {len(results)} SoundFonts:\n")
    for i, sf in enumerate(results, 1):
        rating_str = f" ★{sf.rating:.1f}" if sf.rating > 0 else ""
        print(f"  {i:2}. {sf.name}")
        print(f"      Size: {format_size(sf.file_size)} | Downloads: {sf.downloads}{rating_str}")
        print(f"      Author: {sf.author}")
        print()
    
    # Select a soundfont
    try:
        choice = input("Enter number to download (or 'q' to quit): ").strip()
        if choice.lower() == 'q':
            return
        
        idx = int(choice) - 1
        if idx < 0 or idx >= len(results):
            print("Invalid selection.")
            return
        
        selected = results[idx]
        
        # Choose download location
        print(f"\nDownload: {selected.name}")
        print(f"  1. User directory: {DEFAULT_DOWNLOAD_DIR}")
        print(f"  2. System directory: {SYSTEM_DOWNLOAD_DIR} (requires sudo)")
        
        dest_choice = input("Select destination (1 or 2): ").strip()
        dest_dir = SYSTEM_DOWNLOAD_DIR if dest_choice == "2" else DEFAULT_DOWNLOAD_DIR
        
        print(f"\nDownloading to {dest_dir}...")
        
        def progress(current, total):
            percent = (current / total) * 100
            bar_len = 40
            filled = int(bar_len * current / total)
            bar = "█" * filled + "░" * (bar_len - filled)
            print(f"\r  [{bar}] {percent:.1f}%", end="", flush=True)
        
        result = download_soundfont(selected, dest_dir, progress)
        print()  # New line after progress bar
        
        if result:
            print(f"\n✓ Downloaded to: {result}")
            print("\nPlease consider donating to musical-artifacts.com!")
        else:
            print("\n✗ Download failed.")
            
    except ValueError:
        print("Invalid input.")
    except KeyboardInterrupt:
        print("\nCancelled.")


def main():
    """Main entry point"""
    if len(sys.argv) < 2:
        interactive_mode()
        return
    
    command = sys.argv[1]
    
    if command == "search":
        if len(sys.argv) < 3:
            print("Usage: soundfont-downloader.py search <query>", file=sys.stderr)
            sys.exit(1)
        query = " ".join(sys.argv[2:])
        results = search_soundfonts(query)
        # Output JSON for each result (one per line for easy parsing)
        for sf in results:
            print(sf.to_json())
            
    elif command == "download":
        if len(sys.argv) < 3:
            print("Usage: soundfont-downloader.py download <url|json>", file=sys.stderr)
            sys.exit(1)
        
        # Parse input as JSON or URL
        arg = sys.argv[2]
        dest_dir = sys.argv[3] if len(sys.argv) > 3 else DEFAULT_DOWNLOAD_DIR
        
        if arg.startswith("{"):
            # JSON input
            data = json.loads(arg)
            sf = SoundFont(data)
        else:
            # URL input
            sf = SoundFont({"download_url": arg, "name": "downloaded_soundfont"})
        
        result = download_soundfont(sf, dest_dir)
        if result:
            print(result)
        else:
            sys.exit(1)
            
    elif command == "donate":
        print_donation_banner()
        print(f"Visit: {DONATION_URL}")
        
    else:
        print(f"Unknown command: {command}", file=sys.stderr)
        print("Commands: search, download, donate", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()