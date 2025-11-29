import sys
from yt_dlp import YoutubeDL

def download_as_wav(url, name):
    ydl_opts = {
        "format": "bestaudio/best",
        "noplaylist": True,
        "postprocessors": [
            {
                "key": "FFmpegExtractAudio",
                "preferredcodec": "wav",
                "preferredquality": "1411"  # calidad estándar de WAV
            }
        ],
        "outtmpl": f"{name}.%(ext)s"  # se guarda con el nombre
    }

    with YoutubeDL(ydl_opts) as ydl:
        ydl.download([url])

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Uso:uv run fx-maker.py <url-youtube> <name-archivo>")
    else:
        download_as_wav(str(sys.argv[1]), str(sys.argv[2]))
