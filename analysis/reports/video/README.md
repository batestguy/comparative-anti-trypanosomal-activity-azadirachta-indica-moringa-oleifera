# Walkthrough video

`analysis/media/trypanosoma-tour.mp4` — 2:31, 1280×720, 30 fps.

The video is a recorded tour of the committed results, not a screen capture of
an app: `tour.html` lays out the figures in `analysis/results/figures` beside
the numbers in `analysis/results/tables`, and `record-tour.js` drives it in a
real Chrome window (cursor, caption strip, title and end cards), so every
frame is reproducible from the repo.

## Rebuild

```bash
# 1. serve the repo root (the page loads figures by relative path)
python -m http.server 8001 --bind 127.0.0.1

# 2. record → out/video/raw.webm  (~2.5 min, Chrome opens)
node analysis/reports/video/record-tour.js

# 3. mux the music bed: start 65 s into the track, fade in/out, 1280x720 h264
ffmpeg -i analysis/reports/video/out/video/raw.webm -ss 65.2 -i "Headlock.mp3" \
  -filter_complex "[0:v]fps=30,scale=1280:720[v];\
[1:a]atrim=0:150.9,asetpts=N/SR/TB,volume=0.38,\
afade=t=in:st=0:d=2,afade=t=out:st=145.6:d=5[a]" \
  -map "[v]" -map "[a]" -c:v libx264 -crf 22 -pix_fmt yuv420p \
  -c:a aac -b:a 160k -movflags +faststart -shortest \
  analysis/media/trypanosoma-tour.mp4

# 4. cover still
ffmpeg -ss 105 -i analysis/media/trypanosoma-tour.mp4 -frames:v 1 \
  -y analysis/media/trypanosoma-tour-cover.jpg
```

`snap-scenes.js` screenshots each scene at 1280×720 (`out/scenes/`) for layout
review and reports any scene whose content overflows its 720 px frame — run it
after editing `tour.html`.

## Notes

- Scene height is exactly 720 px and the recorder scrolls to `index * 720`;
  adding or removing a section renumbers the beats in `record-tour.js`.
- The music is a copyrighted recording and is **not** covered by the
  repository's CC BY 4.0 licence. Only the muxed video is committed; the audio
  file is never added to the repo.
- Playwright is resolved from a global install if it is not a project
  dependency (`@playwright/mcp/node_modules/playwright-core` under `%APPDATA%\npm`),
  and the installed Chrome channel is used rather than a bundled headless shell.
