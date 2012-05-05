REM @echo off

"C:\Program Files (x86)\ImageMagick\convert.exe" -size 16x16 xc:transparent z:\l\canvas.png

for /R "H:\Icons\use\famfamfam_flag_icons\png" %%f in (*.png) do (
  "C:\Program Files (x86)\ImageMagick\convert.exe" "H:\Icons\use\famfamfam_flag_icons\png\%%~nf%%~xf" -resize 17x10! "z:\l\%%~nf_out.png"
  "C:\Program Files (x86)\ImageMagick\composite.exe" -gravity center z:\l\%%~nf_out.png z:\l\canvas.png z:\l\%%~nf.ico
  del z:\l\%%~nf_out.png
)

