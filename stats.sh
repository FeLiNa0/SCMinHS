grep -cve '^\s*$' Main.hs Scheme/*hs Scheme/*/*hs && cat Main.hs Scheme/*hs Scheme/*/*hs | sed '/^\s*$/d' | wc -l && wc -l Main.hs Scheme/*hs Scheme/*/*hs && du -hc Main.hs Scheme/*hs Scheme/*/*hs
