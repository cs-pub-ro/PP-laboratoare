* Se ia conținutul sursei de pe elf și se pune într-un fișier `README.rst`
* Se folosește comanda
  * `pandoc --from mediawiki --to gfm -o README.md README.rst`
* Se repară formatul pentru toate bucățile de cod (atât inline, cât și codul multi-linie).
  * Pentru codul multi-linie, se folosește (fără backslashes):
```markdown
\```lisp
\ ...cod...
\```
```
  * Se repară orice alte probleme legate de markup-ul cu * și cu `
