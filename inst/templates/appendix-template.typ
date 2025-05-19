#let article(
  lang: "en",
  region: "NZ",
  font: ("Wickliffe",),
  fontsans: ("Wickliffe Sans",),
  fontmono: ("Consolas", "Cascadia Mono", "CMU Typewriter Text", "Lucida Console",),
  fontsize: 10pt,
  bib: "references.yaml",
  bibliographystyle: "vancouver-superscript",
  doc,
) = {
  set par(leading: 0.55em, first-line-indent: 1em, justify: true, spacing: 0.55em)
  set text(lang: lang,
           region: region,
           font: fontsans,
           size: fontsize)
  show raw: set text(font: fontmono,
                     size: 0.9 * fontsize,
                     weight: "medium")
  show heading: set block(above: 1.4em, below: 1em)
  show heading: set text(font: font)
  show heading.where(level: 1): set heading(numbering: (..nums) => [
    Appendix #numbering("A:", nums.pos().at(0))
  ])
  show heading.where(level: 1): set block(above: 0em, below: 2em)
  set super(typographic: false)
  set bibliography(style: bibliographystyle, title: "References")
  set footnote.entry(
    separator: line(length: 100%),
    indent: 0em
  )
  show footnote.entry: set align(left)
  set list(indent: 1em, marker: [•])

  set figure(placement: none)
  show figure: it => {
    set block(spacing: 0.65em, breakable: true)
    show block: set align(left)
    set text(size: 0.8 * fontsize)
    set place(clearance: 3em)
    set par(first-line-indent: 0em, justify: false)
    it
  }
  show figure.caption: it => {
    set align(left)
    set text(size: fontsize)
    strong(it)
  }
  show figure.where(kind: "suppl-table"): set figure(
    numbering: (..nums) =>
      [#context {numbering("A", counter(heading).get().at(0))}#numbering("1", nums.pos().at(0))],
    supplement: "Table"
  )
  show figure.where(kind: "suppl-image"): set figure(
    numbering: (..nums) =>
      [#context {numbering("A", counter(heading).get().at(0))}#numbering("1", nums.pos().at(0))],
    supplement: "Figure"
  )
  show ref: it => {
    let fig = figure
    let el = it.element
    if el != none and el.func() == fig and (el.kind == "suppl-table" or el.kind == "suppl-image") {
      // Override references for supplementary tables.
      link(el.location())[#el.supplement~#numbering(
         "A",
         ..counter(heading).at(el.location())
       )#numbering(
         "1",
         ..counter(fig.where(kind: el.kind)).at(el.location())
       )]
    } else if el != none and el.func() == fig and (el.kind == "table" or el.kind == "image"){
      link(el.location())[#el.supplement~#numbering(
         "1",
         ..counter(fig.where(kind: el.kind)).at(el.location())
       )]
    } else {
      // Other references as usual.
      it
    }
  }

  show table.cell: set text(size: 0.9 * fontsize)
  set table(inset: (x: 6pt, y: 0.3em), stroke: none)
  set table.hline(stroke: 0.5pt)

  doc
}
