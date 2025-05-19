#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  keywords: none,
  margin: (x: 2.5cm, y: 2.5cm),
  paper: "a4",
  lang: "en",
  region: "NZ",
  font: ("Wickliffe",),
  fontsans: ("Wickliffe Sans",),
  fontmono: ("Consolas", "Cascadia Mono", "CMU Typewriter Text", "Lucida Console",),
  fontsize: 10pt,
  bib: "references.yaml",
  bibliographystyle: "vancouver-superscript",
  sectionnumbering: none,
  appendix: [],
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(leading: 0.55em, first-line-indent: 1em, justify: true, spacing: 0.55em)
  set text(lang: lang,
           region: region,
           font: fontsans,
           size: fontsize)
  show raw: set text(font: fontmono,
                     size: 0.9 * fontsize,
                     weight: "medium")
  set heading(numbering: sectionnumbering)
  show heading: set block(above: 1.4em, below: 1em)
  show heading: set text(font: font)
  set super(typographic: false)
  set bibliography(style: bibliographystyle, title: "References")
  set footnote.entry(
    separator: line(length: 100%),
    indent: 0em
  )
  show footnote.entry: set align(left)
  set list(indent: 1em, marker: [•])

  if title != none {
    align(center)[#block(inset: 2em)[#par(justify: false)[
      #text(font: font, weight: "bold", size: 2em)[#title]
    ]]]
  }

  if subtitle != none {
    align(center)[#block(inset: (x: 2em))[#par(justify: false)[
      #text(font: font, weight: "bold", size: 1.5em)[#subtitle]
    ]]]
  }

  if authors != none {
    let affiliations = ()
    for author in authors {
      for affiliation in author.affiliation {
        if affiliation not in affiliations {
          affiliations.push(affiliation)
        }
      }
    }
    let affiliations_fn = affiliations.map(it => {
      let idx = affiliations.position(i => it == i) + 1
      [#super[#idx] #it]
    })
    footnote(numbering: x => [#sym.zws])[
      #affiliations_fn.join(linebreak())#linebreak()#linebreak()
    ]
    counter(footnote).update(0)
    let names = authors.map(author => {
      let affiliation = author.affiliation.map(aff =>
        str(affiliations.position(i => aff == i) + 1)).join(",")
      [#author.name#if author.email != "" {
        [#footnote(numbering: "*")[Corresponding author.#linebreak()_Email_: #link(
          "mailto:" + author.email.replace("\\", "")
        )]]
      }#super[#affiliation]]
    }).join(", ")
    align(center)[#par(justify: false)[#names]]
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 1em)[
    #set par(first-line-indent: 0em, spacing: 1em)
    *Abstract*

    #abstract

    #if keywords != none {[*Keywords:* #keywords.join("; ")]}
    ]
  }

  set figure(placement: auto)
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

  pagebreak()

  doc

  bibliography(bib)

  appendix
}
