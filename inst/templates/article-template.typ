#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  keywords: none,
  margin: (x: 2cm, y: 2cm),
  paper: "a4",
  cols: 2,
  background: none,
  header: none,
  draft: false,
  lang: "en",
  region: "NZ",
  font: (
    "Charis",
    "Charter",
    "Century Schoolbook",
    "Wickliffe",
    "Libertinus Serif",
    "DejaVu Serif",
  ),
  fontsans: ("Source Sans 3", "Wickliffe Sans", "Gill Sans MT", "DejaVu Sans"),
  fontmono: ("Source Code Pro", "Consolas", "DejaVu Sans Mono"),
  fontsize: 9pt,
  bib: "../references.bib",
  bibliographystyle: "american-medical-association",
  sectionnumbering: none,
  appendix: [],
  doc,
) = {
  let linespace = 1.3em
  set page(
    paper: paper,
    margin: if draft { (x: 2.5cm, y: 2cm) } else { margin },
    columns: if draft { 1 } else { cols },
    numbering: "1",
    background: if background == auto {
      if draft {
        rotate(56deg, text(80pt, fill: rgb("FFCBC4"))[DRAFT])
      } else {
        none
      }
    } else {
      background
    },
    header: if header == auto {
      if draft {
        align(center)[_DRAFT MANUSCRIPT: DO NOT CITE_]
      } else {
        none
      }
    } else {
      header
    },
  )
  set columns(gutter: 1cm)
  set par(
    leading: if draft { 2em } else { linespace },
    first-line-indent: 1em,
    justify: true,
    spacing: if draft { 2em } else { linespace },
  )
  set text(
    lang: lang,
    region: region,
    font: font,
    size: fontsize,
    bottom-edge: "baseline",
    top-edge: "baseline",
  )
  show raw: set text(font: fontmono, size: 0.9 * fontsize, weight: "medium")
  set heading(numbering: sectionnumbering)
  show heading: set block(
    above: if draft { 30pt + 2em } else { 10pt + linespace },
    below: if draft { 10pt + 2em } else { 2pt + linespace },
  )
  show heading: it => {
    // Clever trick to reduce spacing between consecutive headings
    // See https://github.com/typst/typst/issues/2953
    let previous_headings = query(selector(heading).before(here(), inclusive: false))
    if previous_headings.len() > 0 {
      let ploc = previous_headings.last().location().position()
      let iloc = it.location().position()
      if (iloc.page == ploc.page and iloc.x == ploc.x and iloc.y - ploc.y < 50pt) {
        // threshold
        v(-6pt) // amount to reduce spacing
      }
    }
    it
  }
  show heading: set text(font: fontsans, weight: "medium")
  show heading.where(level: 1): set text(weight: "bold", size: 1.1 * fontsize)
  show heading.where(level: 2): set text(size: 1.1 * fontsize)
  show heading.where(level: 3): set text(size: fontsize)
  set bibliography(style: bibliographystyle, title: "References")
  set footnote.entry(
    separator: line(length: 100%),
    indent: 0em,
  )
  show footnote.entry: set align(left)
  set list(indent: 1em, marker: [•])

  page(columns: 1)[
    #if title != none {
      block(inset: (top: if draft { 1cm } else { 2.5cm }), width: 100%)[#align(center)[#par(
        justify: false,
        leading: linespace + 1em,
      )[
        #text(font: fontsans, weight: "medium", size: 2em)[#title]
      ]]]
    }

    #if subtitle != none {
      block(inset: (top: 1cm), width: 100%)[#align(center)[#par(
        justify: false,
        leading: linespace + 1em,
      )[
        #text(font: fontsans, weight: "medium", size: 1.5em)[#subtitle]
      ]]]
    }

    #if authors != none {
      let affiliations = ()
      for author in authors {
        for affiliation in author.affiliation {
          if affiliation not in affiliations { affiliations.push(affiliation) }
        }
      }
      let affiliations_fn = affiliations.map(it => {
        let idx = affiliations.position(i => it == i) + 1
        [#super[#idx] #it]
      })
      footnote(numbering: x => [#sym.zws])[#par(
        first-line-indent: 0em,
        leading: linespace,
        spacing: linespace,
      )[ #affiliations_fn.join(linebreak()) ]]
      counter(footnote).update(0)
      let names = authors
        .map(author => {
          let affiliation = author
            .affiliation
            .map(
              aff => str(affiliations.position(i => aff == i) + 1),
            )
            .join(",")
          [#author.name#if author.email != "" {
              [#footnote(numbering: x => [#sym.zws])[#par(
                first-line-indent: 0em,
                leading: linespace,
                spacing: linespace,
              )[
                \* Corresponding author.
                #linebreak()
                _Email_: #link("mailto:" + author.email.replace("\\", ""))
              ]]]
            }#super[#affiliation]]
        })
        .join(", ")
      block(inset: (top: if draft { 0cm } else { 2.5cm }), width: 100%)[
        #align(center)[#par(justify: false, leading: linespace + 1em)[#names]]
      ]
    }

    #if date != none {
      block(inset: (top: 0.5cm), width: 100%)[#align(center)[#date]]
    }

    #if abstract != none {
      block(
        inset: (x: if draft { 1.5cm } else { 2cm }, top: if draft { 0.5cm } else { 2cm }),
        width: 100%,
      )[
        #set par(
          first-line-indent: 0em,
          spacing: if draft { 2.5em } else { linespace + 0.5em },
          leading: if draft { 2em } else { linespace },
        )
        *Abstract*

        #abstract

        #if keywords != none { [*Keywords:* #keywords.join("; ")] }
      ]
    }
  ]

  set figure(placement: auto, scope: "parent")
  show figure.where(kind: table): set figure.caption(position: top)
  show figure.where(kind: "suppl-table"): set figure.caption(position: top)
  show figure: it => {
    set block(spacing: 1.2em, breakable: true)
    show block: set align(left)
    set text(size: 0.8 * fontsize)
    set place(clearance: 4.5em)
    set figure(placement: none)
    set par(first-line-indent: 0em, justify: false, leading: linespace, spacing: linespace)
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
          ..counter(heading).at(el.location()),
        )#numbering(
          "1",
          ..counter(fig.where(kind: el.kind)).at(el.location()),
        )]
    } else if el != none and el.func() == fig and (el.kind == "table" or el.kind == "image") {
      link(el.location())[#el.supplement~#numbering(
          "1",
          ..counter(fig.where(kind: el.kind)).at(el.location()),
        )]
    } else {
      // Other references as usual.
      it
    }
  }

  show table.cell: set text(size: 0.9 * fontsize)
  set table(inset: (x: 4pt, top: 1.2em, bottom: 0.3em), stroke: none)
  set table.hline(stroke: 0.5pt)

  pagebreak()

  doc

  place.flush()

  bibliography(bib)

  appendix
}

#let figure_with_footnote(
  content,
  caption: none,
  kind: auto,
  lab: none,
  footnote: none,
) = {
  figure(
    [#show figure: set place(clearance: 0em)
      #figure(
        content,
        caption: caption,
        kind: kind,
      ) #label(lab)
      #footnote],
    kind: "none",
    supplement: none,
  )
}
