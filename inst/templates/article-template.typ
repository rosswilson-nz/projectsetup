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
  font: ("Charter", "Century Schoolbook", "Wickliffe", "Libertinus Serif"),
  fontsans: ("Source Sans 3", "Wickliffe Sans", "Gill Sans MT"),
  fontmono: ("Source Code Pro", "Consolas", "DejaVu Sans Mono"),
  fontsize: 10pt,
  bib: "../references.bib",
  bibliographystyle: "american-medical-association",
  sectionnumbering: none,
  appendix: [],
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    columns: if draft { 1 } else { cols },
    numbering: "1",
    background: if background != none {
      background
    } else if draft {
      rotate(56deg, text(80pt, fill: rgb("FFCBC4"))[DRAFT])
    } else {
      none
    },
    header: if header != none {
      header
    } else if draft {
      align(center)[_DRAFT MANUSCRIPT: DO NOT CITE_]
    } else {
      none
    },
  )
  set columns(gutter: 1cm)
  set par(
    leading: if draft { 1.55em } else { 0.45em },
    first-line-indent: 1em,
    justify: true,
    spacing: if draft { 1.55em } else { 0.45em },
  )
  set text(
    lang: lang,
    region: region,
    font: font,
    size: fontsize,
    bottom-edge: "descender",
    top-edge: "ascender",
  )
  show raw: set text(font: fontmono, size: 0.9 * fontsize, weight: "medium")
  set heading(numbering: sectionnumbering)
  show heading: set block(
    above: if draft { 30pt } else { 16pt },
    below: if draft { 10pt } else { 5pt },
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
        v(-13pt) // amount to reduce spacing, could make this dependent on it.level
      }
    }
    it
  }
  show heading: set text(font: fontsans, weight: "medium")
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
      )[
        #text(font: fontsans, weight: "medium", size: 2em)[#title]
      ]]]
    }

    #if subtitle != none {
      block(inset: (top: 1cm), width: 100%)[#align(center)[#par(justify: false)[
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
      footnote(numbering: x => [#sym.zws])[
        #affiliations_fn.join(linebreak())#linebreak()#linebreak()
      ]
      counter(footnote).update(0)
      let names = authors
        .map(author => {
          let affiliation = author
            .affiliation
            .map(
              aff => str(affiliations.position(i => aff == i) + 1),
            )
            .join(",")
          [
            #author.name#if author.email != "" {
              [#footnote(numbering: "*")[
                Corresponding author.
                #linebreak()
                _Email_: #link("mailto:" + author.email.replace("\\", ""))
              ]]
            }#super[#affiliation]
          ]
        })
        .join(", ")
      block(inset: (top: if draft { 0cm } else { 2.5cm }), width: 100%)[#align(center)[#par(
        justify: false,
      )[#names]]]
    }

    #if date != none {
      block(inset: (top: 0.5cm), width: 100%)[#align(center)[#date]]
    }

    #if abstract != none {
      block(inset: (x: 2cm, top: if draft { 0.5cm } else { 2cm }), width: 100%)[
        #set par(
          first-line-indent: 0em,
          spacing: if draft { 1.55em } else { 1em },
          leading: if draft { 1em } else { 0.45em },
        )
        *Abstract*

        #abstract

        #if keywords != none { [*Keywords:* #keywords.join("; ")] }
      ]
    }
  ]

  set figure(placement: auto)
  show figure.where(kind: table): set figure.caption(position: top)
  show figure.where(kind: "suppl-table"): set figure.caption(position: top)
  show figure: it => {
    set block(spacing: 0.65em, breakable: true)
    show block: set align(left)
    set text(size: 0.8 * fontsize)
    set place(clearance: 3em)
    set figure(placement: none)
    set par(first-line-indent: 0em, justify: false, leading: 0.45em)
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
  set table(inset: (x: 6pt, y: 0.3em), stroke: none)
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
