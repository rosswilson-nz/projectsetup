#let appendix(doc) = {
  show heading.where(level: 1): set heading(numbering: (..nums) => [
    Appendix #numbering("A:", nums.pos().at(0))
  ])
  show heading.where(level: 1): set block(above: 0em, below: 2em)
  show heading.where(level: 1): it => {
    pagebreak()
    it
  }

  set figure(placement: none)
  show figure.where(kind: "suppl-table"): set figure(
    numbering: (..nums) => [#context { numbering("A", counter(heading).get().at(0)) }#numbering(
        "1",
        nums.pos().at(0),
      )],
    supplement: "Table",
  )
  show figure.where(kind: "suppl-image"): set figure(
    numbering: (..nums) => [#context { numbering("A", counter(heading).get().at(0)) }#numbering(
        "1",
        nums.pos().at(0),
      )],
    supplement: "Figure",
  )

  set page(columns: 1, margin: (x: 2.5cm, y: 2cm))

  doc
}
