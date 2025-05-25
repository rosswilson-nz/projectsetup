#import "_templates/article.typ": article

#show: article.with(
  title: [Paper title],
  authors: (
    ( name: [Author 1],
      affiliation: ([Department, University, ...], ),
      email: "" ),
  ),
  date: datetime.today().display("[day] [month repr:long] [year]"),
  abstract: [
  _Background_ \

  _Methods_ \

  _Results_ \

  _Conclusion_
  ],
  appendix: [
    #pagebreak()
    #include "appendix.typ"
  ]
)

#let r = json("manuscript-inputs.json")
#let fig = json("fig.json")
#let tbl = json("tbl.json")

= Introduction <introduction>

= Methods <methods>

= Results <results>

Refer to results imported from R inline, e.g. the primary result was #r.primary_result.

Add figures like this:

#figure(
  image(fig.primary),
  caption: [Caption for the figure]
) <fig-primary>

And tables like this:

#figure(
  include tbl.primary,
  caption: [Caption for the table]
) <tbl-primary>

= Discussion <discussion>

== Conclusion <conclusion>

= Funding <funding>

= Conflicts of Interest <coi>
