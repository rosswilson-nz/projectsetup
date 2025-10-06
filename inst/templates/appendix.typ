#import "_templates/appendix.typ": appendix
#import "_templates/article.typ": figure_with_footnote
#show: appendix

#let r = json("appendix-inputs.json")
#let fig = json("fig.json")
#let tbl = json("tbl.json")

= Appendix Title

Label figures as supplementary like this (so that in-text references refer to e.g. Figure A1 instead
of Figure 1):

#figure(
  image(fig.primary),
  caption: [Caption for the figure],
  kind: "suppl-image",
) <fig-supplement>

Figures with footnotes should use the special `figure_with_footnote()` function to place the
footnotes underneath but within the overall figure object:

#figure_with_footnote(
  image(fig.primary),
  caption: [Caption for the figure],
  kind: "suppl-image",
  lab: "fig-supplement-with-footnote",
  footnote: [Footnote(s) for the figure],
)

And tables likewise (no special treatment needed for footnotes):

#figure(
  include tbl.primary,
  caption: [Caption for the table],
  kind: "suppl-table",
) <tbl-primary-supplement>
