#import "_templates/appendix.typ": appendix
#show: appendix

#let r = json("appendix-inputs.json")
#let fig = json("fig.json")
#let tbl = json("tbl.json")

= Appendix Title

Label figures as supplementary like this (so that in-text references refer to e.g. Figure A1 instead of Figure 1):

#figure(
  image(fig.primary),
  caption: [Caption for the figure],
  kind: "suppl-image"
) <fig-primary-supplement>

And tables likewise:

#figure(
  include tbl.primary,
  caption: [Caption for the table],
  kind: "suppl-table"
) <tbl-primary-supplement>
