#import "_templates/appendix.typ": appendix
#show: appendix

#let js = json("inputs-appendix.json")

= Appendix Title

Label figures as supplementary like this (so that in-text references refer to e.g. Figure A1 instead of Figure 1):

#figure(
  image(js.fig.primary),
  caption: [Caption for the figure],
  kind: "suppl-image"
) <fig-primary-supplement>

And tables likewise:

#figure(
  include js.tbl.primary,
  caption: [Caption for the table],
  kind: "suppl-table"
) <tbl-primary-supplement>
