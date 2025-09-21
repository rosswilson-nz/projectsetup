# CMORprojects 0.9.0

* Add `save_table()` to save Typst table to source file

# CMORprojects 0.8.0

* Convert manuscripts to Typst source instead of Quarto

* Reformat codebase with Air and lintr

* Rename example targets with common prefixes

* Update targets config templates for `crew` parallel/distributed computing

* Add template `air.toml` to project working directory

# CMORprojects 0.7.1

* Update default .gitignore

* Add `create.dir` argument to `save_plot()`

* Rename `parameters.R` to `_config.R`

* Use `crew` for parallel/distributed computing

# CMORprojects 0.7.0

* Redesign `create_research_project()`

# CMORprojects 0.6.0

* Move all `typst_figure()` code from `ttables`

# CMORprojects 0.5.1

* Add `typst_figure()` wrapper around `ttables::tfig()`

# CMORprojects 0.5.0

* Deprecate `ttab()` and friends in favour of the new functions in the `ttables` package

* Add `save_plots()` to save plots to SVG figure files

# CMORprojects 0.4.0

* Correct set-up of new project folder when no user .Rprofile is found

* Change `tar_render_manuscript()` to use Typst instead of LaTeX/docx

* Add `appendix` option to `tar_render_manuscript()`

* Add `ttab()` for creating Typst tables in output

* Add `tfig()` for creating Typst figures in output

* Add Typst table and figure formatting functions

# CMORprojects 0.3.5

* Correct identification of linked Quarto source documents in subfolders

# CMORprojects 0.3.4

* Add `\ins{}`, `\del{}`, and `\mrk{}` commands to latex header template

* Correct dependency identification in linked Quarto source documents

# CMORprojects 0.3.3

* Automatically get dependencies for targets created with `tar_render_manuscript()` from 'include' statements in the Quarto source

# CMORprojects 0.3.2

* Minor revisions to `_targets.R` template

# CMORprojects 0.3.1

* Correct targets configuration in `_targets.R` template

# CMORprojects 0.3.0

* Include additional source files in `tar_render_manuscript()` via `include` argument (Quarto only)

* Update Quarto templates to use modular structure

* Add support for PDF output in `tar_render_manuscript()`

# CMORprojects 0.2.1

* Make Quarto rendering consistent with RMarkdown (rendered output moved to 'output' directory)

# CMORprojects 0.2.0

* Add support for rendering Quarto documents as well as RMarkdown

# CMORprojects 0.1.0

* Moved all functions over from `cmor.tools`

* Added a `NEWS.md` file to track changes to the package
