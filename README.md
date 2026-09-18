# Applied Data Analysis

Slides and exercises for the course **Applied Data Analysis** at the
Faculty of National Economy, Bratislava University of Economics and Business.

Materials are available in two languages:

| | |
|---|---|
| [`en/`](en) | English version of every deck |
| [`sk/`](sk) | Slovenská verzia každej prezentácie |
| [`syllabus/`](syllabus) | Course syllabus (EN + SK), LaTeX sources and PDFs |

Each week folder holds a lecture deck (`_p`) and, where applicable, an exercise
deck (`_e`), written in [R Markdown](https://rmarkdown.rstudio.com/) with
[xaringan](https://github.com/yihui/xaringan). Slovak files carry the `_sk`
suffix. The decks are pre-rendered — open the `.html` files directly.

## Course outline

**Part I — Applied Data Analysis in R** (weeks 1–7)

| Week | Topic | Instructor |
|---|---|---|
| 1 | R basics, syntax, vectors, indexing, GitHub | T. Oleš |
| 2 | Data visualization with `ggplot2` | T. Oleš |
| 3 | Spatial visualization with `sf` and `tmap` | T. Oleš |
| 4 | Summarizing, tidying and joining data | T. Oleš |
| 5 | Text analysis: tokenization, sentiment, tf-idf | T. Oleš |
| 6 | Classification: logistic regression, LDA, naive Bayes | T. Oleš |
| 7 | Clustering: k-means, hierarchical, PCA | M. Vaško |

**Part II — Econometrics in a Nutshell** (weeks 8–13) is delivered as a separate
module by Ella Sargsyan, Ph.D. (CERGE-EI Foundation). Those materials are
distributed through the course Moodle page, not this repository.

## Class times

| | Slovak class (NND21210/21) | English class (NND21254/21) |
|---|---|---|
| Day | Tuesday, from 22 Sep 2026 | Wednesday, from 23 Sep 2026 |
| Lecture | 13:30–15:00, room 4B26 | 11:00–12:30, room 4B26 |
| Exercise | 15:00–16:30, room 4B26 | 13:30–15:00, room D203 |

## Rebuilding the slides

```bash
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript render_slides.R sk/7_week
```

A **UTF-8 locale is required**. In the default `C` locale, R's graphics devices
render Slovak diacritics inside plots as `<U+00E1>` instead of `á`. The HTML
around the plot still looks correct, so the corruption is easy to miss —
`render_slides.R` therefore refuses to run outside a UTF-8 locale.

## Acknowledgements

I am very thankful to [@datascience-box](https://datasciencebox.org/) and
everyone in the #rstats education community for providing numerous exercises and
teaching materials that are freely available and open source, and which form the
foundation of this course.

## License

[MIT](LICENSE)
