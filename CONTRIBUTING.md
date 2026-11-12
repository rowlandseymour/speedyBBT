# Contributing to speedyBBT

> Contributing guidelines for speedyBBT

First off, thanks for taking the time to contribute!

## How to report a bug

Open an issue and include:

- Which version you are on (the version number shown by your package manager or run `packageVersion("speedyBBT")` in your R console).
- The operating system and hardware environment.
- Reproduction steps — the smallest snippet or project that triggers the problem. Please use the [reprex](https://reprex.tidyverse.org/) 
 package to generate your reprex.
- What you expected to happen and what actually happened.
- Any relevant logs or screenshots.

## How to request a feature

Open an issue and describe:

- The problem the feature would solve (not just the solution you have in mind).
- Why it matters for others, not only for your use case.
- A rough sketch of the expected behaviour.
- Whether you can help implement it yourself.

The maintainers triage proposals in the issue tracker and label accepted ones as `help-wanted` or `good-first-issue` when they are a good on-ramp.

## Good first issues

New to this project? Start with an issue labelled **good-first-issue** or **help-wanted**.

Those issues are small, well-scoped and have enough context to pick up without deep knowledge of the codebase. Leave a comment to say you are working on it, so nobody else grabs the same task.

## Benefits of contributing

Contributing is a skill like any other, and this project is a good place to build it. You get:

- Thoughtful code review from maintainers who read your work.
- Practice writing tests, documentation and clear commit messages.
- Attribution in the project’s commit history and contributors file.
- A chance to shape a piece of open source used by real researchers.

## Development setup

Create a fork of the package on GitHub, and then clone the fork locally using `git clone`.
You will need to replace `rowlandseymour` in the below with your GitHub username.

```
git clone https://github.com/rowlandseymour/speedyBBT.git
git checkout -b name_of_feature
```

Make any changes to the files and save them. Run the test suite before and after your changes:

```r
install.packages("devtools")
devtools::test()
```

Before pushing your changes, please re-document the package and use `styler` to match the styling:

```r
devtools::document()
devtools::build_readme()

install.packages("styler")
styler::style_pkg()
```

Build the project to check the package can be installed:

```r
devtools::install()
```

To preview your changes on the `pkgdown` site, use:

```r
pkgdown::build_site()
```

## Communication

- Slack: https://github.com/rowlandseymour/speedyBBT/issues
- Issue tracker: https://github.com/rowlandseymour/speedyBBT

## Commit conventions

## Submitting a pull request

1. Fork the repository and create a branch from `main`, for example `fix/new-model`.
2. Make your change; keep it focused and commit with the conventions from the section above.
3. Run the tests, styler, and test the install as described in Development setup.
4. Push the branch and open a pull request.
5. Maintainers review it — expect a few rounds of feedback. Address the comments and push updates.
6. Once approved, a maintainer merges it. Thank you for your contribution!

**This contributors guide was adapted from the template at https://toolwasp.com/contributing-md-generator**