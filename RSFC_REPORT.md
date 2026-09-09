# Quality Assessment for speedyBBT v1.0

An automated assessment of the speedyBBT tool based on the EVERSE
software quality indicators, run on 2026-09-09.

## General Information

- **Software:** speedyBBT
- **Repository:** <https://github.com/rowlandseymour/speedyBBT>
- **Assessment date:** 2026-09-09T11:18:50Z
- **Total checks:** 42

## Summary

- **Passed (`true`)**: 17
- **Failed (`false`)**: 24
- **Errors (`error`)**: 1

## Results Table

| Test ID | Test Name | Result |
|----|----|----|
| <https://w3id.org/rsfc/test/RSFC-01-1> | There is an identifier and resolves | [false](#persistent_and_unique_identifier-https--w3idorg-rsfc-test-rsfc-01-1) |
| <https://w3id.org/rsfc/test/RSFC-01-2> | There is an identifier associated with the software | [false](#persistent_and_unique_identifier-https--w3idorg-rsfc-test-rsfc-01-2) |
| <https://w3id.org/rsfc/test/RSFC-01-3> | Software identifier follows a proper schema | [false](#persistent_and_unique_identifier-https--w3idorg-rsfc-test-rsfc-01-3) |
| <https://w3id.org/rsfc/test/RSFC-03-1> | Software has releases | [true](#has_releases-https--w3idorg-rsfc-test-rsfc-03-1) |
| <https://w3id.org/rsfc/test/RSFC-03-2> | Releases have an id and version number | [true](#has_releases-https--w3idorg-rsfc-test-rsfc-03-2) |
| <https://w3id.org/rsfc/test/RSFC-03-3> | Release versions follow a community established convention | [false](#versioning_standards_use-https--w3idorg-rsfc-test-rsfc-03-3) |
| <https://w3id.org/rsfc/test/RSFC-03-4> | Release identifiers follow the same scheme | [true](#has_releases-https--w3idorg-rsfc-test-rsfc-03-4) |
| <https://w3id.org/rsfc/test/RSFC-03-5> | Last release consistency | [error](#has_releases-https--w3idorg-rsfc-test-rsfc-03-5) |
| <https://w3id.org/rsfc/test/RSFC-03-6> | Version number in metadata | [false](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-03-6) |
| <https://w3id.org/rsfc/test/RSFC-04-1> | Metadata exists | [false](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-04-1) |
| <https://w3id.org/rsfc/test/RSFC-04-2> | There is a README | [true](#software_has_documentation-https--w3idorg-rsfc-test-rsfc-04-2) |
| <https://w3id.org/rsfc/test/RSFC-04-3> | There are title and description | [true](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-04-3) |
| <https://w3id.org/rsfc/test/RSFC-04-4> | Software has descriptive metadata | [false](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-04-4) |
| <https://w3id.org/rsfc/test/RSFC-04-5> | There is a codemeta file | [true](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-04-5) |
| <https://w3id.org/rsfc/test/RSFC-05-1> | There is a repostatus badge | [false](#version_control_use-https--w3idorg-rsfc-test-rsfc-05-1) |
| <https://w3id.org/rsfc/test/RSFC-05-2> | There is contact and/or support metadata | [false](#software_has_documentation-https--w3idorg-rsfc-test-rsfc-05-2) |
| <https://w3id.org/rsfc/test/RSFC-05-3> | Software documentation | [true](#software_has_documentation-https--w3idorg-rsfc-test-rsfc-05-3) |
| <https://w3id.org/rsfc/test/RSFC-06-1> | Authors are declared | [true](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-06-1) |
| <https://w3id.org/rsfc/test/RSFC-06-2> | Contributors are declared | [false](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-06-2) |
| <https://w3id.org/rsfc/test/RSFC-06-3> | Authors have an ORCID | [false](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-06-3) |
| <https://w3id.org/rsfc/test/RSFC-06-4> | Authors have roles | [false](#descriptive_metadata-https--w3idorg-rsfc-test-rsfc-06-4) |
| <https://w3id.org/rsfc/test/RSFC-07-1> | There is an identifier in README or CITATION.cff | [false](#persistent_and_unique_identifier-https--w3idorg-rsfc-test-rsfc-07-1) |
| <https://w3id.org/rsfc/test/RSFC-07-2> | Software identifier resolves to software | [false](#persistent_and_unique_identifier-https--w3idorg-rsfc-test-rsfc-07-2) |
| <https://w3id.org/rsfc/test/RSFC-08-1> | Metadata record in Software Heritage or Zenodo | [false](#archived_in_software_heritage-https--w3idorg-rsfc-test-rsfc-08-1) |
| <https://w3id.org/rsfc/test/RSFC-09-1> | Repository is from Github/Gitlab | [true](#version_control_use-https--w3idorg-rsfc-test-rsfc-09-1) |
| <https://w3id.org/rsfc/test/RSFC-12-1> | There is an article citation or reference publication | [false](#software_has_citation-https--w3idorg-rsfc-test-rsfc-12-1) |
| <https://w3id.org/rsfc/test/RSFC-13-1> | Dependencies are declared | [false](#requirements_specified-https--w3idorg-rsfc-test-rsfc-13-1) |
| <https://w3id.org/rsfc/test/RSFC-13-2> | There are installation instructions | [true](#software_has_documentation-https--w3idorg-rsfc-test-rsfc-13-2) |
| <https://w3id.org/rsfc/test/RSFC-13-3> | Dependencies have version numbers | [false](#requirements_specified-https--w3idorg-rsfc-test-rsfc-13-3) |
| <https://w3id.org/rsfc/test/RSFC-13-4> | There is a dependencies machine-readable file | [false](#requirements_specified-https--w3idorg-rsfc-test-rsfc-13-4) |
| <https://w3id.org/rsfc/test/RSFC-14-1> | Presence of tests in repository | [true](#software_has_tests-https--w3idorg-rsfc-test-rsfc-14-1) |
| <https://w3id.org/rsfc/test/RSFC-14-2> | There are actions to automate tests | [true](#repository_workflows-https--w3idorg-rsfc-test-rsfc-14-2) |
| <https://w3id.org/rsfc/test/RSFC-15-1> | Software has license | [true](#software_has_license-https--w3idorg-rsfc-test-rsfc-15-1) |
| <https://w3id.org/rsfc/test/RSFC-15-2> | License is SPDX compliant | [false](#software_has_license-https--w3idorg-rsfc-test-rsfc-15-2) |
| <https://w3id.org/rsfc/test/RSFC-16-1> | License referenced in metadata files | [false](#software_has_license-https--w3idorg-rsfc-test-rsfc-16-1) |
| <https://w3id.org/rsfc/test/RSFC-17-1> | Repository active | [false](#version_control_use-https--w3idorg-rsfc-test-rsfc-17-1) |
| <https://w3id.org/rsfc/test/RSFC-17-2> | Commit history | [true](#version_control_use-https--w3idorg-rsfc-test-rsfc-17-2) |
| <https://w3id.org/rsfc/test/RSFC-17-3> | Commits are linked to issues | [false](#version_control_use-https--w3idorg-rsfc-test-rsfc-17-3) |
| <https://w3id.org/rsfc/test/RSFC-18-1> | Repository has citation | [true](#software_has_citation-https--w3idorg-rsfc-test-rsfc-18-1) |
| <https://w3id.org/rsfc/test/RSFC-19-1> | Repository has workflows | [true](#repository_workflows-https--w3idorg-rsfc-test-rsfc-19-1) |
| <https://w3id.org/rsfc/test/RSFC-20-1> | Repository has an issue tracker | [true](#support_issue_tracking-https--w3idorg-rsfc-test-rsfc-20-1) |
| <https://w3id.org/rsfc/test/RSFC-21-1> | Repository has contribution guidelines | [false](#has_contribution_guidelines-https--w3idorg-rsfc-test-rsfc-21-1) |

## Detailed Results by Indicator

### archived_in_software_heritage

\#### Metadata record in Software Heritage or Zenodo

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-08-1>
- **Result:** false
- **Process:** Searches for Zenodo and Software Heritage badges in the
  README file of the repository
- **Evidence:** Could not find neither a Zenodo DOI identifier or a
  Software Heritage badge in the repository
- **Suggestions:** You should archive your software not only in
  Github/Gitlab. More information at
  <https://everse.software/RSQKit/archiving_software>

### descriptive_metadata

\#### Version number in metadata

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-03-6>
- **Result:** false
- **Process:** Checks if a version number for the software is indicated
  in the CITATION.cff, codemeta.json or package
  files(i.e. pyproject.toml, pom.xml, etc.)
- **Evidence:** Could not find a version number for the software in any
  of the specified files
- **Suggestions:** You should include the version of your software in
  its metadata. More information at
  <https://everse.software/RSQKit/software_metadata>

\#### Metadata exists

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-04-1>
- **Result:** false
- **Process:** Searches for codemeta, citation and package files in the
  repository
- **Evidence:** Could not find any of the following metadata files: cff,
  package_file
- **Suggestions:** You should describe your software in metadata files.
  More information at <https://everse.software/RSQKit/software_metadata>

\#### There are title and description

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-04-3>
- **Result:** true
- **Process:** Checks if there is a title and a description for the
  software in the metadata
- **Evidence:** Title and description were found in the repository
- **Suggestions:** No suggestions

\#### Software has descriptive metadata

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-04-4>
- **Result:** false
- **Process:** Searches for description, programming languages, date of
  creation and keywords in the repository
- **Evidence:** Could not find any of the following metadata: keywords
- **Suggestions:** You should describe your software using metadata.
  More information at <https://everse.software/RSQKit/software_metadata>

\#### There is a codemeta file

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-04-5>
- **Result:** true
- **Process:** Searches for a codemeta.json file in the repository
- **Evidence:** A codemeta.json file was found in the root of the
  repository
- **Suggestions:** No suggestions

\#### Authors are declared

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-06-1>
- **Result:** true
- **Process:** Searches for authors in various files of the repository
  (i.e. CITATION.cff, AUTHORS.md, codemeta.json)
- **Evidence:** Authors were found in the repository
- **Suggestions:** No suggestions

\#### Contributors are declared

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-06-2>
- **Result:** false
- **Process:** Searches for contributors in various files of the
  repository (i.e. codemeta.json, pyproject.toml, pom.xml)’
- **Evidence:** Found authors but could not find any contributors in the
  repository
- **Suggestions:** Your software should also document its contributors
  if there are any. More information at
  <https://everse.software/RSQKit/documenting_software_project>

\#### Authors have an ORCID

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-06-3>
- **Result:** false
- **Process:** Checks if all authors stated in the CITATION.cff file
  have an ORCID assigned
- **Evidence:** One or more authors do not have an ORCID assigned
- **Suggestions:** When documenting your software’s authors, you should
  include their ORCIDs if possible.

\#### Authors have roles

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-06-4>
- **Result:** false
- **Process:** Checks if all authors stated in a codemeta.json file have
  a role assigned
- **Evidence:** There are one or more authors in the codemeta file that
  do not have roles assigned
- **Suggestions:** When documenting your software’s authors, you should
  include their roles if possible.

### has_contribution_guidelines

\#### Repository has contribution guidelines

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-21-1>
- **Result:** false
- **Process:** Checks if there are contribution guidelines either in the
  README file or if there is a CONTRIBUTING.md file
- **Evidence:** Could not find contribution guidelines in the repository
- **Suggestions:** If you want to properly keep track of the
  colaborations your project receives to ensure its quality and
  fiability, you should add some contribution guidelines so the
  colaborators know how you want contributions to be made

### has_releases

\#### Software has releases

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-03-1>
- **Result:** true
- **Process:** Searches for release tags in the repository
- **Evidence:** These releases were found:
  - speedyBBT 1.0
- **Suggestions:** No suggestions

\#### Releases have an id and version number

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-03-2>
- **Result:** true
- **Process:** Checks if all of the releases have an identifier and a
  version
- **Evidence:** All of the releases have an id and a version
- **Suggestions:** No suggestions

\#### Release identifiers follow the same scheme

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-03-4>
- **Result:** true
- **Process:** Checks if all of the version identifiers follow the same
  scheme
- **Evidence:** All of the releases URLs follow the same scheme
- **Suggestions:** No suggestions

\#### Last release consistency

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-03-5>
- **Result:** error
- **Process:** Checks if the latest release tag matches the version
  stated in the package file of the repository
- **Evidence:** Could not get the necessary information to perform the
  test, it being releases and/or version in package file
- **Suggestions:** None

### persistent_and_unique_identifier

\#### There is an identifier and resolves

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-01-1>
- **Result:** false
- **Process:** Searches for an identifier (i.e. DOI or SWHID) in the
  README file of the repository
- **Evidence:** Could not find any identifier in the repository
- **Suggestions:** You should include a resolvable, unique and
  persistent identifier in your README file. More information at
  <https://everse.software/RSQKit/software_identifiers>

\#### There is an identifier associated with the software

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-01-2>
- **Result:** false
- **Process:** Searches for an identifier in the CITATION.cff,
  codemeta.json and README files
- **Evidence:** Could not find an identifier in any of the CITATION,
  codemeta or README files
- **Suggestions:** Remember that identifiers should be included in other
  files aside from README like codemeta.json, CITATION.cff. More
  information at <https://everse.software/RSQKit/software_identifiers>

\#### Software identifier follows a proper schema

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-01-3>
- **Result:** false
- **Process:** Checks if the identifiers associated with the software
  follow any of these schemas: DOI, URN, GITHUB and SWHID
- **Evidence:** Could not find any identifier in the README file
- **Suggestions:** You should include a resolvable, unique and
  persistent identifier in your README file. More information at
  <https://everse.software/RSQKit/software_identifiers>

\#### There is an identifier in README or CITATION.cff

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-07-1>
- **Result:** false
- **Process:** Searches for an identifier in the README or CITATION.cff
  files of the repository
- **Evidence:** Could not find an identifier in neither of the README or
  CITATION files in the repository
- **Suggestions:** You should include your software’s identifier in your
  README or CITATION.cff files. More information at

\#### Software identifier resolves to software

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-07-2>
- **Result:** false
- **Process:** Checks if the identifier found in the README file or
  metadata files (i.e. codemeta.json, CITATION.cff) resolves to a page
  that links back to the software repository
- **Evidence:** Could not find any identifier in the repository
- **Suggestions:** You should include a resolvable, unique and
  persistent identifier in your README file. More information at
  <https://everse.software/RSQKit/software_identifiers>

### repository_workflows

\#### There are actions to automate tests

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-14-2>
- **Result:** true
- **Process:** Searches for workflows that contain test or tests in
  their names
- **Evidence:** There are workflows or actions that perform automated
  tests
- <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/.github/workflows/slow-tests.yml>
- **Suggestions:** No suggestions

\#### Repository has workflows

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-19-1>
- **Result:** true
- **Process:** Searches for workflows in the repository
- **Evidence:** Workflows were found in:
  - <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/.github/workflows/generate-codemeta.yml>
  - <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/.github/workflows/fair-assessment.yml>
  - <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/.github/workflows/slow-tests.yml>
- **Suggestions:** No suggestions

### requirements_specified

\#### Dependencies are declared

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-13-1>
- **Result:** false
- **Process:** Searches for dependencies in project configuration files,
  README and dependencies files such as requirements.txt
- **Evidence:** Could not find any dependencies indicated in the
  repository
- **Suggestions:** You should have your dependencies stated somewhere to
  enable reproducibility. More information at
  <https://everse.software/RSQKit/reproducible_software_environments>

\#### Dependencies have version numbers

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-13-3>
- **Result:** false
- **Process:** Checks if all of the dependencies stated in the
  machine-readable file (e.g. requirements.txt, pyproject.toml, etc.) of
  the repository have a version indicated
- **Evidence:** Could not find any dependencies indicated in the
  repository
- **Suggestions:** You should have your dependencies stated somewhere to
  enable reproducibility. More information at
  <https://everse.software/RSQKit/reproducible_software_environments>

\#### There is a dependencies machine-readable file

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-13-4>
- **Result:** false
- **Process:** Checks if dependencies are indicated in a
  machine-readable file
- **Evidence:** Could not find any dependencies indicated in the
  repository
- **Suggestions:** You should have your dependencies stated somewhere to
  enable reproducibility. More information at
  <https://everse.software/RSQKit/reproducible_software_environments>

### software_has_citation

\#### There is an article citation or reference publication

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-12-1>
- **Result:** false
- **Process:** Searches for an article citation or a reference
  publication in the codemeta and citation files
- **Evidence:** Could not find neither a reference publication or
  citation to an article in the repository
- **Suggestions:** You should include other forms of citation like
  article citations and reference publications in your software’s
  metadata. More information at
  <https://everse.software/RSQKit/creating_good_readme>

\#### Repository has citation

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-18-1>
- **Result:** true
- **Process:** Searches for a CITATION.cff file and README file in the
  repository
- **Evidence:** A citation was found in:
  - <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/citation.cff>
  - <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/README.Rmd>
- **Suggestions:** No suggestions

### software_has_documentation

\#### There is a README

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-04-2>
- **Result:** true
- **Process:** Searches for a README file in the repository
- **Evidence:** There is a README file in the repository
- **Suggestions:** No suggestions

\#### There is contact and/or support metadata

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-05-2>
- **Result:** false
- **Process:** Searches for contact and support information in the
  repository
- **Evidence:** Could not find any of the following information:
  contact, support, support_channels
- **Suggestions:** You should include contact information in your
  software’s metadata in case someone wants to ask for information.

\#### Software documentation

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-05-3>
- **Result:** true
- **Process:** Searches for a README file in the root repository and
  other forms of documentation such as a Read The Docs badge or url
- **Evidence:** Documentation was found in:  
- <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/README.Rmd>  
- <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/README.md>
- **Suggestions:** No suggest

\#### There are installation instructions

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-13-2>
- **Result:** true
- **Process:** Searches for installation instructions in the README file
  of the repository
- **Evidence:** Installation instructions were found in the repository
- **Suggestions:** No suggestions

### software_has_license

\#### Software has license

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-15-1>
- **Result:** true
- **Process:** Searches for a file named ‘LICENSE’ or ‘LICENSE.md’ in
  the root of the repository.
- **Evidence:** A license was found in:
  - <https://raw.githubusercontent.com/rowlandseymour/speedyBBT/main/LICENSE.md>
- **Suggestions:** No suggestions

\#### License is SPDX compliant

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-15-2>
- **Result:** false
- **Process:** Checks if the licenses detected are SPDX compliant
- **Evidence:** There is one or more licenses that are not SPDX
  compliant
- **Suggestions:** You should include SPDX tags to ensure that your
  licenses are machine-readable. More information at
  <https://everse.software/RSQKit/licensing_software>

\#### License referenced in metadata files

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-16-1>
- **Result:** false
- **Process:** Searches for licensing information in the codemeta,
  citation and package files if they exist
- **Evidence:** Could not find any licensing information in the
  following metadata files: citation, package
- **Suggestions:** Information about your license should be present in
  other metadata files like codemeta.json, package files or CITATION.
  More information on <https://everse.software/RSQKit/software_metadata>

### software_has_tests

\#### Presence of tests in repository

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-14-1>
- **Result:** true
- **Process:** Searches for files and/or directories that mention test
  in their names
- **Evidence:** Files and/or directories that mention test were found
  at:  
- .github/workflows/slow-tests.yml  
- .github/workflows/test-coverage.yaml  
- tests
- tests/longtests  
- tests/longtests/darTiedModelMeansFullRun.csv  
- tests/longtests/test-mcmc-long.R  
- tests/testthat.R  
- tests/testthat  
- tests/testthat/darTiedModelMeansShort.csv
- tests/testthat/forcedMarriageModelMeans.csv  
- tests/testthat/forcedMarriageModelMeansNoFormula.csv  
- tests/testthat/setup.R  
- tests/testthat/test-01-parameters.R  
- tests/testthat/test-02-matrices.R
- tests/testthat/test-03-mcmc.R
- tests/testthat/wimbledonModelMeans.csv
- **Suggestions:** No suggestions

### support_issue_tracking

\#### Repository has an issue tracker

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-20-1>
- **Result:** true
- **Process:** Checks if there is an issue tracker in the repository.
- **Evidence:** Found an issue tracker in the repository
- **Suggestions:** No suggestions

### version_control_use

\#### There is a repostatus badge

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-05-1>
- **Result:** false
- **Process:** Searches for a repo status badge in the README file of
  the repository
- **Evidence:** Could not find a repo status badge in the repository
- **Suggestions:** You should include the state of your repository in
  the README file

\#### Repository is from Github/Gitlab

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-09-1>
- **Result:** true
- **Process:** Checks if the URL provided is indeed a Github or Gitlab
  repository
- **Evidence:** URL provided is a Github or Gitlab repository
- **Suggestions:** No suggestions

\#### Repository active

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-17-1>
- **Result:** false
- **Process:** Checks if there is a repo_status badge with value Active
  and if there are commits in the repository
- **Evidence:** Could not find a repo status badge in the repository
- **Suggestions:** You should keep your repository active and indicate
  it with a repostatus badge

\#### Commit history

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-17-2>
- **Result:** true
- **Process:** Checks if the software repository has a commits history
- **Evidence:** Commits were found in the repository
- **Suggestions:** No suggestions

\#### Commits are linked to issues

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-17-3>
- **Result:** false
- **Process:** Checks if there is at least one of the existing issues
  (opened or closed) referenced in any of the commits made in the
  default branch of the repository
- **Evidence:** There is not any commits linked to any issues in the
  repository
- **Suggestions:** It is good practice to indicate in your commits which
  issues you are targeting or solving

### versioning_standards_use

\#### Release versions follow a community established convention

- **Test ID:** <https://w3id.org/rsfc/test/RSFC-03-3>
- **Result:** false
- **Process:** Checks if all of the releases versions follow the SemVer
  or CalVer versioning standards
- **Evidence:** There is one version number of a release that does not
  follow either SemVer or CalVer
- **Suggestions:** You should use a versioning standard for all of your
  releases. More information at
  <https://everse.software/RSQKit/releasing_software>
