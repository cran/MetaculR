

<!--- next entry here -->

## 0.2.0
2022-03-21

### Breaking changes

#### **cran:** Update Description field; add examples (e3e2af6b5fa677c73abaaaf773b2bf60a8cfdd64)

This commit changed functions with capital letters after the underscore to lowercase letters, e.g.,

- `MetaculR_Brier()` --> `MetaculR_brier()`
- `MetaculR_Questions()` --> `MetaculR_questions()`

# MetaculR 0.1.1
2022-03-21

## Fixes

- **.rbuildignore:** Add some unnecessary `httptest` files (6ea3a4e8ee95b7e6a4e03a971ab0de1cadc4f84b)
- **news.md:** Added NEWS.md and added to Site (4fff8e9fc037e52f2e1ebf94df892cce23bc60a8)
- **cran-comments.md:** Added more NOTES after `rhub::check_for_cran()` (fb246d2ef7b32f426de64f15b61d9aa2353e36c7)
- **cran:** Add CRAN-RELEASE to .Rbuildignore (0c23c006da8d1b7ddc0edc225a621f6eec7ca9a2)
- **site:** Add Site URL to README; update MetaculR.Rmd (382c6e99cf30f4bf2d2ad02ee4a6827ae8aedff5)
- **functions:** Add return values description (f71805f9c9ea05944c999386f70babd39d68d8f6)
- **metaculr_login:** Add user_agent; trim empty final page (0700722137837bc06731090daa365cebd7235e81)
- **site:** Build vignette and site (9f641bc77034c8b1b240ed4240a9b03bb3e59f95)
- **.gitlab-ci.yml:** Remove extra `devtools::check()` jobs (8690e9d249104c8087be643e495ff208f0ca5f45)
- **cran:** Remove extra test environments and fixed NOTE (1f3f549f8b415334a383c403c581961f2245c0df)
- **.gitlab-ci.yml:** Install pandoc in jobs (5dc7e5a0dd7c236a9cefec020b005942b3ee5a8b)
- **news.md:** Use NEWS.md with automated semantic versioning (cb3a7c488984892d5afc0b7cca7e3a9e7c57f122)
- **news.md:** Add tag for automated semantic versioning (65923936e87598bf779d25a8342f647e3bcfec60)

# MetaculR 0.1.0
2022-03-11

## Features

- Initialize project (da01a45c9e39e3bed9493658d78cd8236bcb65df)
- **site:** Add GitLab Pages to .gitlab-ci.yml (1b2f4c5fb056dc1f6d811b8b30c1e79dc40b133f)

## Fixes

- **site:** Remove `docs` from .gitignore (de31aa4ff16129d0a62653ad7d705a50988752b7)
- **.gitlab-ci.yml:** Installed devtools to run `devtools::check()` (bebaff5b67e12d6960cd85073922b886a7faa6bf)
- **.gitlab-ci.yml:** Remove `sudo` (ed26fca99eeb19b0d4a0b95e808169034b32eed1)
- **.gitlab-ci.yml:** Install dependencies (c5ca5e55aa713dc2b308e8334759295f2e22958c)
- **.gitlab-ci.yml:** Add rmarkdown to Check (b98b1594d4cdf5015acfaea90384474a34959aa0)
- **.rbuildignore:** Add go-semrel-gitlab files to be ignored (b6087d6c9eb6d47cdb55b565890e23e4397a6745)
- **news.md:** Use NEWS.md rather than CHANGELOG.md (025b2f66e51f9eb65a556ccac60c6a6259d25422)
- **site:** Update title, badges, pkgdown and bug links (751b45686a9141f7bed4f4e5a1f8a62062f07af1)
- **cran-comments:** Updated test images (d48d6e793cdf4a120425d4d74a9232acf9bcca82)
- **.gitlab-ci.yml:** Install dependencies differently (17d3d732643da3a9fa8ffb8325d8d5da83c6b3f8)
- **readme.md:** Update link to lifecyle definitions (77ed4502d940e8671600f9ca695d0ee7975d5fa5)
- **description:** Remove `LazyData: true` (e46f6e965a6339799936350b914830e58a8dbbda)
- **.gitlab-ci.yml:** Add `devtools::check_win_devel()` (c2fa1702db371c1a52033e0a44ac877289e3b177)
- **vignette:** Workaround to mock API calls (34b6f4318acd3a9de81d9705abc4d93dd733ac92)
- **site:** Build website with new vignette file (0efd3fd1871b88825d2b40db33e157859401207e)
- **man:** Updated roxygen notes for all functions (c2a604e204c54b2289a7cd8792dc8e9030c439aa)
- **cran-comments.md:** Added note about API authentication workaround (f7cf6035d07744c9bccf4c5288d092a9ade1ba6b)
- **.gitlab-ci.yml:** Add code coverage job (22dbd70cb89a69be01d70094f7f0901145935839)
- **.gitlab-ci.yml:** Update job dependency name (bc7c332a334a8b8789d9b125a94df47c73593822)
- **.gitlab-ci.yml:** Use `R` in script, not `Rscript` (e2b48bc9873a92908ee6fda4a51496679a7d3127)
- **.gitlab-ci.yml:** Add image to code coverage job (858c5d2da09a892f30890c2b4f425eec7fd9b700)
- **.gitlab-ci.yml:** Install 'covr' in job (11cd866348c86f7360a91e261d7f2e6b6be9a6fc)
- **.gitlab-ci.yml:** Add Debian packages needed for 'covr' (ccca15459245f7d1dc202df9c1b6be5e2a55189c)
- **.gitlab-ci.yml:** Add MetaculR dependencies to job (d041476db5dd9165b4ac2f4fd46c5d56f6e479b2)
- **.gitlab-ci.yml:** Add 'testthat' and 'devtools' to job (cf63056999a94abf0c1ae3fcb0ce6d522595579e)
- **.gitlab-ci.yml:** Use correct path, `/coverage` (2820435ae48c74190d16aac90b5882a28657759d)
- **site:** Updated with new function documentation (4b1a87911362328051de7389f52e85a93d11ea74)

<!--- downloads here -->
