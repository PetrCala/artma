# Offer to install missing suggested packages

In interactive sessions, offer to install the optional packages a method
needs before it runs. Returns the packages still missing afterwards (the
whole set unchanged when the user declines or the session is
non-interactive), so the caller can decide to skip the method.

## Usage

``` r
prompt_install_missing_packages(
  pkgs,
  method_name,
  is_installed = NULL,
  install_packages = NULL
)
```

## Arguments

- pkgs:

  *\[character\]* Packages the method suggests but that are absent.

- method_name:

  *\[character\]* Method the packages are needed for.

- is_installed:

  *\[function, optional\]* Predicate testing package availability.
  Injectable for testing; defaults to `requireNamespace`.

- install_packages:

  *\[function, optional\]* Installer. Injectable for testing; defaults
  to
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html).

## Value

*\[character\]* The packages that remain missing.
