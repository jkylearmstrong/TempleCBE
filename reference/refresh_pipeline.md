# Refresh Modification Timestamps Across Pipeline Objects

Traverses pipeline objects (`FilePath`, `FileUses`, `FileOutputs`) and
inspects files on disk to update `@mtime` to the current disk timestamp
(or `as.POSIXct(NA)` if missing/deleted). Recursively refreshes
`@dependencies` and `@output`.

## Usage

``` r
refresh_pipeline(x)
```

## Arguments

- x:

  A `FilePath`, `FileUses`, `FileOutputs`, or list of such objects.

## Value

The refreshed pipeline object(s).
