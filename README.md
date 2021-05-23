Repro code for GHC-9 performance issues in streamly. Each issue is on a
separate branch:

* `unfoldrM`: the most basic, rewrite rule related issue
* `postscan`: postscan combinator related issue
* `after`: the `after` combinator related issue
* `splitOnSeq`: the `splitOnSeq` combinator related issue
