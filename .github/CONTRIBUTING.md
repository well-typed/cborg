# Contributing

## Commits

Rules for contribution:

  * 80-character column maximum.
  * The first line of a commit message should be 73 columns max.
  * Always run tests. If benchmarks regress, give OS information,
    and we'll discuss.
  * Always reference the issue you're working on in the bug tracker
    in your commit message, and if it fixes the issue, close it.

You can use GitHub pull requests OR just email me patches directly
(see `git format-patch --help`,) whatever you are more comfortable with.

One nice aspect of submitting a pull request is that
[travis-ci.org](http://travis-ci.org) bots will automatically merge, build
and run tests against your commits, and continue as you update the request,
so you can be sure you didn't typo stuff or something before a final merge.

For multi-commit requests, your code may get squashed into the
smallest possible logical changes and commiting with author
attribution in some cases. In general, try to keep the history clean
of things like "fix typo" and "obvious build break fix", and this
won't normally be necessary.

### Notes on sign-offs and attributions, etc.

When you commit, **please use -s to add a Signed-off-by
line**. `Signed-off-by` is interpreted as a very weak statement of
ownership, much like Git itself: by adding it, you make clear that the
contributed code abides by the project license, and you are rightfully
contributing it yourself or on behalf of someone. You should always do
this.

This means that if the patch you submit was authored by someone else -- perhaps
a coworker for example that you submit it from or you revive a patch that
someone forgot about a long time ago and resubmit it -- you should also
include their name in the details if possible.

## Hacker notes

N/A.
