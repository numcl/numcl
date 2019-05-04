# Developer's Certificate of Origin

To handle the legal aspects of contribution, we ask each Pull Request submitter
to sign the Developer's Certificate of Origin 1.1 (DCO) [1] that the Linux®
Kernel community uses [2] to manage code contributions.

[1] legal/DCO1.1.txt
[2] https://elinux.org/Developer_Certificate_Of_Origin

We simply ask that when submitting a patch for review, the developer must
include a sign-off statement in the commit message.

Here is an example Signed-off-by line, which indicates that the submitter
accepts the DCO linked above:

    Signed-off-by: Your Name <your@mail.org>

As of now, you need to add a sign-off message to every commit. To reduce the burden,
we recommend squashing the commits in the PR as much as possible, or use the following:

You can include this automatically when you commit a change to your local git
repository using the following command:

    git commit -s

Alternatively/in addition you can have a template for your commit text in a file
like git-commit-template with the DCO above, and run `git config commit.template
git-commit-template` once to have it as your default when running `git commit`.

