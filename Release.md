# How To Release
1.  Create a git tag using `git tag -a vX.Y.Z -m "vX.Y.Z"`
2. Push the tag using `git push origin vX.Y.Z` 
Doing so will create a Github Action build for the `X.Y.Z` release.  This Github Action builds the release, checks mima compatibility using the [strictSemVer](https://github.com/djspiewak/sbt-spiewak/blob/709fc19b389394777e61206e4d1b6df69e039e24/core/src/main/scala/sbtspiewak/SpiewakPlugin.scala#L400-L444) option, and then releases to Sonatype.
