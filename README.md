# Software Quality Causes Project Survey Site

This site surveys people about their opinions on the quality of the software
they use. It may or may not be online at goodcode.cc. This is part of a larger
project studying the effects of programming languages, tools and practices on
software quality. The survey data collected here are the dependent variable to
our regression, while the programming languages, tools and practices used -
along with confounders - are the independent variables. There is a somewhat
out-of-date blog post about the project
[here](http://www.echonolan.net/posts/2016-05-19-notes-toward-an-empirical-study-of-programming-language-effectiveness.html).

# Contributing

If you'd like to contribute, you're very welcome to do so! Comment on a GitHub
issue, file a new one, or email me at echo@echonolan.net to tell me what you're
doing.

## Getting it running

For local dev, you need:

- stack
- docker
- docker-compose

First, copy `emp-pl-site_dev_env.sample` and `postgres_dev_env.sample` to those
same names without the `.sample` bit. Then open them up in a text editor and
fill in the commented out variables. You need to set up Google OAuth login. See
[this page](https://hackage.haskell.org/package/yesod-auth-1.4.17/docs/Yesod-Auth-GoogleEmail2.html)
for instructions.

Then:
- Get the Docker image for building the site `stack docker pull`
- Start the Postgres and Selenium containers `docker-compose up -d postgres selenium`
- Generate TLS key+cert `bash dev-certs/genkey.sh`
- Build the site `stack build`
- Build the devel server `stack build yesod-bin`

To run the devel server, run `stack exec --docker-run-args "-p 3443:3443" -- yesod devel`
It should be up at https://localhost:3443 now.

To run the tests, `stack test`. It takes a while.

It's deployed using Kubernetes onto GKE. If you need instructions for that for
some reason, file an issue.
