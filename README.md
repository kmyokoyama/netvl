# Building and installing the package from source

First, you need to get all the sources from GitHub. It can be done by cloning this repository

```
git clone git@github.com:kmyokoyama/tcc.git
```

From the parent directory of *tcc/*, run:

```
R CMD build tcc
```

in order to build the tar.gz file. Then, it's a good idea to check the tar.gz file to ensure that there's no
comprising errors:

```
R CMD check tcc_<version>.tar.gz
```

where <version> is the version of the package. If everything looks fine, you're ready to install it in R. In R console, type:

```
install.packages("pathtotargz", repos = NULL, type="source")
```

And it's done!
