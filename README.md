# Overview 

Bionitio provides a template for command line bioinformatics tools in various programming languages.

In each language we implement a simple tool that carries out a basic bioinformatics task.
The program reads one or more input FASTA files, computes a variety of simple statistics on each file, and prints a tabulated output.

The purpose of the tool is to provide an easy-to-understand working example that is built on best-practice software engineering principles. It can be used as a basis for learning and as a solid foundation for starting new projects. We provide a script called `bionitio-boot.sh` for starting new projects from bionitio, which saves time and ensures good programming practices are adopted from the beginning (see below for details).

An additional advantage of bionitio is that it allows us to compare programming styles in different languages and programming paradigms.

## Languages

| Language | Repository | Travis Testing Status |
|----------|-----------------------|------------|
| C        | <https://github.com/bionitio-team/bionitio-c> | [![travis](https://travis-ci.org/bionitio-team/bionitio-c.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-c) | 
| C++      | <https://github.com/bionitio-team/bionitio-cpp> | [![travis](https://travis-ci.org/bionitio-team/bionitio-cpp.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-cpp) | 
| C#       | <https://github.com/bionitio-team/bionitio-csharp> | [![travis](https://travis-ci.org/bionitio-team/bionitio-csharp.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-csharp) | 
| Clojure  | <https://github.com/bionitio-team/bionitio-clojure> | [![travis](https://travis-ci.org/bionitio-team/bionitio-clojure.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-clojure) | 
| Java     | <https://github.com/bionitio-team/bionitio-java> | [![travis](https://travis-ci.org/bionitio-team/bionitio-java.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-java) | 
| Javascript | <https://github.com/bionitio-team/bionitio-js> | [![travis](https://travis-ci.org/bionitio-team/bionitio-js.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-js) | 
| Haskell  | <https://github.com/bionitio-team/bionitio-haskell> | [![travis](https://travis-ci.org/bionitio-team/bionitio-haskell.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-haskell) | 
| Perl 5   | <https://github.com/bionitio-team/bionitio-perl5> | [![travis](https://travis-ci.org/bionitio-team/bionitio-perl5.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-perl5) | 
| Python   | <https://github.com/bionitio-team/bionitio-python> | [![travis](https://travis-ci.org/bionitio-team/bionitio-python.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-python) | 
| R        | <https://github.com/bionitio-team/bionitio-r> | [![travis](https://travis-ci.org/bionitio-team/bionitio-r.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-r) | 
| Ruby     | <https://github.com/bionitio-team/bionitio-ruby> | [![travis](https://travis-ci.org/bionitio-team/bionitio-ruby.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-ruby) | 
| Rust     | <https://github.com/bionitio-team/bionitio-rust> | [![travis](https://travis-ci.org/bionitio-team/bionitio-rust.svg?branch=master)](https://travis-ci.org/bionitio-team/bionitio-rust) | 

## Basic functionality of bionitio

Bionitio is intended to be a simple prototypical bioinformatics tool that is easy to understand and modify. Therefore it has only minimal functionality; just enough to demonstrate all the key features of a real bioinformatics command line program without becoming distracted by unnecessary complexity.

If you use bionitio as the starting point for a new project we expect that you will rewrite it to implement your own desired functionality. However, much of the boilerplate is already provided for you; modifying the program should be significantly easier than starting from scratch.

All implementations of bionitio implement the same functionality and provide the same command line interface.
Specific details of bionitio's behaviour, usage, and installation, can be found in the README for each implementation.

Key features of the tool include:

* Command line argument parsing and usage information.
* Reading input from multiple files or optionally from standard input.
* The use of library code for parsing a common bioinformatics file format (FASTA).
* Progress and error logging.
* Defined exit status values.
* A test suite (unit testing and integration testing). 
* A version number.
* Standardised software building and packaging using programming language specific mechanisms.
* A standard open-source software license. 
* User documentation.
* Code documentation.

Where possible we follow the recommended conventions for programming style for each implementation language.

# License

The bionitio project is released as open source software under the terms of [MIT License](https://raw.githubusercontent.com/bionitio-team/bionitio/master/LICENSE).
However, we grant permission to users who derive their own projects from bionitio to apply their own license to their derived works. Licenses applied to projects deriving from bionitio do not affect in any way the license of the overall bionitio project, or licenses applied to other independent derivations.

# Starting a new project from bionitio

[How to set up a new bionitio project, step-by-step](https://github.com/bionitio-team/bionitio/wiki).

In the examples below `$` indicates the Unix prompt.

One of the main goals of bionitio is to provide a good place to start writing bioinformatics command line tools. To make that easy we've provided a shell script called `bionitio-boot.sh` to help you start a new project, which is run like so:

```
$ boot/bionitio-boot.sh -i python -n skynet -c BSD-3-Clause -g cyberdyne -a 'Miles Bennett Dyson' -e 'miles@cyberdyne.com' 
```

The example above starts a fresh project called `skynet` under the BSD-3-Clause license, using Python as the implementation language. A new git repository will be created in a sub-directory called `skynet` which will be initialised with a copy of bionitio and a blank revision history. All references to `bionitio` in the source code are replaced with `skynet`. Finally, the code is pushed to a new repository on [www.github.com](https://www.github.com) for the username `cyberdyne`.

You should replace `skynet` with a project name of your choice, and `cyberdyne` with your github username, if you have a github account. You may be asked to enter your github username. This assumes you do not already have a github project of the given name. If you don't have a github account, do not use the `-g` option. 

After you have started a new project from bionitio you are free to modify it as you see fit, modifying its functionality to suit your own requirements.


When setting up a new project using `bionitio-boot.sh` You must specify the following things: 

Required:

* -i LANGUAGE: the programming language you want to use (one of: c, clojure, cpp, haskell, java, js, perl5, python, r, ruby, rust)
* -n NAME: the name of your new project.

Optional:

* -c LICENSE: the license that you want to assign to your new project (one of: Apache-2.0, BSD-2-Clause, BSD-3-Clause, GPL-2.0, GPL-3.0, MIT). If you do not specify a license then it defaults to the MIT license.
* -g GITHUB-USERNAME: create a new remote repository in github and push new project to that repository. Replace `GITHUB-USERNAME` with your actual github user name. You may be prompted for your github password. This assumes you do not already have a repository in github with the same name as specified by the `-n NAME` option.
* -a AUTHOR-NAME: Use this name for the author of the code (will appear in source code headers and other places where a name is appropriate).
* -e AUTHOR-EMAIL: Use this string for the email address of author of the code (will appear in source code headers and other places where an email address is appropriate).
* -v: enable verbose mode; the script will print a lot more information about what it is doing. This is mostly useful for debugging if it does not work as expected. 
* -l LOGFILE: log progress information to the file named LOGFILE. This may be useful for debugging purposes. 

If you don't have a local copy of the script, you can run it from the web like so, using curl:

```
$ curl -sSf https://raw.githubusercontent.com/bionitio-team/bionitio/master/boot/bionitio-boot.sh \
 | bash -s -- -i python -n skynet -c BSD-3-Clause -g cyberdyne -a 'Miles Bennett Dyson' -e 'miles@cyberdyne.com'
```

If you prefer not to run a shell script from the web, then you can make a local copy of the `bionitio-boot.sh` script, and run it locally, as shown below:

```
# Copy the script to your local computer
$ curl https://raw.githubusercontent.com/bionitio-team/bionitio/master/boot/bionitio-boot.sh > bionitio-boot.sh

# Inspect the script to ensure you are happy with the commands it will execute on your system.

# Run the script on your local computer
$ bash bionitio-boot.sh -i python -n skynet -c BSD-3-Clause -g cyberdyne -a 'Miles Bennett Dyson' -e 'miles@cyberdyne.com'
```

# Authors

Alphabetically:

* Jessica Chung
* Harriet Dashnow
* Peter Georgeson
* Andrew Lonsdale
* Michael Milton
* Bernie Pope
* David R Powell
* Torsten Seemann
* Clare Sloggett
* Anna Syme
