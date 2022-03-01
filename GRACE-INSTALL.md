Introduction
============

UMD's DIT has successfully installed a `racket` environment on Grace, but has yet to setup `nasm`.
This is a manual workaround for CMSC430 students.

Overview
--------

We will be doing the following to your GRACE environment:

* Changing your default path (`$PATH`) to
  - include the Racket installation
  - have a location for local binaries
* Retrieving the source code for the Netwide Assembler
* Building that source code
* Installing the resulting `nasm` binary to a location on your path
* Installing necessary Racket packages

Adding Racket to your $PATH
===========================


Ensuring $HOME/.local exists
----------------------------

First we want to create the following directory _if it does not already exist_: `$HOME:/.local`.

When you log into GRACE your default working directory will be your home directory.
If you changed any of this behavior for a previous class, you can ensure that you're in home directory by running `cd $HOME`.
Once in your home directory run: `ls .local`, this will tell you if the directory already exists. If you get something like the following, it means that you'll need to create that directory:

```
grace7:~: ls .local
ls: cannot access .local: No such file or directory
```

If it does not exist, you need to create it with `mkdir .local` or `mkdir $HOME/.local`.

Adding Racket and .local to your PATH
-------------------------------------

You will want to add two directories to your PATH:

* The GRACE Racket environment: `/cell_root/software/racket/8.4/sys/bin/`
* The `.local` directory that you created: `$HOME/.local/bin` (you didn't need to create the `bin` part)

You can do this by manually editing your shell configuration. If you use the default GRACE shell, you can do it easily by running the following command from within`$HOME`:

```
echo 'set path = ( $path /cell_root/software/racket/8.4/sys/bin/ $HOME/.local/bin)' >> .cshrc.mine
```

It is important that you use single-quotes, exactly as shown above.

You can test that this works by doing the following:

```
source .cshrc
racket
```

And that should load the `racket` repl.

Getting NASM Set up
===================

Getting the NASM Source
-----------------------

From your home directory, run the following commands:

```
mkdir nasm-build
wget https://www.nasm.us/pub/nasm/releasebuilds/2.15.05/nasm-2.15.05.tar.xz -P nasm-build
```

Building NASM
-------------

Change into the `nasm-build` directory and run the following. This will extract the source from the tarball, configure the build for our PATH on GRACE, and then build the binary according to that configuration:

```
cd nasm-build/
tar xf nasm-2.15.05.tar.xz
cd nasm-2.15.05
./configure --prefix=$HOME/.local
make install
```

Once this is completed (a few minutes), you can test it by changing back to your home directory and running

```
source .cshrc
nasm --version
```

And you should see something like: `NASM version 2.15.05 compiled on Feb 18 2022`

Racket Packages
===============

Once all this is completed, you should be able to install the CMSC430 packages.
First, we need one dependency, `rackcheck`, which can be installed as:

```
raco pkg install rackcheck
```

Then you can run the following command to install the CMSC430 languages/tools:

```
raco pkg install 'https://github.com/cmsc430/www.git?path=langs#main'
```

In order to test that install the racket packages has worked, you can try to replicate the following CLI session:

```
grace7:~: racket
Welcome to Racket v8.4 [cs].
> (require a86)
> (Mov 'rax 42)
(Mov 'rax 42)
> (Mov 42 'rax)
Mov: expects register or offset; given 42 [,bt for context]
> 
```
