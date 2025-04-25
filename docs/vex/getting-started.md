# Getting started with Vex

Welcome to Vex! This guide will help you install Vex, write your first program, and understand the basics to get up and running quickly.

---

## Installation

To install Vex, follow these steps:

### Using Prebuilt Binaries (Recommended)
1. Visit the [official Vex website](#) and download the latest release for your operating system.
2. Extract the archive and add the `Vex` binary to your system's `PATH`.

### Building from Source
If you prefer building from source:

#### Linux/Windows (x86_64)
We recommend using `ghcup`, the official Haskell toolchain installer.

Visit the official [ghcup website](https://www.haskell.org/ghcup/) and follow the installation guide.

once `ghcup` is installed:
```bash
git clone https://github.com/PeterGriffinSr/Vex.git
cd vex
cabal build
```

#### Linux (i386)
If you're on a 32-bit system, you can install the toolchain manually:

##### Step 1: Install GHC
Download and install GHC manually:
```bash
wget https://downloads.haskell.org/~ghc/9.6.7/ghc-9.6.7-i386-deb9-linux.tar.xz
tar -xf ghc-9.6.7-i386-deb9-linux.tar.xz
cd ghc-9.6.7
./configure
sudo make install
```

##### Step 2: Install Cabal
```bash
wget https://downloads.haskell.org/~cabal/Cabal-3.12.1.0/
tar -xf Cabal-3.12.1.0.tar.gz
sudo mv cabal /usr/local/bin
```

Ensure ghc and cabal are available in your PATH by running:
```
ghc --version
cabal --version
```

Then build Vex:
```bash
git clone https://github.com/PeterGriffinSr/Vex.git
cd Vex
cabal build
```
---

# Your First Vex Program
Create a file called `main.vex` with the following content:
```
val () -> int: main fn () =>
    print<string> "Hello, world";
    0;
```
To compile and run:
```
vex main.vex -o hello
./hello
```

---

# Running the REPL
You can experiment with Vex interactively using the REPL:
```
vex repl
```

Try this:
```
> val (int) -> int: sqaure fn (x) => x * x;
> sqaure(4);
> 16
```

---

# What's Next?
Now that you’re set up, explore the rest of the documentation:
- [Syntax Overview](/docs/vex/syntax.md)
- [Type System](/docs/vex/type-system.md)