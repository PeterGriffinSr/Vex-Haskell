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

```
git clone https://github.com/PeterGriffinSr/Vex.git

cd vex

cabal build
```
Note: Ensure you have `cabal`, a GHC compiler (like `ghc`), installed.

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