Jupyter Kernel and Client API in Ada
====================================

[![Copr build status](https://copr.fedorainfracloud.org/coprs/reznik/ada/package/jupyter-ada/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/reznik/ada/)

This is repositoru contains
[Jupyter Client API](https://jupyter-client.readthedocs.io/en/stable/index.html)
binding and Ada Kernel written in Ada.

## Install

### From GitHub release

Download [the latest release](https://github.com/reznikmm/jupyter/releases) and unzip it
in some directory. Move `kernel.json` into `$HOME/.local/share/jupyter/kernels/ada/kernel.json`,
then edit it by replacing `.bin` with the directory where `ada_kernel` is.
You can also export `JUPYTER_PATH` to help IDE find `kernel.json`:

    export JUPYTER_PATH=$HOME/.local/share/jupyter

### Using [alire](https://alire.ada.dev)

    alr get --build jupyter_kernel
    cd jupyter_kernel*
    JUPYTER_PATH=$PWD jupyter-notebook --debug

### Build from sources
Unpack source and run `make`.

### Dependencies
It depends on
 * [GNAT](https://www.adacore.com/download/more) Ada compiler and `gprbuild` tool
 * [Matreshka](https://forge.ada-ru.org/matreshka) library
 * [ZeroMQ-Ada](https://github.com/persan/zeromq-Ada) binding to zeromq
 * [Spawn](https://github.com/AdaCore/spawn) - a process launch library
 * [Alire](https://alire.ada.dev/) package manager (optional)

For now the Ada Kernel runs on Linux and Mac OS X (not tested) only, because it uses `dlopen` to load shared
libraries.

## Usage
Run `make` to build the library and examples. Then run jupyter-notebook:

```
JUPYTER_PATH=$PWD jupyter-notebook --debug
```

Now you can open a notebook file, for example `tests/Hello_Ada.ipynb`.
If you are going to use `alr` make sure to build projects as shared libraries.

## Related works
* [Jupyter Ada kernel](https://github.com/gusthoff/jupyter-ada-kernel)

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/reznikmm/jupyter/issues/new)
or submit PRs.

## License

[MIT](LICENSES/MIT.txt) © Maxim Reznik

