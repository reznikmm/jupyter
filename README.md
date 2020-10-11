Jupyter Client API in Ada
=========================

This is repositoru contains
[Jupyter Client API](https://jupyter-client.readthedocs.io/en/stable/index.html)
binding and Ada Kernel written in Ada.

## Install
### Using [alire](https://alire.ada.dev)

    alr get --build jupyter_kernel
    cd jupyter_kernel*
    ln -s ./alire/build/.objs .
    PATH=$PATH:$PWD/alire/build/.objs/driver JUPYTER_PATH=$PWD jupyter-notebook --debug

### Build from sources
Unpack source and run `make`.

### Dependencies
It depends on
 * [Matreshka](https://forge.ada-ru.org/matreshka) library.
 * [ZeroMQ-Ada](https://github.com/persan/zeromq-Ada)

For now the Ada Kernel runs on Linux and Mac OS X (not tested) only, because it uses `dlopen` to load shared
libraries.

## Usage
Run `make` to build the library and examples. Then run jupyter-notebook:

```
JUPYTER_PATH=$PWD jupyter-notebook --debug
```

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
