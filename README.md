Jupyter Client API in Ada
=========================

This is
[Jupyter Client API](https://jupyter-client.readthedocs.io/en/stable/index.html)
binding in Ada.

## Install
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
