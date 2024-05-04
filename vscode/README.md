# Jupyter Ada Kernel extension for VS Code

The extension simplifies installation of a Jupyter Kernel for Ada.

## Features

The `Install Jupyter Ada Kernel` command lets the user install Ada kernel and
configures it for usage in VS Code, Jupyter notebook and others IDE.

## Dependencies

The kernel requires

* GNAT Ada compiler
* GNAT Project Manager [gprbuild](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html)
* [Alire: Ada LIbrary REpository](https://alire.ada.dev/) (*Optional*)

## Get started

1. Install this extension in the VS Code
2. Open the Command Palette (`Ctrl+Shift+P`) and type in
   `Install Jupyter Ada Kernel` command
3. Install the [Jupyter Extension](https://marketplace.visualstudio.com/items?itemName=ms-toolsai.jupyter)
4. Open or create a notebook file by opening the Command Palette
   (`Ctrl+Shift+P`) and select `Create: New Jupyter Notebook`
5. Select `Ada` kernel by clicking on the kernel picker in the top right
   of the notebook or by invoking the `Notebook: Select Notebook Kernel`
   command. Choose `Jupyter Kernel...` and then `Ada` kernel.
6. Enter next two cells and execute them
    1. `with Ada.Text_IO;`
    2. `Ada.Text_IO.Put_Line ("Hello!");`

## Jupyter Ada Kernel usage

The Jupyter Ada Kernel let you insert following cells:

### Context clauses cells

Such a cell could contains *with*, *use*-clauses and `pragma`-s. Example:

```ada
with Ada.Text_IO;
```

### Declarations cells

Such a cell could contains *declarations* (those don't require completion). Example:

```ada
Text : constant String = "Hello, World!";
```

### Statements cells

Such a cell could contains *statements* (Including compound statements). Example:

```ada
Ada.Text_IO.Put (Text);
```

### Bodies cells

If the declaration requires a completion, then it should be in the same cell:

```ada
procedure Test;

procedure Test is
begin
   Ada.Text_IO.Put ("Test!");
end Test;
```

### *Magic* cells

If you have *Alire* installed, then you can use `%alr` magic cells to install
dependencies (but they should work as shared libraries).

    %lsmagic

    %als?

    %als with spawn

**Enjoy!**
