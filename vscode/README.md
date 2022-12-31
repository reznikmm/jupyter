# Jupyter Ada Kernel extension for VS Code

The extension simplifies installation of a Jupyter Kernel for Ada.

## Features

The `Install Jupyter Ada Kernel` command lets the user install Ada kernel and
configures it for usage in VS Code, Jupyter notebook and others IDE.

##  Dependencies

The kernel requires
* GNAT Ada compiler
* GNAT Project Manager [gprbuild](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html)
* [ALIRE: Ada LIbrary REpository](https://alire.ada.dev/) (*Optional*)

## Get started

1. Install this extension in the VS Code
2. Open the Command Palette (`Ctrl+Shift+P`) and type in
   `Install Jupyter Ada Kernel` command
3. Install the [Jupyter Extension](https://marketplace.visualstudio.com/items?itemName=ms-toolsai.jupyter)
4. Open or create a notebook file by opening the Command Palette
   (`Ctrl+Shift+P`) and select `Jupyter: Create New Jupyter Notebook`
   ![Create New Jupyter Notebook](https://camo.githubusercontent.com/1a1b93284c1064d34669bffe18933627eaf4a713582904d7728b214d123b61c3/68747470733a2f2f636f64652e76697375616c73747564696f2e636f6d2f6173736574732f646f63732f64617461736369656e63652f646174612d736369656e63652d7475746f7269616c2f6372656174652d6e6f7465626f6f6b2e706e67)
5. Select `Ada` kernel by clicking on the kernel picker in the top right
   of the notebook or by invoking the `Notebook: Select Notebook Kernel`
   command
   ![Kernel Picker](https://camo.githubusercontent.com/5621cf127311a2aae0dc40d0c796895f7f23c88c3c1d46f2de94fe9bad1b2956/68747470733a2f2f636f64652e76697375616c73747564696f2e636f6d2f6173736574732f646f63732f64617461736369656e63652f646174612d736369656e63652d7475746f7269616c2f73656c6563742d6b65726e656c2e706e67)
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
