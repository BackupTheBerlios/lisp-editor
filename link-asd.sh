
##For sbcl.

## Makes symbolic links to the systems list. 
## Change ~/.sbcl/systems/ if yours differs.

ln -s $PWD/libs/generic.asd ~/.sbcl/systems/
ln -s $PWD/libs/denest.asd ~/.sbcl/systems/

ln -s $PWD/gil/gil.asd ~/.sbcl/systems/

ln -s $PWD/tools/expression-hook.asd ~/.sbcl/systems/

ln -s $PWD/tools/expression-scan.asd ~/.sbcl/systems/

ln -s $PWD/tools/autodoc.asd ~/.sbcl/systems/

ln -s $PWD/tools/gil-log.asd ~/.sbcl/systems/
