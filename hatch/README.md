Hatch is not enabled by default for this project, since it is not mandatory that you use it, and it will be faster to compile before adding the dependency on gloss.

To start using Hatch with Hurtle, you should:

1. Copy the "Hatch" folder from here into the src/ directory
2. Add "src/hatch" to the list of "source directories" for hurtle, which is
   around line 35 of package.yaml. (Currently commented out)
3. Add "gloss" and "gloss-juicy" to the dependencies list, around 
   line 13-4 of package.yaml. (Currently commented out)