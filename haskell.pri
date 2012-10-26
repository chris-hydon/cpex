# Compiler invocations.
GHC = ghc --make -i$${HS_SRC} -outputdir $${HS_DEST} -dynamic -O

# Haskell compiler tool.
haskell_c.commands = $${GHC} ${QMAKE_FILE_IN}
haskell_c.input = HS_SOURCES
haskell_c.output_function = haskellOutput
haskell_c.variable_out = OBJECTS
haskell_c.clean_commands = "find $${HS_DEST} -regextype posix-extended -regex '.*\\.(o|hi|h)' -exec $(DEL_FILE) {} \\;"

defineReplace(haskellOutput) {
  oname = $$replace(1, .hs, .o)
  # Can't just use replace here because HS_SRC is absolute, while oname is relative.
  output = $$system("echo $${oname} | sed s/.*$${HS_DEST}/$${HS_DEST}/")
  return($$output)
}

# Section to discover the location of haskell libs.
defineReplace(which_hs) {
  return($$system("$${HS_SRC}/which_hs $$1"))
}

for(dep, HS_DEPS) {
  lib = $$which_hs($$dep)
  HS_LIBS += $$lib
  HS_LIB_DIRS *= $$dirname(lib)
}

QMAKE_EXTRA_COMPILERS += haskell_c
