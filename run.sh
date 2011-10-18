export SSAXLIB=`pwd`/support/ssax-sxml
#gsi convert.scm --xml samples_docs/docbook/definitive_guide/en/book5.xml --tex x.tex
gsi -:d- convert.scm --xml samples_docs/docbook/definitive_guide/en/src/ch02.xml --tex tmp/test.tex
