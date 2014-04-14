#!/bin/bash

TEST_PLACE="testplatz"
SOURCES_DIR="sources"
INT_SRC="../Lex.hs"

HSINT_NAME="../int_c"

mkdir $TEST_PLACE
echo Vytvaram zlozku $TEST_DIR

ghc $INT_SRC -o $HSINT_NAME

# modifikacia zdrojaku pre preklad pomocou g++

for SFILE in `cd $SOURCES_DIR; ls *.c`; do
	echo -n Spracovavam $SFILE :::" "

	touch $TEST_PLACE/c_in
	if [ -f $SOURCES_DIR/$SFILE.in ]; then
		cp $SOURCES_DIR/$SFILE.in $TEST_PLACE/c_in
	fi
	touch $TEST_PLACE/x_ref
	touch $TEST_PLACE/c_in
	if [ -f $SOURCES_DIR/$SFILE.ref ]; then
		cp $SOURCES_DIR/$SFILE.ref $TEST_PLACE/x_ref
	fi
	
	$HSINT_NAME $SOURCES_DIR/$SFILE < $TEST_PLACE/c_in > $TEST_PLACE/$SFILE.out 2>&1

	# diffnem referencny s nasim
	diff -q $TEST_PLACE/$SFILE.out $TEST_PLACE/x_ref
	if [ $? -eq 0 ]; then
		echo -e "\e[38;5;10mOK\e[0m"
	else
		echo -e "\e[38;5;196mFiles .out and .ref differ\e[0m"
		echo "Press enter to continue ..."
		read
	fi
	rm -rf $TEST_PLACE/*
done
rm -rf $TEST_PLACE