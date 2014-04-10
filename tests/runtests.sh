#!/bin/bash

TEST_DIR="testplatz"
SOURCES_DIR="sources"

GCC_NAME="gcc"
HSINT_NAME="../Lex"

mkdir $TEST_DIR
echo Vytvaram zlozku $TEST_DIR

# modifikacia zdrojaku pre preklad pomocou g++

for SFILE in `cd $SOURCES_DIR; ls *.c`; do
	echo -n Spracovavam $SFILE :::" "
	# skopcit subor so zdrojakom do testovacieho adresara
	cp $SOURCES_DIR/$SFILE $TEST_DIR/2run_hs.c
	# skopcit vstupy a vystupy do testovacieho adresara
	touch $TEST_DIR/c_in
	if [ -f $SOURCES_DIR/$SFILE.in ]; then
		cat $SOURCES_DIR/$SFILE.in > $TEST_DIR/c_in
	fi

	echo "#include <iostream>" > $TEST_DIR/2run_cpp.c
	echo "#include <string>" >> $TEST_DIR/2run_cpp.c
	echo "#include <stdio.h>" >> $TEST_DIR/2run_cpp.c
	echo "using namespace std;" >> $TEST_DIR/2run_cpp.c
	echo "" >> $TEST_DIR/2run_cpp.c

	cat $TEST_DIR/2run_hs.c | sed 's/print(\([^)]*\))/std::cout << \1 << std::endl/g' | sed 's/scan(s_\([^)]*\))/getline(std::cin,s_\1)/g' | sed 's/scan(i_\([^)]*\))/scanf("%d",\&i_\1)/g' >> $TEST_DIR/2run_cpp.c
	#cat $TEST_DIR/2run_cpp.c

	# preklad zdrojaku pomocou g++
	g++ $TEST_DIR/2run_cpp.c -o $TEST_DIR/2run_c
	./$TEST_DIR/2run_c < $TEST_DIR/c_in > $TEST_DIR/c_out
	RET_C=$?

	# interpretacia cez nas interpret
	#echo ---
	#cat $TEST_DIR/2run_hs.c
	#echo ---
	./$HSINT_NAME $TEST_DIR/2run_hs.c < $TEST_DIR/c_in > $TEST_DIR/hs_out
	RET_HS=$?

	# diffnem referencny s nasim
	if [ $RET_C -ne $RET_HS ]; then
		echo "Rozdielne navratove hodnoty -> G++: $RET_C, HS: $RET_HS !!!"
	else
		diff -q $TEST_DIR/c_out $TEST_DIR/hs_out
		if [ $? -eq 0 ]; then
			echo "OK"
		fi
	fi
	rm -rf $TEST_DIR/*
done
rm -rf $TEST_DIR