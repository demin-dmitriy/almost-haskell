ANTLR=./tools/antlr4
ANTLR_OPTIONS=-Dlanguage=Python3 -visitor
BUILD_DIR=build
SRC_DIR=src

all: generate_files

generate_files: copy_files
	${ANTLR} ${BUILD_DIR}/PreLexer.g4 -o ${BUILD_DIR} ${ANTLR_OPTIONS}
	# Looks like a bug in antlr
	mv ${BUILD_DIR}/${BUILD_DIR}/* ${BUILD_DIR}

	env PYTHONPATH=./${BUILD_DIR} \
		python -c "from Lexer import *; print(AHToken.exportToAntlr())" \
		> ${BUILD_DIR}/Lexer.tokens

	${ANTLR} ${BUILD_DIR}/AHParser.g4 -o ${BUILD_DIR} ${ANTLR_OPTIONS}

	mv ${BUILD_DIR}/${BUILD_DIR}/* ${BUILD_DIR}

	rmdir ${BUILD_DIR}/${BUILD_DIR}

copy_files:
	cp --preserve=timestamps -R ${SRC_DIR}/* ${BUILD_DIR}/

java_files: generate_files
	cd ${BUILD_DIR}; ../tools/antlr4 PreLexer.g4 AHParser.g4
	javac -classpath "./tools/antlr4.jar" ${BUILD_DIR}/*.java

test: copy_files
	python ${BUILD_DIR}/Test.py

clean:
	rm -rf ${BUILD_DIR}/*