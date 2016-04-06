ANTLR=./tools/antlr4
ANTLR_OPTIONS=-Dlanguage=Python3 -visitor
BUILD_DIR=build
SRC_DIR=src

all: generate_files

generate_files: \
		${SRC_DIR}/Grammar.g4 \
		${SRC_DIR}/PreLexer.g4
	cp -R ${SRC_DIR}/* ${BUILD_DIR}/
	${ANTLR} ${BUILD_DIR}/PreLexer.g4 -o ${BUILD_DIR} ${ANTLR_OPTIONS}
	# Looks like a bug in antlr
	mv ${BUILD_DIR}/${BUILD_DIR}/* ${BUILD_DIR}

	env PYTHONPATH=./${BUILD_DIR} \
		python -c "from Lexer import *; print(AHToken.exportToAntlr())" \
		> ${BUILD_DIR}/Lexer.tokens

	${ANTLR} ${BUILD_DIR}/Grammar.g4 -o ${BUILD_DIR} ${ANTLR_OPTIONS}

	mv ${BUILD_DIR}/${BUILD_DIR}/* ${BUILD_DIR}

	rmdir ${BUILD_DIR}/${BUILD_DIR}

clean:
	rm -rf ${BUILD_DIR}/*