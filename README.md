Functional Programming Interpreter - ReadMe
Project Overview
This project involves the development of a functional programming interpreter designed for a simple language that emphasizes function definitions and lambda expressions. The language is characterized by immutability, recursive functions, and the absence of variable assignments or mutable state.

Language Specifications
1. Data Types
INTEGER: Whole numbers (e.g., -3, 0, 42)
BOOLEAN: Logical values True and False
2. Operations
Arithmetic Operations (for INTEGERs):
Addition (+)
Subtraction (-)
Multiplication (*)
Division (/) (integer division)
Modulo (%)
Boolean Operations:
AND (&&)
OR (||)
NOT (!)
Comparison Operations:
Equal to (==)
Not equal to (!=)
Greater than (>)
Less than (<)
Greater than or equal to (>=)
Less than or equal to (<=)
3. Functions
Named Function Definitions: Define functions with specific names.
Anonymous Functions (Lambda Expressions): Define unnamed functions using lambda expressions.
Function Application: Invoke functions with arguments.
4. Recursion
Support for recursive function calls.
Replacement for traditional loops using recursion.
5. Immutability
All values are immutable.
No variable assignments or state changes.
Project Components
1. Lexer
Tokenizes the input source code.
Handles whitespace, comments, and all language constructs.
2. Parser
Constructs an Abstract Syntax Tree (AST) from tokenized input.
Parses according to the BNF (Backus–Naur Form) grammar of the language.
3. Interpreter
Executes commands in both interactive mode (REPL) and batch mode (executing a full program).
Evaluates the AST, supports function application, higher-order functions, and manages function calls with a call stack.
4. Error Handling
Comprehensive error checking and reporting.
Meaningful error messages for syntax, type, and runtime errors.
5. REPL (Read-Eval-Print Loop)
Interactive mode for executing commands line by line and printing the results.
6. Documentation
Provides BNF grammar for the language.
Describes language syntax, features, and design considerations.
Comments throughout the code to explain key components and algorithms.
7. Testing
Comprehensive test suite covering all language features, including edge cases and error conditions.
