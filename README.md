# GODE - A Minimal Haskell Debugger with Causal Tracing

GODE is a lightweight debugger that helps you understand the causal relationships in your program's execution. It provides detailed traces of decision paths, making it easy to understand why a particular branch was taken and what led to that decision.

## Project Status

**Work in Progress**

### Currently Implemented

- Basic causal tracing for conditional expressions
- Support for comparison operators (`>`, `<`, `==`, `>=`, `<=`)
- Interactive debugging mode
- Colorized error messages
- Basic test suite
- Command-line interface

### Coming Soon

- Multiple variable support
  - Variable declaration and assignment
  - Variable scope tracking
  - Variable value history
- Enhanced tracing
  - JSON output format
  - File-based trace logging
  - Custom trace formatting
  - Trace filtering and search
- Advanced debugging features
  - Breakpoints
  - Step-by-step execution
  - Watch expressions
  - Call stack visualization
- Integration capabilities
  - IDE integration
  - Log aggregation
  - Performance metrics
- Documentation
  - API documentation
  - Debugging guides
  - Example scenarios
  - Best practices

## Features

- Causal tracing of program execution paths
- Detailed logging of decision points and their outcomes
- Support for multiple comparison operators: `>`, `<`, `==`, `>=`, `<=`
- Interactive debugging mode (REPL)
- Colorized error messages for better visibility
- Clear visualization of the decision chain

## Installation

1. Clone the repository:

```bash
git clone https://github.com/yourusername/gode.git
cd gode
```

2. Build the project:

```bash
cabal build
```

## Usage

### Command Line Mode

Debug expressions directly from the command line:

```bash
cabal run gode -- <value> <operator> <threshold> <then-value> <else-value>
```

Examples:

```bash
# Debug a greater-than condition
cabal run gode -- 5 '>' 3 high low

# Debug a less-than condition
cabal run gode -- 2 '<' 3 low high

# Debug an equality check
cabal run gode -- 3 '==' 3 match no-match

# Debug a greater-than-or-equal condition
cabal run gode -- 3 '>=' 3 high low

# Debug a less-than-or-equal condition
cabal run gode -- 2 '<=' 3 low high
```

### Interactive Debugging Mode

Start the interactive debugging session:

```bash
cabal run gode -- repl
```

In the debugger, enter expressions to trace:

```
<number> <operator> <number> <then-value> <else-value>
```

Examples:

```
gode> 2 < 6 high low
gode> 3 >= 3 match no-match
gode> 5 == 3 yes no
```

Type `quit` to exit the debugger.

## Trace Output

The debugger provides detailed traces showing the causal chain of decisions:

```
[GODE] Decision: x >= 3 is True
[GODE]   ↳ val = 3 (from cli: 3)
[GODE]   ↳ Because 3 >= 3
Result: high
```

Each trace shows:

- The condition being evaluated
- The input value and its source
- The reason for the decision
- The final outcome

## Supported Operators

- `>` : Greater than
- `<` : Less than
- `==` : Equal to
- `>=` : Greater than or equal to
- `<=` : Less than or equal to

## Error Handling

The debugger provides clear, colorized error messages for:

- Invalid operators
- Invalid numbers
- Invalid format
- Missing arguments

## Development

### Project Structure

```
gode/
├── app/
│   └── Main.hs         # Debugger interface
├── src/
│   └── Gode/
│       └── Core.hs     # Core debugging logic
├── test/
│   └── Spec.hs         # Test suite
└── gode.cabal          # Cabal package configuration
```

### Running Tests

```bash
cabal test
```

## Future Enhancements

- Support for multiple variables
- JSON output format for machine processing
- File-based trace logging
- Custom trace formatting
- Integration with other debugging tools

## License

MIT License - see LICENSE file for details

## Author

Bahaa Tantaoui (ryver)
