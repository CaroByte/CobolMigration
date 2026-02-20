---
mode: ask
description: Generate a comprehensive COBOL unit test suite for a given program or paragraph using the COBOL-check framework.
---

# Generate COBOL Unit Tests

You are a COBOL testing expert. Generate a complete COBOL-check unit test suite for the COBOL program or paragraph provided.

## Instructions

Use the [COBOL-check](https://github.com/openmainframeproject/cobol-check) framework (Open Mainframe Project). COBOL-check intercepts COBOL programs at the paragraph level and injects test assertions without modifying the source program.

### Test file conventions
- Test file name: `${programName}.cut` (COBOL Unit Test)
- One `TestSuite` per COBOL section or logical feature area
- One `TestCase` per paragraph or business rule being tested
- Use `MOVE` to set up `WORKING-STORAGE` fields before calling a paragraph
- Use `EXPECT` to assert outcomes

### COBOL-check syntax reference

```
TestSuite "Suite name"

    TestCase "test case description"
        MOVE <value> TO <WS-FIELD>
        PERFORM <PARAGRAPH-NAME>
        EXPECT <WS-RESULT> TO BE <expected-value>
        EXPECT <WS-FLAG> TO BE "Y"
        EXPECT <WS-COUNTER> TO EQUAL 3

    TestCase "another test case"
        MOVE ZEROS TO WS-AMOUNT
        PERFORM CALC-TOTAL
        EXPECT WS-AMOUNT TO EQUAL ZERO
```

### Assertion types available
| Assertion | Meaning |
|-----------|---------|
| `TO BE <literal>` | Exact equality |
| `TO EQUAL <field>` | Equality with another field |
| `TO BE NUMERIC` | Field contains only digits |
| `TO BE ALPHABETIC` | Field contains only letters |
| `TO BE ALPHANUMERIC` | Any characters |
| `NOT TO BE <literal>` | Inequality |
| `TO BE GREATER THAN <value>` | Numeric comparison |
| `TO BE LESS THAN <value>` | Numeric comparison |

---

## What to generate

Given the COBOL program below, produce:

1. **A `.cut` test file** covering:
   - Happy-path scenarios for every paragraph in the PROCEDURE DIVISION
   - Boundary conditions (zero values, maximum field sizes, empty strings)
   - Error/exception paths (invalid input, FILE STATUS codes 10, 22, 35, etc.)
   - Each 88-level condition name exercised by at least one test

2. **A test data setup section** at the top of the `.cut` file using `MOVE` statements to initialise `WORKING-STORAGE` fields to known states before each test.

3. **Comments** above each `TestCase` explaining what business rule it validates and which COBOL line(s) it covers.

---

## Example — interest calculation

### Source COBOL
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. CALC-INT.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-PRINCIPAL       PIC 9(9)V9(2) COMP-3.
01 WS-RATE            PIC 9(3)V9(4) COMP-3.
01 WS-INTEREST        PIC 9(9)V9(2) COMP-3.
01 WS-RETURN-CODE     PIC 9(2).
   88 CALC-OK         VALUE 0.
   88 RATE-ZERO-ERR   VALUE 10.

PROCEDURE DIVISION.
MAIN-PARA.
    IF WS-RATE = ZEROS
        MOVE 10 TO WS-RETURN-CODE
        STOP RUN
    END-IF
    COMPUTE WS-INTEREST ROUNDED =
        WS-PRINCIPAL * WS-RATE / 100
    MOVE 0 TO WS-RETURN-CODE
    STOP RUN.
```

### Generated `.cut` test file
```
TestSuite "CALC-INT Interest Calculation Tests"

    *> Happy path: standard interest calculation
    TestCase "calculates interest correctly for positive principal and rate"
        MOVE 1000.00 TO WS-PRINCIPAL
        MOVE 005.0000 TO WS-RATE
        PERFORM MAIN-PARA
        EXPECT WS-INTEREST TO EQUAL 50.00
        EXPECT WS-RETURN-CODE TO BE 0

    *> Boundary: zero rate should trigger error code
    TestCase "returns error code 10 when rate is zero"
        MOVE 1000.00 TO WS-PRINCIPAL
        MOVE 000.0000 TO WS-RATE
        PERFORM MAIN-PARA
        EXPECT WS-RETURN-CODE TO BE 10

    *> Boundary: zero principal
    TestCase "returns zero interest when principal is zero"
        MOVE 0 TO WS-PRINCIPAL
        MOVE 005.0000 TO WS-RATE
        PERFORM MAIN-PARA
        EXPECT WS-INTEREST TO EQUAL ZERO
        EXPECT WS-RETURN-CODE TO BE 0

    *> Boundary: maximum field size
    TestCase "handles maximum principal value without overflow"
        MOVE 999999999.99 TO WS-PRINCIPAL
        MOVE 001.0000 TO WS-RATE
        PERFORM MAIN-PARA
        EXPECT WS-RETURN-CODE TO BE 0

    *> 88-level condition: verify CALC-OK is set
    TestCase "CALC-OK condition is true after successful calculation"
        MOVE 500.00 TO WS-PRINCIPAL
        MOVE 010.0000 TO WS-RATE
        PERFORM MAIN-PARA
        EXPECT WS-RETURN-CODE TO BE 0

    *> 88-level condition: verify RATE-ZERO-ERR is set
    TestCase "RATE-ZERO-ERR condition is true when rate is zero"
        MOVE 500.00 TO WS-PRINCIPAL
        MOVE ZEROS TO WS-RATE
        PERFORM MAIN-PARA
        EXPECT WS-RETURN-CODE TO BE 10
```

---

## Now generate tests for this program

Paste the COBOL source below and I will produce a complete `.cut` test suite following the pattern above.

```cobol
{{selection}}
```

> **Tip**: Select the full COBOL source file (or the paragraphs you want tested) before running this prompt. Copilot will use `{{selection}}` to insert the selected text automatically.
