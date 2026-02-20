---
mode: ask
description: Migrate a COBOL program or paragraph to modern Java 17/21, producing a Spring Boot service class, a domain model, and a JUnit 5 test skeleton.
---

# Migrate COBOL to Java

You are a senior COBOL-to-Java migration engineer. Convert the COBOL source provided into idiomatic Java 17/21 following the conventions in this repository.

## Migration Steps (follow in order)

### Step 1 — Analyse the COBOL source

Produce the following table before writing any Java code:

| COBOL Element | Detail | Java Mapping |
|---------------|--------|--------------|
| `PROGRAM-ID` | `<name>` | Class name `<Name>Service` |
| Inputs (LINKAGE / files) | `<list>` | Method parameters / `@RequestBody` |
| Outputs | `<list>` | Return type |
| Paragraphs | `<list>` | Private methods |
| WORKING-STORAGE fields | `<list>` | Fields / record components |
| Business rules | `<list>` | Comments in code |
| Migration risks | `<list>` | `// TODO` markers |

### Step 2 — Create the domain model

Map each significant `01`-level group to a Java `record` or `@Entity`:

```java
// 01 WS-CUSTOMER GROUP → Java record
public record Customer(
    String id,          // PIC X(10)
    String name,        // PIC X(30)
    BigDecimal balance  // PIC 9(9)V9(2) COMP-3
) {}
```

### Step 3 — Create the service class

- Package: `com.example.migration.<domain>`
- Class: `<ProgramName>Service`
- Annotate with `@Service` and `@Transactional` (if DB access is involved)
- One `public` method per COBOL `ENTRY POINT` or primary paragraph
- One `private` method per COBOL paragraph

```java
@Service
@Transactional
public class CalcIntService {

    private static final Logger log = LoggerFactory.getLogger(CalcIntService.class);

    // COBOL: MAIN-PARA
    public BigDecimal calculateInterest(String accountId) { … }

    // COBOL: VALIDATE-RATE
    private void validateRate(BigDecimal rate) { … }
}
```

### Step 4 — Map file I/O to Spring Data

| COBOL | Java |
|-------|------|
| `OPEN INPUT file` | `repository.findAll()` / `repository.findById()` |
| `READ file INTO record AT END …` | `repository.findAll()` returns `List<T>` |
| `WRITE record` | `repository.save(entity)` |
| `REWRITE record` | `repository.save(entity)` (JPA merge) |
| `DELETE record` | `repository.delete(entity)` |
| Batch `READ-PROCESS-WRITE` loop | Spring Batch `ItemReader` → `ItemProcessor` → `ItemWriter` |

### Step 5 — Generate the test skeleton

Produce a `<ProgramName>ServiceTest.java` skeleton with:

- One `@Test` method per public method (happy path)
- One `@Test` method per error/boundary condition identified in Step 1
- `@Mock` for all injected dependencies
- `@BeforeEach` setup with representative test data

---

## Output Format

Produce the output in this order:

1. **Analysis table** (Step 1)
2. **Domain model** — `record` or `@Entity` classes
3. **Service class** — complete, compilable Java
4. **Test skeleton** — complete JUnit 5 class with TODO comments in test bodies

---

## Migration Rules

- **Never use `double` or `float`** for monetary values — always `BigDecimal`.
- **Use `RoundingMode.HALF_UP`** unless the COBOL uses `ROUNDED MODE TRUNCATION`.
- **Preserve business logic exactly** on the first pass; refactor only after tests pass.
- **`// COBOL: <PARA-NAME> line <n>`** comment on the first line of every migrated method.
- **`// TODO: verify business rule`** on any line where COBOL intent is ambiguous.
- **`// TODO: verify rounding`** on every `BigDecimal.divide()` call.
- Map `FILE STATUS 10` (end-of-file) to returning an empty `Optional` or throwing `EndOfFileException`.
- Map `FILE STATUS 22` (duplicate key) to throwing `DuplicateKeyException`.
- Map `FILE STATUS 35` (file not found) to throwing `FileNotFoundException`.
- Replace `GO TO` with structured loops; never emit `goto` in Java.
- Replace `PERFORM THRU` ranges with a sequence of private method calls.

---

## Complete Example

### Input COBOL
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PAYROLL.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-EMPLOYEE-ID    PIC X(8).
01 WS-HOURS-WORKED   PIC 9(3)V9(2) COMP-3.
01 WS-HOURLY-RATE    PIC 9(5)V9(2) COMP-3.
01 WS-GROSS-PAY      PIC 9(7)V9(2) COMP-3.
01 WS-OVERTIME-HRS   PIC 9(3)V9(2) COMP-3.
01 WS-RETURN-CODE    PIC 9(2).
   88 PAYROLL-OK     VALUE 0.
   88 INVALID-HOURS  VALUE 11.

PROCEDURE DIVISION.
MAIN-PARA.
    PERFORM VALIDATE-INPUT
    IF PAYROLL-OK
        PERFORM CALC-GROSS-PAY
    END-IF
    STOP RUN.

VALIDATE-INPUT.
    IF WS-HOURS-WORKED < 0 OR WS-HOURS-WORKED > 168
        MOVE 11 TO WS-RETURN-CODE
    ELSE
        MOVE 0 TO WS-RETURN-CODE
    END-IF.

CALC-GROSS-PAY.
    IF WS-HOURS-WORKED > 40
        SUBTRACT 40 FROM WS-HOURS-WORKED GIVING WS-OVERTIME-HRS
        COMPUTE WS-GROSS-PAY ROUNDED =
            (40 * WS-HOURLY-RATE)
            + (WS-OVERTIME-HRS * WS-HOURLY-RATE * 1.5)
    ELSE
        COMPUTE WS-GROSS-PAY ROUNDED =
            WS-HOURS-WORKED * WS-HOURLY-RATE
    END-IF.
```

### Output — Analysis Table
| COBOL Element | Detail | Java Mapping |
|---------------|--------|--------------|
| `PROGRAM-ID` | `PAYROLL` | `PayrollService` |
| Inputs | `WS-EMPLOYEE-ID`, `WS-HOURS-WORKED`, `WS-HOURLY-RATE` | `PayrollRequest record` |
| Outputs | `WS-GROSS-PAY`, `WS-RETURN-CODE` | `PayrollResult record` |
| Paragraphs | `MAIN-PARA`, `VALIDATE-INPUT`, `CALC-GROSS-PAY` | 3 methods |
| Business rules | Regular pay ≤ 40h; overtime × 1.5; hours range 0–168 | Inline comments |
| Migration risks | None identified | — |

### Output — Domain Model
```java
package com.example.migration.payroll;

import java.math.BigDecimal;

public record PayrollRequest(
    String employeeId,    // PIC X(8)
    BigDecimal hoursWorked, // PIC 9(3)V9(2) COMP-3
    BigDecimal hourlyRate   // PIC 9(5)V9(2) COMP-3
) {}

public record PayrollResult(
    BigDecimal grossPay,   // PIC 9(7)V9(2) COMP-3
    int returnCode         // PIC 9(2): 0=OK, 11=invalid hours
) {}
```

### Output — Service Class
```java
package com.example.migration.payroll;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Payroll gross-pay calculation.
 * Migrated from COBOL program PAYROLL.
 */
@Service
public class PayrollService {

    private static final Logger log = LoggerFactory.getLogger(PayrollService.class);
    private static final BigDecimal REGULAR_HOURS_MAX = BigDecimal.valueOf(40);
    private static final BigDecimal OVERTIME_MULTIPLIER = new BigDecimal("1.5");
    private static final BigDecimal MAX_HOURS = BigDecimal.valueOf(168);

    // COBOL: MAIN-PARA
    public PayrollResult calculateGrossPay(PayrollRequest request) {
        log.info("Calculating gross pay for employee {}", request.employeeId());
        int returnCode = validateInput(request.hoursWorked());
        if (returnCode != 0) {
            return new PayrollResult(BigDecimal.ZERO, returnCode);
        }
        BigDecimal grossPay = calcGrossPay(request.hoursWorked(), request.hourlyRate());
        return new PayrollResult(grossPay, 0);
    }

    // COBOL: VALIDATE-INPUT
    private int validateInput(BigDecimal hoursWorked) {
        if (hoursWorked.compareTo(BigDecimal.ZERO) < 0
                || hoursWorked.compareTo(MAX_HOURS) > 0) {
            return 11; // COBOL: 88 INVALID-HOURS VALUE 11
        }
        return 0;
    }

    // COBOL: CALC-GROSS-PAY
    private BigDecimal calcGrossPay(BigDecimal hoursWorked, BigDecimal hourlyRate) {
        if (hoursWorked.compareTo(REGULAR_HOURS_MAX) > 0) {
            BigDecimal overtimeHours = hoursWorked.subtract(REGULAR_HOURS_MAX);
            BigDecimal regularPay = REGULAR_HOURS_MAX.multiply(hourlyRate);
            // COBOL COMPUTE ROUNDED applies rounding to the final result only for this expression;
            // intermediate multiplication results are not rounded. This matches the Java behaviour below.
            // TODO: verify rounding — confirm COBOL ROUNDED MODE matches RoundingMode.HALF_UP here
            BigDecimal overtimePay = overtimeHours.multiply(hourlyRate).multiply(OVERTIME_MULTIPLIER);
            return regularPay.add(overtimePay).setScale(2, RoundingMode.HALF_UP);
        }
        // TODO: verify rounding — confirm COBOL ROUNDED MODE matches RoundingMode.HALF_UP here
        return hoursWorked.multiply(hourlyRate).setScale(2, RoundingMode.HALF_UP);
    }
}
```

### Output — Test Skeleton
```java
package com.example.migration.payroll;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.math.BigDecimal;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.*;

class PayrollServiceTest {

    private final PayrollService service = new PayrollService();

    /** COBOL: CALC-GROSS-PAY — regular hours (≤ 40) */
    @Test
    void calculateGrossPay_regularHours_returnsHoursTimesRate() {
        PayrollRequest request = new PayrollRequest("EMP-0001", new BigDecimal("40.00"), new BigDecimal("25.00"));
        PayrollResult result = service.calculateGrossPay(request);
        assertThat(result.returnCode()).isZero();
        assertThat(result.grossPay()).isEqualByComparingTo("1000.00");
    }

    /** COBOL: CALC-GROSS-PAY — overtime (> 40 hours) */
    @Test
    void calculateGrossPay_overtimeHours_appliesOneAndHalfMultiplier() {
        PayrollRequest request = new PayrollRequest("EMP-0002", new BigDecimal("50.00"), new BigDecimal("20.00"));
        // regular: 40 × 20 = 800; overtime: 10 × 20 × 1.5 = 300; total = 1100
        PayrollResult result = service.calculateGrossPay(request);
        assertThat(result.returnCode()).isZero();
        assertThat(result.grossPay()).isEqualByComparingTo("1100.00");
    }

    /** COBOL: VALIDATE-INPUT — hours > 168 → return code 11 */
    @Test
    void calculateGrossPay_tooManyHours_returnsInvalidHoursCode() {
        PayrollRequest request = new PayrollRequest("EMP-0003", new BigDecimal("200.00"), new BigDecimal("15.00"));
        PayrollResult result = service.calculateGrossPay(request);
        assertThat(result.returnCode()).isEqualTo(11);
        assertThat(result.grossPay()).isEqualByComparingTo(BigDecimal.ZERO);
    }

    /** COBOL: VALIDATE-INPUT — negative hours → return code 11 */
    @Test
    void calculateGrossPay_negativeHours_returnsInvalidHoursCode() {
        PayrollRequest request = new PayrollRequest("EMP-0004", new BigDecimal("-1.00"), new BigDecimal("15.00"));
        assertThat(service.calculateGrossPay(request).returnCode()).isEqualTo(11);
    }
}
```

---

## Now migrate this COBOL program

```cobol
{{selection}}
```

> **Tip**: Select the COBOL source file (or the paragraphs you want migrated) before running this prompt.
