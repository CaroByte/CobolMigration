---
mode: agent
description: Generate a comprehensive JUnit 5 test suite for a Java class that was migrated from COBOL, using Mockito, AssertJ, and Spring Boot Test.
---

# Generate Java Unit Tests (JUnit 5)

You are a Java testing expert specialising in unit tests for code migrated from COBOL. Generate a complete JUnit 5 test class for the Java source provided.

## Test Stack

| Concern | Library / Annotation |
|---------|---------------------|
| Test runner | JUnit 5 (Jupiter) — `@Test`, `@ParameterizedTest` |
| Mocking | Mockito 5 — `@Mock`, `@InjectMocks`, `@ExtendWith(MockitoExtension.class)` |
| Assertions | AssertJ — `assertThat(…).isEqualTo(…)` |
| Spring services | `@ExtendWith(SpringExtension.class)` or `@SpringBootTest` (integration) |
| JPA repositories | `@DataJpaTest` |
| REST controllers | `@WebMvcTest` + `MockMvc` |
| Financial arithmetic | `BigDecimal` comparisons with `assertThat(actual).isEqualByComparingTo(expected)` |

---

## Test Structure Rules

1. **One test class per production class.** Name: `<ClassName>Test.java` in the same package under `src/test/java/`.
2. **One test method per business rule or code path.** Method name format: `<methodName>_<scenario>_<expectedOutcome>` (e.g., `calculateInterest_zeroRate_throwsException`).
3. **Arrange–Act–Assert (AAA)** structure with blank lines separating each section.
4. **`@BeforeEach`** for shared test fixture setup (equivalent to COBOL test data initialisation).
5. **`@ParameterizedTest` + `@MethodSource`** for boundary-value and equivalence-class testing (replacing repetitive COBOL test cases).
6. **`BigDecimal` comparisons** must use `.isEqualByComparingTo()`, not `.isEqualTo()`, to avoid scale mismatches.
7. Every test must have a **Javadoc comment** explaining which COBOL business rule it validates.

---

## Coverage Requirements

Generate tests covering:

- ✅ **Happy path** — typical inputs produce expected outputs
- ✅ **Boundary values** — zero, minimum, maximum field values (matching COBOL PIC clause limits)
- ✅ **Null / empty inputs** — where the Java API allows nulls
- ✅ **Error paths** — invalid inputs, domain exceptions, repository failures
- ✅ **Each branch** of every `if`, `switch`, or `Optional` chain
- ✅ **Each enum constant** of domain enums (migrated from COBOL 88-level conditions)

---

## Example — migrated interest calculation service

### Production class
```java
package com.example.migration.interest;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Calculates simple interest. Migrated from COBOL program CALC-INT.
 */
@Service
public class InterestCalculationService {

    private final AccountRepository accountRepository;

    public InterestCalculationService(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    /**
     * Calculates annual interest for an account.
     * COBOL: MAIN-PARA / COMPUTE WS-INTEREST ROUNDED = WS-PRINCIPAL * WS-RATE / 100
     *
     * @throws AccountNotFoundException if account does not exist
     * @throws IllegalArgumentException if rate is zero or negative
     */
    public BigDecimal calculateInterest(String accountId) {
        Account account = accountRepository.findById(accountId)
            .orElseThrow(() -> new AccountNotFoundException(accountId));
        if (account.interestRate().compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Interest rate must be positive");
        }
        return account.balance()
            .multiply(account.interestRate())
            .divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);
    }
}
```

### Generated test class
```java
package com.example.migration.interest;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class InterestCalculationServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @InjectMocks
    private InterestCalculationService service;

    private static final String ACCOUNT_ID = "ACC-001";

    @BeforeEach
    void setUp() {
        // Default happy-path account — override per test as needed
        Account defaultAccount = new Account(ACCOUNT_ID, new BigDecimal("1000.00"), new BigDecimal("5.0000"));
        when(accountRepository.findById(ACCOUNT_ID)).thenReturn(Optional.of(defaultAccount));
    }

    /**
     * COBOL: COMPUTE WS-INTEREST ROUNDED = WS-PRINCIPAL * WS-RATE / 100
     * Happy path: 1000.00 × 5% = 50.00
     */
    @Test
    void calculateInterest_standardInputs_returnsCorrectInterest() {
        // Act
        BigDecimal result = service.calculateInterest(ACCOUNT_ID);

        // Assert
        assertThat(result).isEqualByComparingTo("50.00");
    }

    /**
     * COBOL: IF WS-RATE = ZEROS → MOVE 10 TO WS-RETURN-CODE
     * Zero rate must be rejected.
     */
    @Test
    void calculateInterest_zeroRate_throwsIllegalArgumentException() {
        // Arrange
        Account zeroRateAccount = new Account(ACCOUNT_ID, new BigDecimal("1000.00"), BigDecimal.ZERO);
        when(accountRepository.findById(ACCOUNT_ID)).thenReturn(Optional.of(zeroRateAccount));

        // Act & Assert
        assertThatIllegalArgumentException()
            .isThrownBy(() -> service.calculateInterest(ACCOUNT_ID))
            .withMessageContaining("Interest rate must be positive");
    }

    /**
     * COBOL: account not found → MOVE 35 TO WS-FILE-STATUS
     */
    @Test
    void calculateInterest_unknownAccountId_throwsAccountNotFoundException() {
        // Arrange
        when(accountRepository.findById("UNKNOWN")).thenReturn(Optional.empty());

        // Act & Assert
        assertThatExceptionOfType(AccountNotFoundException.class)
            .isThrownBy(() -> service.calculateInterest("UNKNOWN"));
    }

    /**
     * COBOL: boundary — zero principal should yield zero interest.
     */
    @Test
    void calculateInterest_zeroPrincipal_returnsZeroInterest() {
        // Arrange
        Account zeroPrincipalAccount = new Account(ACCOUNT_ID, BigDecimal.ZERO, new BigDecimal("5.0000"));
        when(accountRepository.findById(ACCOUNT_ID)).thenReturn(Optional.of(zeroPrincipalAccount));

        // Act
        BigDecimal result = service.calculateInterest(ACCOUNT_ID);

        // Assert
        assertThat(result).isEqualByComparingTo(BigDecimal.ZERO);
    }

    /**
     * COBOL: PIC 9(9)V9(2) boundary — maximum principal value.
     */
    @Test
    void calculateInterest_maximumPrincipal_doesNotOverflow() {
        // Arrange
        Account maxAccount = new Account(ACCOUNT_ID, new BigDecimal("999999999.99"), new BigDecimal("1.0000"));
        when(accountRepository.findById(ACCOUNT_ID)).thenReturn(Optional.of(maxAccount));

        // Act & Assert — must not throw ArithmeticException
        assertThatCode(() -> service.calculateInterest(ACCOUNT_ID)).doesNotThrowAnyException();
    }

    /**
     * Parameterized: multiple principal/rate/expected combinations from COBOL golden file.
     */
    @ParameterizedTest(name = "principal={0}, rate={1} → interest={2}")
    @MethodSource("interestScenarios")
    void calculateInterest_variousInputs_returnsExpectedInterest(
            String principal, String rate, String expected) {
        // Arrange
        Account account = new Account(ACCOUNT_ID, new BigDecimal(principal), new BigDecimal(rate));
        when(accountRepository.findById(ACCOUNT_ID)).thenReturn(Optional.of(account));

        // Act
        BigDecimal result = service.calculateInterest(ACCOUNT_ID);

        // Assert
        assertThat(result).isEqualByComparingTo(expected);
    }

    static Stream<org.junit.jupiter.params.provider.Arguments> interestScenarios() {
        return Stream.of(
            org.junit.jupiter.params.provider.Arguments.of("500.00",  "10.0000", "50.00"),
            org.junit.jupiter.params.provider.Arguments.of("250.50",  "4.0000",  "10.02"),
            org.junit.jupiter.params.provider.Arguments.of("100.00",  "12.5000", "12.50"),
            org.junit.jupiter.params.provider.Arguments.of("0.01",    "100.0000","0.01")
        );
    }
}
```

---

## Now generate tests for this class

Paste the Java source below and I will produce a complete JUnit 5 test class following the pattern above.

```java
{{selection}}
```

> **Tip**: Select the Java class (or method) you want tested before running this prompt. Copilot will use `{{selection}}` to insert the selected text automatically.
