# GitHub Copilot Instructions — COBOL to Java Migration

## Project Purpose

This repository is a reference hub for migrating enterprise COBOL systems to modern Java. It contains GitHub Copilot Instructions, prompt files, custom agents, and skills designed to accelerate, standardise, and de-risk large-scale COBOL-to-Java migrations.

---

## Context You Must Always Apply

1. **Source language**: IBM Enterprise COBOL (COBOL 85 / COBOL 2002). Batch and online (CICS) programs are both in scope.
2. **Target language**: Java 17 or Java 21 (LTS), using Spring Boot 3.x for service layer, JPA/Spring Data for persistence, and JUnit 5 + Mockito for tests.
3. **Migration philosophy**: Preserve business logic exactly; modernise structure, idioms, and technology. Do **not** change what the program does; change *how* it does it.
4. **Quality bar**: Every migrated class must have unit tests with ≥ 80 % line coverage. Integration tests are required for all database and file I/O paths.

---

## COBOL Language Understanding

### Program Structure
A COBOL program has four mandatory divisions in order:

```cobol
IDENTIFICATION DIVISION.    -- metadata (program name, author, date)
ENVIRONMENT DIVISION.       -- platform/file assignments
DATA DIVISION.              -- all variable declarations
PROCEDURE DIVISION.         -- executable logic
```

### Data Division Sections
| Section | Purpose |
|---------|---------|
| `FILE SECTION` | Record layouts for files declared in ENVIRONMENT |
| `WORKING-STORAGE SECTION` | Program-level variables (global state) |
| `LOCAL-STORAGE SECTION` | Variables re-initialised on each CALL (like local variables) |
| `LINKAGE SECTION` | Parameters passed in by a calling program |

### PIC Clause Cheat-Sheet
| PIC | Java Equivalent | Notes |
|-----|-----------------|-------|
| `PIC 9(n)` | `long` / `BigDecimal` | Unsigned integer, n digits |
| `PIC S9(n)` | `long` / `BigDecimal` | Signed integer |
| `PIC 9(m)V9(n)` | `BigDecimal` | Implied decimal; m integer digits, n fractional |
| `PIC X(n)` | `String` | Fixed-length alphanumeric; right-pad with spaces |
| `PIC A(n)` | `String` | Alphabetic only |
| `PIC 9(n) COMP` | `int` / `long` | Binary (COMP/COMP-4) |
| `PIC 9(n) COMP-3` | `BigDecimal` | Packed decimal (BCD) |
| `PIC 1` | `boolean` | Single bit |

### Common COBOL Verbs → Java Idioms
| COBOL | Java |
|-------|------|
| `MOVE A TO B` | `b = a;` |
| `MOVE SPACES TO B` | `b = "  ";` (fixed width) or `b = "";` |
| `MOVE ZEROS TO B` | `b = 0;` or `b = BigDecimal.ZERO;` |
| `ADD A TO B` | `b += a;` |
| `SUBTRACT A FROM B` | `b -= a;` |
| `MULTIPLY A BY B GIVING C` | `c = a * b;` |
| `DIVIDE A INTO B GIVING C REMAINDER D` | `c = b / a; d = b % a;` |
| `COMPUTE X = (A + B) * C` | `x = (a + b) * c;` |
| `PERFORM PARA-NAME` | `methodName();` |
| `PERFORM PARA-NAME UNTIL condition` | `while (!condition) { methodName(); }` |
| `PERFORM VARYING I FROM 1 BY 1 UNTIL I > N` | `for (int i = 1; i <= n; i++)` |
| `EVALUATE TRUE … WHEN … END-EVALUATE` | `switch` expression or `if-else if` chain |
| `IF … ELSE … END-IF` | `if (…) { … } else { … }` |
| `CALL 'PROGRAM' USING WS-PARM` | Method call or service invocation |
| `STOP RUN` | `System.exit(0)` or `return` from `main()` |
| `GO TO` | Refactor to structured loop/method (avoid direct translation) |
| `STRING … DELIMITED SIZE INTO …` | `String.format()` or `StringBuilder.append()` |
| `UNSTRING … DELIMITED BY … INTO …` | `String.split()` |
| `INSPECT … TALLYING … REPLACING …` | `String.replace()` / regex |

### COBOL Best Practices to Recognise
- **Copybooks** (`COPY` statement): equivalent to Java interfaces or shared DTOs.
- **OCCURS DEPENDING ON**: variable-length arrays → Java `List<T>`.
- **REDEFINES**: union-like overlay → Java sealed classes or a type discriminator.
- **88-level condition names**: `boolean` helper fields or `enum` constants in Java.
- **PERFORM THRU**: multi-paragraph ranges → extract into a single method.
- **File STATUS codes**: map to Java exceptions or `Optional<>` return types.

---

## Java Target Best Practices

### Code Style
- Use **Java 17+** features: records, sealed classes, pattern matching (`instanceof`), text blocks.
- Prefer **immutable value objects** (`record`) for what COBOL WORKING-STORAGE fields represent.
- Use `BigDecimal` (never `double`) for all currency and financial calculations — mirrors COBOL COMP-3 semantics.
- Name methods in `camelCase` after the COBOL paragraph they replace; keep a `// COBOL: PARA-NAME` comment on the first migration pass.
- Keep classes small: one responsibility per class, ≤ 200 lines preferred.

### Spring Boot Conventions
- `@Service` for business-logic classes migrated from COBOL PROCEDUREs.
- `@Repository` + Spring Data JPA for VSAM/DB2 file/table access.
- `@Value` or `@ConfigurationProperties` for what COBOL ENVIRONMENT division hard-codes.
- Use `@Transactional` on service methods that were part of a COBOL batch unit of work.

### Error Handling
- Replace `FILE STATUS` checks with checked or unchecked exceptions.
- Use Spring's `@ControllerAdvice` / `ProblemDetail` (RFC 9457) for REST APIs.
- Never swallow exceptions silently; always log with SLF4J.

### Logging
```java
private static final Logger log = LoggerFactory.getLogger(MyClass.class);
log.info("Processing record {}", recordId);   // prefer {} placeholder, not string concat
```

---

## Migration Strategy Guidelines

### Recommended Approach: Strangler Fig Pattern
1. **Identify** bounded domains in the COBOL system (payroll, claims, inventory, etc.).
2. **Wrap** each domain behind a Java façade that still calls the original COBOL (via JNI, IBM CICS web service, or mainframe API).
3. **Replace** one COBOL program at a time with an equivalent Java service.
4. **Retire** the COBOL code once the Java service passes all regression tests.

### Migration Checklist per Program
- [ ] Document the program's purpose, inputs, and outputs.
- [ ] Map every WORKING-STORAGE field to a Java field/record.
- [ ] Map every FILE SECTION record to a Java DTO.
- [ ] Convert every paragraph to a private method.
- [ ] Preserve all business rules verbatim in the first pass.
- [ ] Write unit tests before refactoring business logic.
- [ ] Run the COBOL golden-file test suite against the Java output.

---

## Copilot Usage in This Repository

| Resource | Location | Purpose |
|----------|----------|---------|
| These instructions | `.github/copilot-instructions.md` | Always-on context for every Copilot interaction |
| COBOL Expert Agent | `.github/chatmodes/cobol-expert.chatmode.md` | Deep COBOL analysis and explanation |
| Java Expert Agent | `.github/chatmodes/java-expert.chatmode.md` | Java migration target guidance |
| COBOL Unit Test Prompt | `.github/prompts/cobol-unit-tests.prompt.md` | Generate COBOL unit tests |
| Java Unit Test Prompt | `.github/prompts/java-unit-tests.prompt.md` | Generate Java unit tests |
| Migration Prompt | `.github/prompts/cobol-to-java-migration.prompt.md` | Step-by-step program migration |
| COBOL Analysis Skill | `.github/skills/cobol-analysis.md` | Structured COBOL comprehension skill |

---

## General Copilot Behaviour Rules

- When asked to translate COBOL to Java, always produce a **full class** with package declaration, imports, and Javadoc.
- When a COBOL construct has no direct Java equivalent, explain the semantic difference in a comment.
- Always suggest JUnit 5 tests alongside any migrated method.
- When uncertain about a business rule embedded in COBOL, flag it with `// TODO: verify business rule — migrated from COBOL line <n>`.
- Prefer `BigDecimal` arithmetic over `int`/`long` whenever the source PIC clause contains a decimal point.
- Treat every `WORKING-STORAGE` section as a candidate for a Java `record` or a Spring `@ConfigurationProperties` bean.
