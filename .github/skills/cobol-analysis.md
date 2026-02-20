# Copilot Skill — COBOL Program Analysis

This skill teaches GitHub Copilot how to systematically analyse complex IBM Enterprise COBOL programs to extract meaning, structure, and business logic in preparation for Java migration.

## Skill Purpose

When Copilot encounters unfamiliar or complex COBOL code, it should apply the pattern-matching rules below to:
1. Identify the program type and intent
2. Map data structures to their Java equivalents
3. Extract hidden business rules
4. Flag high-risk constructs that require human review

---

## Pattern Library — COBOL Constructs to Recognise

### 1. Packed Decimal Arithmetic (COMP-3)

**Pattern to detect:**
```cobol
PIC 9(m)V9(n) COMP-3
```

**What it means:** Packed Binary-Coded Decimal (BCD). Each byte stores two decimal digits. The `V` is an implied (invisible) decimal point — it is **not stored** in memory.

**Migration rule:**
- Always map to `java.math.BigDecimal`
- Preserve the scale: `n` decimal places → `.setScale(n, RoundingMode.HALF_UP)`
- Beware of COBOL's default `ROUNDED` behaviour: it rounds half-up

**Example:**
```cobol
01 WS-AMOUNT  PIC 9(7)V9(2) COMP-3.  *> 7 integer + 2 decimal digits
COMPUTE WS-AMOUNT ROUNDED = WS-PRICE * WS-QTY.
```
```java
// Java equivalent
BigDecimal amount = price.multiply(qty).setScale(2, RoundingMode.HALF_UP);
```

---

### 2. REDEFINES — Union-Like Data Overlays

**Pattern to detect:**
```cobol
01 WS-DATE-NUMERIC    PIC 9(8).
01 WS-DATE-FORMATTED  REDEFINES WS-DATE-NUMERIC.
   05 WS-YEAR         PIC 9(4).
   05 WS-MONTH        PIC 9(2).
   05 WS-DAY          PIC 9(2).
```

**What it means:** Two or more names pointing to the **same memory area**, interpreted differently. This is COBOL's union type.

**Migration rule:**
- Use a Java `sealed interface` with multiple `record` implementations, OR
- Use a single class with factory methods that parse the raw value differently
- Prefer a proper domain type (`LocalDate`) over raw `int` fields

**Example:**
```java
// Prefer this:
LocalDate date = LocalDate.parse(ws_date_numeric.toString(),
    DateTimeFormatter.ofPattern("yyyyMMdd"));
```

---

### 3. OCCURS DEPENDING ON — Variable-Length Arrays

**Pattern to detect:**
```cobol
01 WS-TABLE.
   05 WS-MAX-ITEMS   PIC 9(3).
   05 WS-ITEM OCCURS 1 TO 100 TIMES DEPENDING ON WS-MAX-ITEMS.
      10 WS-ITEM-ID  PIC X(10).
      10 WS-ITEM-AMT PIC 9(7)V9(2) COMP-3.
```

**What it means:** A variable-length array bounded at runtime by another field.

**Migration rule:**
- Map to `java.util.List<ItemRecord>` where `ItemRecord` is a `record`
- The `WS-MAX-ITEMS` field becomes `items.size()`
- Do not pre-size the list; add items via `list.add()`

---

### 4. GO TO Spaghetti

**Pattern to detect:**
```cobol
PARA-A.
    ...
    GO TO PARA-C.
PARA-B.
    ...
PARA-C.
    ...
    GO TO PARA-A.   *> loop back
```

**What it means:** Unstructured control flow. Extremely common in COBOL written before COBOL 85 introduced structured verbs.

**Migration rule:**
- Do **not** translate to Java `goto` (Java has no `goto`).
- Identify the loop intent: if `GO TO PARA-A` is a loop, replace with `while`/`for`.
- If it is a fall-through, reorganise paragraphs into a sequential `do { … } while` or a linear method call sequence.
- Flag with `// TODO: verify control flow — migrated from GO TO`

---

### 5. PERFORM THRU — Multi-Paragraph Ranges

**Pattern to detect:**
```cobol
PERFORM PARA-100-START THRU PARA-100-END.
```

**What it means:** Execute all paragraphs from `PARA-100-START` to `PARA-100-END` sequentially. The range boundary is implicit (it depends on source order).

**Migration rule:**
- Replace with explicit sequential method calls for each paragraph in the range.
- Do not rely on source order — verify what paragraphs fall between the start and end.

---

### 6. CICS Commands — Online Transaction Processing

**Pattern to detect:**
```cobol
EXEC CICS READ FILE('CUSTFILE')
          INTO(WS-CUSTOMER-RECORD)
          RIDFLD(WS-CUST-ID)
          RESP(WS-CICS-RESP)
END-EXEC.
```

**What it means:** A CICS API call for VSAM file I/O in an online (non-batch) context.

**Migration rule:**
- Map to a Spring Data JPA/JDBC repository method.
- `RESP` code `NORMAL` (0) → no exception; `NOTFND` (13) → `Optional.empty()`; other → exception.
- The entire CICS program maps to a Spring Boot `@RestController` or `@MessageListener`.

---

### 7. EXEC SQL — Embedded DB2

**Pattern to detect:**
```cobol
EXEC SQL
    SELECT CUST_NAME, CUST_BAL
    INTO :WS-CUST-NAME, :WS-CUST-BAL
    FROM CUSTOMER
    WHERE CUST_ID = :WS-CUST-ID
END-EXEC.
IF SQLCODE NOT = 0
    PERFORM HANDLE-DB-ERROR
END-IF.
```

**What it means:** DB2 SQL embedded in COBOL. Host variables (`:WS-*`) are COBOL fields.

**Migration rule:**
- Use `Spring Data JPA` `@Query` or a `JpaRepository` method.
- `SQLCODE 0` → success; `SQLCODE +100` → no rows (return `Optional.empty()`); negative `SQLCODE` → throw `DataAccessException`.
- Map each host variable to a method parameter or a JPA entity field.

---

### 8. Copybooks — Shared Data Definitions

**Pattern to detect:**
```cobol
COPY CUSTMAST.
```
or
```cobol
COPY ERRORCDS REPLACING ==:PREFIX:== BY ==WS==.
```

**What it means:** A `COPY` statement inlines another file (the copybook) at compile time. It is COBOL's `#include` / interface mechanism.

**Migration rule:**
- One copybook → one Java `record` or `interface` in a `shared` package.
- `REPLACING` substitutions → the Java type is still one class; ignore the prefix.
- Copybooks used in multiple programs → place the Java equivalent in a `common` Maven module.

---

### 9. SORT Verb — In-Program Sorting

**Pattern to detect:**
```cobol
SORT SORT-FILE
    ON ASCENDING KEY SR-CUST-ID
    USING INPUT-FILE
    GIVING OUTPUT-FILE.
```

**What it means:** COBOL built-in sort using an external SORT work file.

**Migration rule:**
- Read all records into a `List<T>`, sort with `list.sort(Comparator.comparing(T::custId))`, write back.
- For large datasets, use Spring Batch `SortedItemReader` or a DB `ORDER BY` clause.

---

### 10. Date Arithmetic — Century Date Handling

**Pattern to detect:**
```cobol
MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY.  *> yyyymmdd
IF WS-BIRTH-YEAR < 50
    MOVE 2000 TO WS-CENTURY
ELSE
    MOVE 1900 TO WS-CENTURY
END-IF.
```

**What it means:** COBOL programs written before Y2K often use 2-digit years with windowing logic. 2-digit year < 50 → 20xx; ≥ 50 → 19xx.

**Migration rule:**
- Use `java.time.LocalDate`, `Year`, `YearMonth` — no 2-digit years.
- Apply the same windowing logic if the data store still has 2-digit years.
- Flag every date field: `// TODO: verify Y2K windowing — confirm century assumption`

---

## Using This Skill

When Copilot sees unfamiliar COBOL, it should:
1. Scan for the patterns above.
2. Name each pattern found.
3. Explain what the pattern does in plain English.
4. Provide the recommended Java equivalent.
5. List any `// TODO` markers needed in the migrated code.

This skill is especially useful with the **COBOL Expert** custom agent (`.github/chatmodes/cobol-expert.chatmode.md`) and the **Migration Prompt** (`.github/prompts/cobol-to-java-migration.prompt.md`).
