# CobolMigration

A reference hub of **GitHub Copilot resources** to accelerate the migration of enterprise COBOL systems to modern Java. It contains Copilot Instructions, custom chat agents, prompt files, and skills that you can drop directly into your own migration project.

---

## What's Included

| Resource | File | Purpose |
|----------|------|---------|
| **Copilot Instructions** | `.github/copilot-instructions.md` | Always-on context: COBOL/Java best practices, PIC-to-Java type mapping, migration strategy |
| **COBOL Expert Agent** | `.github/chatmodes/cobol-expert.chatmode.md` | Custom Copilot chat mode for deep COBOL analysis |
| **Java Expert Agent** | `.github/chatmodes/java-expert.chatmode.md` | Custom Copilot chat mode for idiomatic Java migration targets |
| **COBOL Unit Test Prompt** | `.github/prompts/cobol-unit-tests.prompt.md` | Generate COBOL-check `.cut` test suites for COBOL programs |
| **Java Unit Test Prompt** | `.github/prompts/java-unit-tests.prompt.md` | Generate JUnit 5 + Mockito + AssertJ test classes for migrated Java |
| **Migration Prompt** | `.github/prompts/cobol-to-java-migration.prompt.md` | Step-by-step COBOL → Java migration: analysis, domain model, service class, test skeleton |
| **COBOL Analysis Skill** | `.github/skills/cobol-analysis.md` | Pattern library teaching Copilot to decode complex COBOL constructs |

---

## How to Adopt This Into Your Own Project

### Step 1 — Copy the `.github` folder

```bash
# From your migration project root
cp -r path/to/CobolMigration/.github .
```

Or cherry-pick only the resources you need.

### Step 2 — Enable GitHub Copilot

Ensure GitHub Copilot (with chat) is enabled for your repository. The `.github/copilot-instructions.md` file is automatically loaded by Copilot Chat in VS Code, JetBrains IDEs, and GitHub.com.

### Step 3 — Use the Custom Agents

In Copilot Chat, switch to a custom chat mode by clicking the agent selector:

- **COBOL Expert** → Analyse a legacy COBOL program, extract business rules, identify migration risks.
- **Java Expert** → Generate idiomatic Java 17/21, Spring Boot services, and test classes.

### Step 4 — Run the Prompts

Open a COBOL or Java file, select relevant code, then run a prompt:

| Goal | Prompt File | How to Run |
|------|-------------|-----------|
| Generate COBOL unit tests | `cobol-unit-tests.prompt.md` | Select COBOL source → Copilot Chat → attach prompt |
| Migrate COBOL to Java | `cobol-to-java-migration.prompt.md` | Select COBOL source → Copilot Chat → attach prompt |
| Write Java unit tests | `java-unit-tests.prompt.md` | Select Java class → Copilot Chat → attach prompt |

### Step 5 — Use the COBOL Analysis Skill

The `.github/skills/cobol-analysis.md` file teaches Copilot a structured pattern library for:
- Packed decimal (`COMP-3`) → `BigDecimal`
- `REDEFINES` → sealed classes
- `OCCURS DEPENDING ON` → `List<T>`
- `GO TO` spaghetti → structured loops
- CICS commands → Spring REST controllers
- Embedded DB2 SQL → Spring Data JPA
- Copybooks → shared Java records/interfaces
- Date windowing → `java.time.LocalDate`

Reference this file in your Copilot chat session when Copilot doesn't understand a COBOL construct.

---

## Repository Structure

```
.github/
├── copilot-instructions.md          ← always-on Copilot context
├── chatmodes/
│   ├── cobol-expert.chatmode.md     ← COBOL specialist agent
│   └── java-expert.chatmode.md      ← Java migration agent
├── prompts/
│   ├── cobol-unit-tests.prompt.md   ← COBOL test generation
│   ├── java-unit-tests.prompt.md    ← Java test generation
│   └── cobol-to-java-migration.prompt.md  ← full migration workflow
└── skills/
    └── cobol-analysis.md            ← COBOL pattern recognition skill
README.md
```

---

## Quick-Start Example

1. Open `CalcInterest.cbl` in VS Code.
2. Select all (`Ctrl+A`).
3. Open Copilot Chat, switch to **COBOL Expert** mode.
4. Type: `Analyse this program and produce the migration analysis table.`
5. Review the output, then switch to **Java Expert** mode.
6. Attach the `cobol-to-java-migration.prompt.md` prompt and say: `Migrate this to Java.`
7. Attach the `java-unit-tests.prompt.md` prompt to the generated service class: `Generate the full JUnit 5 test suite.`

---

## Best Practices Summary

### COBOL
- Use `EVALUATE TRUE` instead of nested `IF` chains.
- Always check `FILE STATUS` after every file operation.
- Prefer `COMPUTE` for multi-operator arithmetic.
- Use meaningful paragraph names: `CALC-INTEREST`, `READ-CUSTOMER`.
- Document copybook field meanings with inline comments.

### Java (migration target)
- Use `BigDecimal` (never `double`) for all financial values.
- Use Java `record` types for immutable COBOL data group equivalents.
- Use `RoundingMode.HALF_UP` to match COBOL `ROUNDED` semantics.
- Annotate migrated methods with `// COBOL: <PARA-NAME>` on the first pass.
- Every migrated class must have ≥ 80 % unit test line coverage.

### Migration Strategy
- Follow the **Strangler Fig pattern**: wrap, replace one program at a time, retire.
- Write golden-file regression tests against COBOL output before migrating.
- Migrate data structures first, then business logic, then I/O.
- Never change business rules during the first migration pass.
