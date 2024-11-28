# CS474-HW3
CS474 HW3 fuzzy logic expansion with partial evaluation and conditional constructs.

This project contains the new features required for CS474 HW3, including partial evaluation, and conditional functions including GREATER_EQUAL, GREATER_EQUAL_SET, IFTRUE, and ELSERUN. The report.docx include the detail explanation of the implementation and the semantics of this DSL.

## Installation

### Prerequisites

To run this project, you'll need:
- **Scala 2.13.x**
- **SBT (Scala Build Tool)**
- **Git** for version control.

### Steps

1. **Clone the Repository**:
   Open a terminal and run the following command:
   ```bash
   git clone https://github.com/Holyhbsun/CS474-HW3.git
   cd CS474-HW3

2. **Build the Project**:
   You can use SBT to build the project. In the project directory, run:
   ```bash
    sbt compile

3. **Run the Program**:
   To run the program, use:
   ```bash
    sbt run

4. **Run Tests**:
   To run the test, use:
   ```bash
    sbt test

## Usage
The following are the new features implemented in this project.
### Object Design
- **GREATER_EQUAL**: Allow for fuzzy comparisons between numbers.
- **GREATER_EQUAL_SET**: Allow for fuzzy comparisons between sets.
- **Conditional Expressions**: Objects like IFTRUE, THENEXECUTE, and ELSERUN are evaluated by first evaluating the condition and then the appropriate branch.

### Evaluation
- **Partial Evaluation**: Each FuzzyExpression implements the partialEvaluate method. It allows expressions to be partially evaluated based on the available variables defined in the environment.
- **evaluateExpression**: It evaluates FuzzyExpression instances with partial evaluations. It could now handle conditional expressions in addition to the previous one.
- **evaluateGateOperation**: Similar to previous one but supports partial evaluations.
- **evaluateSetOperation**: Similar to previous one but supports partial evaluations.