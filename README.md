# edk2_expr

A rust library crate aim for implementing [EDK2 Meta-Data Expression](https://tianocore-docs.github.io/edk2-MetaDataExpressionSyntaxSpecification/release-1.30/) parsing and evaluating.

## Usage

Use `parse_expr(&str)` to create a expression syntax tree.  
Use `eval(&str, &dict)` to evaluating the result of expression.
