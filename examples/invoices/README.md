# Invoices

A small invoicing system for PC FAND: customers, products, invoices and
their items, three reports and a Prolog query. It uses every kind of
chapter.

    cd examples/invoices
    ../../bin/fand invoices --source-in src    # tools/build.sh does this
    ../../bin/fand invoices

Log in as `admin` (may change the settings) or `clerk`, then choose
Data > Load sample data.

| Chapter | Shows |
|---|---|
| 001 U | users: name, code, password, access rights |
| 002 F PARAM | a parametric file (`#K @@`): its fields are global variables, `PARAM.VatRate` |
| 003 F CUSTOMER.X | an indexed file; an alternative key sorted by the national alphabet (`ByName(@) * ~Name`) |
| 004 F PRODUCT.X | a check (`#L`) |
| 005 F INVOICE.X | a foreign key (`CUSTOMER Cust`), computed fields (`#C`), an additive assignment to a global variable (`#A PARAM.NextNo := …`), checks, implicit values (`#I`) |
| 006 F ITEM.X | a duplicate key and two foreign keys, an additive change (`#A INVOICE.Total += Amount`), a dependency taking the price from the product (`#D`) |
| 007 F SALES.X | the output of the transformation |
| 008 D | user functions `WithVat` and `DaysLate` |
| 009 E InvForm | a form: a screen template with field masks |
| 010 I Head | the report heading, put into both reports by `{$include Head}` |
| 011 R InvList | a report grouped by customer (`#CH`, `#DE`, `#CF`, `#RF`), sums, `cond`, user functions |
| 012 R InvPrint | a report from two files: every invoice with its items |
| 013 M Summary | a transformation: one SALES record per customer (`sum`, `I1.count`) |
| 014 P ShowLine | a procedure with parameters, called from Prolog |
| 015 L Unpaid | Prolog over the data files (`#DATABASE @invoice(...)`) |
| 016 P main | the menu bar |
| 017 P Samples | record variables; `writerec(r,0,+)` appends with the additive changes |
| 018 P Settings | access rights (`trust(2)`) |
| 019 P Sales | `merge` and an automatic report |
| 020 P ShowUnpaid | `lproc` in a window |
| 021 P About | |

Edit the chapters in FAND's developer environment (`fand invoices D`), or
as the text files in `src/` and store them back with `--source-in src`.

Worth knowing when writing FAND:

- The logical operators are `^` (not), `&` (and), `|` (or).
- A data file must not have the task's name.
- The number of records is `CUSTOMER.nrecs`, not `nrecs(CUSTOMER)`.
- `count` is a special function in reports and transformations; a field
  named Count cannot be summed.
- Task names have at most 8 characters, as on DOS.
