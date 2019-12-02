# Table creator

Sometimes you are tasked with importing a CSV file into a Postgres database.
It can be a hassle to manually derive what the corresponding table in Postgres
should look like.

This tool scans through CSV file(s) and tries to determine the correct types
for then to spit out a table definition that can then be used to create the table.

Example:

```
$ ./TableCreator test/data1.csv
- Processing file /path/to/test/data1.csv: 
! SUCCESS

  CREATE TABLE "data1" (
    "TEXT1" text,
    "TEXT2" text,
    "BOOL_COLUMN" boolean,
    "INTEGER" int,
    "FLOAT_DOT" real,
    "FLOAT_COMMA" real,
    "LOOKS_LIKE_BOOL" int
  )
 ``` 

## Assumptions

This tool makes a lot of assumptions:
- the columns type are one of: `text`, `integer`, `real` or `boolean`
- the file name should be the table name
- `;` is used as a column separator

## How to build

You can build and test the tool with `dotnet run`.
To create a release for Linux or macOS, run `make publish`.