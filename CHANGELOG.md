# Changelog

## v1.2.1 - 2024-05-13

- Corrected typespec.

## v1.2.0 - 2023-10-17

- The `thoas:decode/2` function now accepts the `keys` option which can be one
  of `reference`, `copy`, `to_existing_atom`, or `to_atom`.

## v1.1.1 - 2023-10-11

- Corrected an incorrect typespec.

## v1.1.0 - 2023-10-11

- Dates and datetimes may now be encoded with `thoas:encode/2`.

## v1.0.0 - 2023-01-23

- `float_to_binary(Float, [short])` is now used for encoding floats. This
  improves performance and memory usage, but means this library now requires OTP
  25.0 or later.

## v0.4.1 - 2023-01-23

- Corrected the `json_term` type to include `null`, `true`, and `false`.

## v0.4.0 - 2022-08-26

- Adds support for integer keys.

## v0.3.0 - 2022-07-27

- Proplists may now be encoded with `thoas:encode/2`.

## v0.2.0 - 2021-12-29

- The atom `null` is now used to represent `null`, not `nil` as was previously
  used.

## v0.1.0 - 2021-12-28

- Initial version
