# Thoas

A blazing fast JSON parser and generator in pure Erlang.

Thoas is an Erlang conversion of the Elixir based [Jason library][jason], and as
such the parser and generator are at least twice as fast as other Elixir/Erlang
libraries. The performance is comparable to `jiffy`, which is implemented in C
as a NIF. Thoas and Jason are usually only twice as slow.

Both parser and generator fully conform to
[RFC 8259](https://tools.ietf.org/html/rfc8259) and
[ECMA 404](http://www.ecma-international.org/publications/standards/Ecma-404.htm)
standards. The parser is tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite).

## Installation

### Erlang

```erlang
% rebar.config
{deps, [thoas]}
```

### Gleam

```shell
gleam add thoas
```

### Elixir

```elixir
# mix.exs
def deps do
  [{:thoas, "~> 0.1"}]
end
```

## Basic Usage

```erlang
> thoas:encode(#{"age" => 44, "name" => "Steve Irwin", "nationality" => "Australian"}).
<<"{\"age\":44,\"name\":\"Steve Irwin\",\"nationality\":\"Australian\"}">>

> thoas:decode(<<"{\"age"\:44,"\name"\:\"Steve Irwin\",\"nationality\":\"Australian\"}">>).
{ok, #{<<"age">> => 44, <<"name">> => <<"Steve Irwin">>, <<"nationality">> => <<"Australian">>}}
```

## Benchmarks

Benchmarks against most popular Elixir & Erlang json libraries can be executed after
going into the `bench/` folder and then executing `mix bench.encode` and `mix bench.decode`.
A HTML report of the benchmarks (after their execution) can be found in
`bench/output/encode.html` and `bench/output/decode.html` respectively.

## Differences to Jason

Jason has a couple feature differences compared to Poison.

  * Jason follows the JSON spec more strictly, for example it does not allow
    unescaped newline characters in JSON strings - e.g. `"\"\n\""` will
    produce a decoding error.
  * no support for decoding into data structures (the `as:` option).
  * no built-in encoders for `MapSet`, `Range` and `Stream`.
  * no support for encoding arbitrary structs - explicit implementation
    of the `Jason.Encoder` protocol is always required.
  * different pretty-printing customisation options (default `pretty: true` works the same)

If you require encoders for any of the unsupported collection types, I suggest
adding the needed implementations directly to your project:

```elixir
defimpl Jason.Encoder, for: [MapSet, Range, Stream] do
  def encode(struct, opts) do
    Jason.Encode.list(Enum.to_list(struct), opts)
  end
end
```

If you need to encode some struct that does not implement the protocol,
if you own the struct, you can derive the implementation specifying
which fields should be encoded to JSON:

```elixir
@derive {Jason.Encoder, only: [....]}
defstruct # ...
```

It is also possible to encode all fields, although this should be
used carefully to avoid accidentally leaking private information
when new fields are added:

```elixir
@derive Jason.Encoder
defstruct # ...
```

Finally, if you don't own the struct you want to encode to JSON,
you may use `Protocol.derive/3` placed outside of any module:

```elixir
Protocol.derive(Jason.Encoder, NameOfTheStruct, only: [...])
Protocol.derive(Jason.Encoder, NameOfTheStruct)
```

## License

Thoas is released under the Apache License 2.0 - see the [LICENSE](LICENSE) file.

Thoas is based off of [Jason][jason], which is also Apache 2.0 lienced.

Some elements of tests and benchmarks have their origins in the
[Poison library](https://github.com/devinus/poison) and were initially licensed under [CC0-1.0](https://creativecommons.org/publicdomain/zero/1.0/).

[jason]: https://github.com/michalmuskala/jason
