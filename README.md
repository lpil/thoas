# Thoas

A blazing fast JSON parser and generator in pure Erlang.

Thoas is an Erlang conversion of the Elixir based [Jason library][jason]. 

The parser and generator are at least twice as fast as other Elixir/Erlang
libraries. The performance is comparable to `jiffy`, which is implemented in C
as a NIF. Thoas and Jason are usually only twice as slow.

Both parser and generator fully conform to
[RFC 8259](https://tools.ietf.org/html/rfc8259) and
[ECMA 404](http://www.ecma-international.org/publications/standards/Ecma-404.htm)
standards. The parser is tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite).

If you like this library thank Michał and the Jason contributors. They did all
the hard work!

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
  [{:thoas, "~> 0.3"}]
end
```

## Basic Usage

```erlang
> thoas:encode(#{age => 44, name => <<"Steve Irwin">>, nationality => <<"Australian">>}).
<<"{\"age\":44,\"name\":\"Steve Irwin\",\"nationality\":\"Australian\"}">>

> thoas:decode(<<"{\"age\":44,\"name\":\"Steve Irwin\",\"nationality\":\"Australian\"}">>).
{ok, #{<<"age">> => 44, <<"name">> => <<"Steve Irwin">>, <<"nationality">> => <<"Australian">>}}
```

## Erlang <-> JSON mapping

Erlang                        | JSON
------                        | ------
`null`                        | `null`
`true`                        | `true`
`false`                       | `false`
`nil`                         | `"nil"`
`undefined`                   | `"undefined"`
`other_atom`                  | `"other_atom"`
`1`                           | `1`
`1.1`                         | `1.1`
`-2.0`                        | `-2.0`
`[]`                          | `[]`
`[1, 2]`                      | `[1,2]`
`"chars"`                     | `[99,104,97,114,115]`
`<<"text">>`                  | `"text"`
`<<"\n">>`                    | `"\\n"`
`#{}`                         | `{}`
`#{<<"name">> => <<"Lucy">>}` | `{"name":"Lucy"}`
`#{score => 42, win => true}` | `{"score":42,"win":true}`
`[{proplists, true}]`         | `{"proplists":true}`


## Benchmarks

Benchmarks against most popular Elixir & Erlang json libraries can be executed after
going into the `bench/` folder and then executing `mix bench.encode` and `mix bench.decode`.
A HTML report of the benchmarks (after their execution) can be found in
`bench/output/encode.html` and `bench/output/decode.html` respectively.

## Differences to Jason

Thoas has some feature differences compared to Jason.

- Thoas is written in Erlang.
- Thoas has no support for Elixir protocols.
- Thoas has no support for pretty-printing JSON.
- Thoas has no support for detecting duplicate object keys.
- Thoas has no support for decoding objects to ordered dictionaries.
- Thoas has no support for decoding object keys as atoms.
- Thoas has no support for decoding floats to Elixir decimals.
- Thoas has an additional non-recursive encoder API that may be useful when
  working within statically typed languages such as Gleam.

## Why not use Jason?

Jason rocks, but if you're writing Erlang, Gleam, or some other BEAM language
you probably don't want to pull in the Elixir compiler and their standard
libraries just to get a really fast JSON parser. Thoas is just Erlang and uses
rebar3, so it can be easily added as a dependency to projects written in any
BEAM language.

Thoas also has a non-recursive API that may be useful from statically typed
languages or in highly performance constrained scenarios.

## Why is it called Thoas?

Thoas was [a son of Jason](https://en.wikipedia.org/wiki/Thoas_(son_of_Jason)).

## License

Thoas is released under the Apache License 2.0 - see the [LICENSE](LICENSE) file.

Thoas is based off of [Jason][jason], which is also Apache 2.0 licenced.

Some elements of tests and benchmarks have their origins in the
[Poison library](https://github.com/devinus/poison) and were initially licensed under [CC0-1.0](https://creativecommons.org/publicdomain/zero/1.0/).

[jason]: https://github.com/michalmuskala/jason
