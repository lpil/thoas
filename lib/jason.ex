defmodule :thoas do
  @moduledoc """
  A blazing fast JSON parser and generator in pure Elixir.
  """

  @type escape :: :json | :unicode_safe | :html_safe | :javascript_safe
  @type maps :: :naive | :strict

  @type encode_opt :: {:escape, escape} | {:maps, maps}
  @type keys :: :atoms | :atoms! | :strings | :copy | (String.t() -> term)

  @type strings :: :reference | :copy

  @type decode_opt :: {:keys, keys} | {:strings, strings}

  @doc """
  Parses a JSON value from `input` iodata.

  ## Options

    * `:strings` - controls how strings (including keys) are decoded. Possible values are:

      * `:reference` (default) - when possible tries to create a sub-binary into the original
      * `:copy` - always copies the strings. This option is especially useful when parts of the
        decoded data will be stored for a long time (in ets or some process) to avoid keeping
        the reference to the original data.

  """
  def decode(input, opts \\ []) do
    input = :erlang.iolist_to_binary(input)
    :thoas_decode.parse(input, format_decode_opts(opts))
  end

  @doc """
  Generates JSON corresponding to `input`.

  The generation is controlled by the `Jason.Encoder` protocol,
  please refer to the module to read more on how to define the protocol
  for custom data types.

  ## Options

    * `:escape` - controls how strings are encoded. Possible values are:

      * `:json` (default) - the regular JSON escaping as defined by RFC 7159.
      * `:javascript_safe` - additionally escapes the LINE SEPARATOR (U+2028)
        and PARAGRAPH SEPARATOR (U+2029) characters to make the produced JSON
        valid JavaScript.
      * `:html_safe` - similar to `:javascript_safe`, but also escapes the `/`
        character to prevent XSS.
      * `:unicode_safe` - escapes all non-ascii characters.

    * `:maps` - controls how maps are encoded. Possible values are:

      * `:strict` - checks the encoded map for duplicate keys and raises
        if they appear. For example `%{:foo => 1, "foo" => 2}` would be
        rejected, since both keys would be encoded to the string `"foo"`.
      * `:naive` (default) - does not perform the check.
  """
  def encode(input, opts \\ []) do
    input
    |> do_encode(format_encode_opts(opts))
    |> :erlang.iolist_to_binary()
  end

  @doc """
  Generates JSON corresponding to `input` and returns iodata.

  This function should be preferred to `:thoas_encode/2`, if the generated
  JSON will be handed over to one of the IO functions or sent
  over the socket. The Erlang runtime is able to leverage vectorised
  writes and avoid allocating a continuous buffer for the whole
  resulting string, lowering memory use and increasing performance.

  """
  # TODO
  def encode_to_iodata(input, opts \\ []) do
    do_encode(input, format_encode_opts(opts))
  end

  defp do_encode(input, opts) do
    :thoas_encode.encode(input, opts)
  end

  defp format_encode_opts(opts) do
    # TODO
    Enum.into(opts, %{escape: :json, maps: :naive})
  end

  defp format_decode_opts(opts) do
    # TODO
    Enum.into(opts, %{keys: :strings, strings: :reference})
  end
end
