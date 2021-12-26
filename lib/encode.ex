defmodule :jaserl_encode do
  @moduledoc """
  Utilities for encoding elixir values to JSON.
  """

  # TODO
  import Bitwise

  @doc false
  @spec encode(any, map) :: iodata
  def encode(value, opts) do
    escape = escape_function(opts)
    value(value, escape)
  end

  defp escape_function(%{escape: escape}) do
    case escape do
      :json -> &escape_json/3
      :html -> &escape_html/3
      :unicode -> &escape_unicode/3
      :javascript -> &escape_javascript/3
    end
  end

  @doc false
  # We use this directly in the helpers and deriving for extra speed
  def value(value, escape) when is_atom(value) do
    encode_atom(value, escape)
  end

  def value(value, escape) when is_binary(value) do
    encode_string(value, escape)
  end

  def value(value, _escape) when is_integer(value) do
    integer(value)
  end

  def value(value, _escape) when is_float(value) do
    float(value)
  end

  def value(value, escape) when is_list(value) do
    list(value, escape)
  end

  def value(value, escape) when is_map(value) do
    case :maps.to_list(value) do
      [] -> "{}"
      keyword -> map_naive(keyword, escape)
    end
  end

  @compile {:inline, integer: 1, float: 1}

  def atom(atom, escape) do
    encode_atom(atom, escape)
  end

  defp encode_atom(nil, _escape), do: "null"
  defp encode_atom(true, _escape), do: "true"
  defp encode_atom(false, _escape), do: "false"

  defp encode_atom(atom, escape),
    do: encode_string(:erlang.atom_to_binary(atom, :utf8), escape)

  @spec integer(integer) :: iodata
  def integer(integer) do
    :erlang.integer_to_list(integer)
  end

  @spec float(float) :: iodata
  def float(float) do
    :io_lib_format.fwrite_g(float)
  end

  defp list([], _escape) do
    "[]"
  end

  defp list([head | tail], escape) do
    [
      ?[,
      value(head, escape)
      | list_loop(tail, escape)
    ]
  end

  defp list_loop([], _escape) do
    ']'
  end

  defp list_loop([head | tail], escape) do
    [
      ?,,
      value(head, escape)
      | list_loop(tail, escape)
    ]
  end

  def keyword(list, _) when list == [], do: "{}"

  def keyword(list, escape) when is_list(list) do
    map_naive(list, escape)
  end

  def map(value, escape) do
    case :maps.to_list(value) do
      [] -> "{}"
      keyword -> map_naive(keyword, escape)
    end
  end

  defp map_naive([{key, value} | tail], escape) do
    [
      "{\"",
      key(key, escape),
      "\":",
      value(value, escape)
      | map_naive_loop(tail, escape)
    ]
  end

  defp map_naive_loop([], _escape) do
    '}'
  end

  defp map_naive_loop([{key, value} | tail], escape) do
    [
      ",\"",
      key(key, escape),
      "\":",
      value(value, escape)
      | map_naive_loop(tail, escape)
    ]
  end

  @doc false
  # This is used in the helpers and deriving implementation
  def key(string, escape) when is_binary(string) do
    escape.(string, string, 0)
  end

  def key(atom, escape) when is_atom(atom) do
    string = Atom.to_string(atom)
    escape.(string, string, 0)
  end

  def key(other, escape) do
    string = String.Chars.to_string(other)
    escape.(string, string, 0)
  end

  def string(string, escape) do
    encode_string(string, escape)
  end

  defp encode_string(string, escape) do
    [?\", escape.(string, string, 0), ?\"]
  end

  slash_escapes = Enum.zip('\b\t\n\f\r\"\\', 'btnfr"\\')
  surogate_escapes = Enum.zip([0x2028, 0x2029], ["\\u2028", "\\u2029"])
  ranges = [{0x00..0x1F, :unicode} | slash_escapes]
  html_ranges = [{0x00..0x1F, :unicode}, {?/, ?/} | slash_escapes]
  escape_jt = :jaserl_codegen.jump_table(html_ranges, :error)

  Enum.each(escape_jt, fn
    {byte, :unicode} ->
      sequence = List.to_string(:io_lib.format("\\u~4.16.0B", [byte]))
      defp escape(unquote(byte)), do: unquote(sequence)

    {byte, char} when is_integer(char) ->
      defp escape(unquote(byte)), do: unquote(<<?\\, char>>)

    {byte, :error} ->
      defp escape(unquote(byte)), do: throw(:error)
  end)

  ## regular JSON escape

  json_jt = :jaserl_codegen.jump_table(ranges, :chunk, 0x7F + 1)

  defp escape_json(data, original, skip) do
    escape_json(data, [], original, skip)
  end

  Enum.map(json_jt, fn
    {byte, :chunk} ->
      defp escape_json(<<byte, rest::bits>>, acc, original, skip)
           when byte === unquote(byte) do
        escape_json_chunk(rest, acc, original, skip, 1)
      end

    {byte, _escape} ->
      defp escape_json(<<byte, rest::bits>>, acc, original, skip)
           when byte === unquote(byte) do
        acc = [acc | escape(byte)]
        escape_json(rest, acc, original, skip + 1)
      end
  end)

  defp escape_json(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0x7FF do
    escape_json_chunk(rest, acc, original, skip, 2)
  end

  defp escape_json(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0xFFFF do
    escape_json_chunk(rest, acc, original, skip, 3)
  end

  defp escape_json(<<_char::utf8, rest::bits>>, acc, original, skip) do
    escape_json_chunk(rest, acc, original, skip, 4)
  end

  defp escape_json(<<>>, acc, _original, _skip) do
    acc
  end

  defp escape_json(<<byte, _rest::bits>>, _acc, original, _skip) do
    throw_invalid_byte_error(byte, original)
  end

  Enum.map(json_jt, fn
    {byte, :chunk} ->
      defp escape_json_chunk(<<byte, rest::bits>>, acc, original, skip, len)
           when byte === unquote(byte) do
        escape_json_chunk(rest, acc, original, skip, len + 1)
      end

    {byte, _escape} ->
      defp escape_json_chunk(<<byte, rest::bits>>, acc, original, skip, len)
           when byte === unquote(byte) do
        part = binary_part(original, skip, len)
        acc = [acc, part | escape(byte)]
        escape_json(rest, acc, original, skip + len + 1)
      end
  end)

  defp escape_json_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0x7FF do
    escape_json_chunk(rest, acc, original, skip, len + 2)
  end

  defp escape_json_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0xFFFF do
    escape_json_chunk(rest, acc, original, skip, len + 3)
  end

  defp escape_json_chunk(<<_char::utf8, rest::bits>>, acc, original, skip, len) do
    escape_json_chunk(rest, acc, original, skip, len + 4)
  end

  defp escape_json_chunk(<<>>, acc, original, skip, len) do
    part = binary_part(original, skip, len)
    [acc | part]
  end

  defp escape_json_chunk(<<byte, _rest::bits>>, _acc, original, _skip, _len) do
    throw_invalid_byte_error(byte, original)
  end

  ## javascript safe JSON escape

  defp escape_javascript(data, original, skip) do
    escape_javascript(data, [], original, skip)
  end

  :lists.map(
    fn
      {byte, :chunk} ->
        defp escape_javascript(<<byte, rest::bits>>, acc, original, skip)
             when byte === unquote(byte) do
          escape_javascript_chunk(rest, acc, original, skip, 1)
        end

      {byte, _escape} ->
        defp escape_javascript(<<byte, rest::bits>>, acc, original, skip)
             when byte === unquote(byte) do
          acc = [acc | escape(byte)]
          escape_javascript(rest, acc, original, skip + 1)
        end
    end,
    json_jt
  )

  defp escape_javascript(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0x7FF do
    escape_javascript_chunk(rest, acc, original, skip, 2)
  end

  :lists.map(
    fn {byte, escape} ->
      defp escape_javascript(<<unquote(byte)::utf8, rest::bits>>, acc, original, skip) do
        acc = [acc | unquote(escape)]
        escape_javascript(rest, acc, original, skip + 3)
      end
    end,
    surogate_escapes
  )

  defp escape_javascript(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0xFFFF do
    escape_javascript_chunk(rest, acc, original, skip, 3)
  end

  defp escape_javascript(<<_char::utf8, rest::bits>>, acc, original, skip) do
    escape_javascript_chunk(rest, acc, original, skip, 4)
  end

  defp escape_javascript(<<>>, acc, _original, _skip) do
    acc
  end

  defp escape_javascript(<<byte, _rest::bits>>, _acc, original, _skip) do
    throw_invalid_byte_error(byte, original)
  end

  :lists.map(
    fn
      {byte, :chunk} ->
        defp escape_javascript_chunk(<<byte, rest::bits>>, acc, original, skip, len)
             when byte === unquote(byte) do
          escape_javascript_chunk(rest, acc, original, skip, len + 1)
        end

      {byte, _escape} ->
        defp escape_javascript_chunk(<<byte, rest::bits>>, acc, original, skip, len)
             when byte === unquote(byte) do
          part = binary_part(original, skip, len)
          acc = [acc, part | escape(byte)]
          escape_javascript(rest, acc, original, skip + len + 1)
        end
    end,
    json_jt
  )

  defp escape_javascript_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0x7FF do
    escape_javascript_chunk(rest, acc, original, skip, len + 2)
  end

  :lists.map(
    fn {byte, escape} ->
      defp escape_javascript_chunk(<<unquote(byte)::utf8, rest::bits>>, acc, original, skip, len) do
        part = binary_part(original, skip, len)
        acc = [acc, part | unquote(escape)]
        escape_javascript(rest, acc, original, skip + len + 3)
      end
    end,
    surogate_escapes
  )

  defp escape_javascript_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0xFFFF do
    escape_javascript_chunk(rest, acc, original, skip, len + 3)
  end

  defp escape_javascript_chunk(<<_char::utf8, rest::bits>>, acc, original, skip, len) do
    escape_javascript_chunk(rest, acc, original, skip, len + 4)
  end

  defp escape_javascript_chunk(<<>>, acc, original, skip, len) do
    part = binary_part(original, skip, len)
    [acc | part]
  end

  defp escape_javascript_chunk(<<byte, _rest::bits>>, _acc, original, _skip, _len) do
    throw_invalid_byte_error(byte, original)
  end

  ## HTML safe JSON escape

  html_jt = :jaserl_codegen.jump_table(html_ranges, :chunk, 0x7F + 1)

  defp escape_html(data, original, skip) do
    escape_html(data, [], original, skip)
  end

  Enum.map(html_jt, fn
    {byte, :chunk} ->
      defp escape_html(<<byte, rest::bits>>, acc, original, skip)
           when byte === unquote(byte) do
        escape_html_chunk(rest, acc, original, skip, 1)
      end

    {byte, _escape} ->
      defp escape_html(<<byte, rest::bits>>, acc, original, skip)
           when byte === unquote(byte) do
        acc = [acc | escape(byte)]
        escape_html(rest, acc, original, skip + 1)
      end
  end)

  defp escape_html(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0x7FF do
    escape_html_chunk(rest, acc, original, skip, 2)
  end

  Enum.map(surogate_escapes, fn {byte, escape} ->
    defp escape_html(<<unquote(byte)::utf8, rest::bits>>, acc, original, skip) do
      acc = [acc | unquote(escape)]
      escape_html(rest, acc, original, skip + 3)
    end
  end)

  defp escape_html(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0xFFFF do
    escape_html_chunk(rest, acc, original, skip, 3)
  end

  defp escape_html(<<_char::utf8, rest::bits>>, acc, original, skip) do
    escape_html_chunk(rest, acc, original, skip, 4)
  end

  defp escape_html(<<>>, acc, _original, _skip) do
    acc
  end

  defp escape_html(<<byte, _rest::bits>>, _acc, original, _skip) do
    throw_invalid_byte_error(byte, original)
  end

  Enum.map(html_jt, fn
    {byte, :chunk} ->
      defp escape_html_chunk(<<byte, rest::bits>>, acc, original, skip, len)
           when byte === unquote(byte) do
        escape_html_chunk(rest, acc, original, skip, len + 1)
      end

    {byte, _escape} ->
      defp escape_html_chunk(<<byte, rest::bits>>, acc, original, skip, len)
           when byte === unquote(byte) do
        part = binary_part(original, skip, len)
        acc = [acc, part | escape(byte)]
        escape_html(rest, acc, original, skip + len + 1)
      end
  end)

  defp escape_html_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0x7FF do
    escape_html_chunk(rest, acc, original, skip, len + 2)
  end

  Enum.map(surogate_escapes, fn {byte, escape} ->
    defp escape_html_chunk(<<unquote(byte)::utf8, rest::bits>>, acc, original, skip, len) do
      part = binary_part(original, skip, len)
      acc = [acc, part | unquote(escape)]
      escape_html(rest, acc, original, skip + len + 3)
    end
  end)

  defp escape_html_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0xFFFF do
    escape_html_chunk(rest, acc, original, skip, len + 3)
  end

  defp escape_html_chunk(<<_char::utf8, rest::bits>>, acc, original, skip, len) do
    escape_html_chunk(rest, acc, original, skip, len + 4)
  end

  defp escape_html_chunk(<<>>, acc, original, skip, len) do
    part = binary_part(original, skip, len)
    [acc | part]
  end

  defp escape_html_chunk(<<byte, _rest::bits>>, _acc, original, _skip, _len) do
    throw_invalid_byte_error(byte, original)
  end

  ## unicode escape

  defp escape_unicode(data, original, skip) do
    escape_unicode(data, [], original, skip)
  end

  Enum.map(json_jt, fn
    {byte, :chunk} ->
      defp escape_unicode(<<byte, rest::bits>>, acc, original, skip)
           when byte === unquote(byte) do
        escape_unicode_chunk(rest, acc, original, skip, 1)
      end

    {byte, _escape} ->
      defp escape_unicode(<<byte, rest::bits>>, acc, original, skip)
           when byte === unquote(byte) do
        acc = [acc | escape(byte)]
        escape_unicode(rest, acc, original, skip + 1)
      end
  end)

  defp escape_unicode(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0xFF do
    acc = [acc, "\\u00" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + 2)
  end

  defp escape_unicode(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0x7FF do
    acc = [acc, "\\u0" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + 2)
  end

  defp escape_unicode(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0xFFF do
    acc = [acc, "\\u0" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + 3)
  end

  defp escape_unicode(<<char::utf8, rest::bits>>, acc, original, skip)
       when char <= 0xFFFF do
    acc = [acc, "\\u" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + 3)
  end

  defp escape_unicode(<<char::utf8, rest::bits>>, acc, original, skip) do
    char = char - 0x10000

    acc = [
      acc,
      "\\uD",
      :erlang.integer_to_list(0x800 ||| char >>> 10, 16),
      "\\uD" | :erlang.integer_to_list(0xC00 ||| (char &&& 0x3FF), 16)
    ]

    escape_unicode(rest, acc, original, skip + 4)
  end

  defp escape_unicode(<<>>, acc, _original, _skip) do
    acc
  end

  defp escape_unicode(<<byte, _rest::bits>>, _acc, original, _skip) do
    throw_invalid_byte_error(byte, original)
  end

  Enum.map(json_jt, fn
    {byte, :chunk} ->
      defp escape_unicode_chunk(<<byte, rest::bits>>, acc, original, skip, len)
           when byte === unquote(byte) do
        escape_unicode_chunk(rest, acc, original, skip, len + 1)
      end

    {byte, _escape} ->
      defp escape_unicode_chunk(<<byte, rest::bits>>, acc, original, skip, len)
           when byte === unquote(byte) do
        part = binary_part(original, skip, len)
        acc = [acc, part | escape(byte)]
        escape_unicode(rest, acc, original, skip + len + 1)
      end
  end)

  defp escape_unicode_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0xFF do
    part = binary_part(original, skip, len)
    acc = [acc, part, "\\u00" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + len + 2)
  end

  defp escape_unicode_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0x7FF do
    part = binary_part(original, skip, len)
    acc = [acc, part, "\\u0" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + len + 2)
  end

  defp escape_unicode_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0xFFF do
    part = binary_part(original, skip, len)
    acc = [acc, part, "\\u0" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + len + 3)
  end

  defp escape_unicode_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len)
       when char <= 0xFFFF do
    part = binary_part(original, skip, len)
    acc = [acc, part, "\\u" | :erlang.integer_to_list(char, 16)]
    escape_unicode(rest, acc, original, skip + len + 3)
  end

  defp escape_unicode_chunk(<<char::utf8, rest::bits>>, acc, original, skip, len) do
    char = char - 0x10000
    part = binary_part(original, skip, len)

    acc = [
      acc,
      part,
      "\\uD",
      :erlang.integer_to_list(0x800 ||| char >>> 10, 16),
      "\\uD" | :erlang.integer_to_list(0xC00 ||| (char &&& 0x3FF), 16)
    ]

    escape_unicode(rest, acc, original, skip + len + 4)
  end

  defp escape_unicode_chunk(<<>>, acc, original, skip, len) do
    part = binary_part(original, skip, len)
    [acc | part]
  end

  defp escape_unicode_chunk(<<byte, _rest::bits>>, _acc, original, _skip, _len) do
    throw_invalid_byte_error(byte, original)
  end

  @compile {:inline, throw_invalid_byte_error: 2}
  defp throw_invalid_byte_error(byte, original) do
    throw({:invalid_byte, <<"0x"::utf8, :erlang.integer_to_binary(byte, 16)::binary>>, original})
  end
end
