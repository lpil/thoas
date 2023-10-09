defmodule Jason.DecodeTest do
  use ExUnit.Case, async: true

  test "numbers" do
    assert_fail_with("-", :unexpected_end_of_input)
    assert_fail_with("--1", {:unexpected_byte, "0x2D", 1})
    assert_fail_with("01", {:unexpected_byte, "0x31", 1})
    assert_fail_with(".1", {:unexpected_byte, "0x2E", 0})
    assert_fail_with("1.", :unexpected_end_of_input)
    assert_fail_with("1e", :unexpected_end_of_input)
    assert_fail_with("1.0e+", :unexpected_end_of_input)
    assert_fail_with("1e999", {:unexpected_sequence, "1e999", 0})

    assert parse!("0") == 0
    assert parse!("1") == 1
    assert parse!("-0") == 0
    assert parse!("-1") == -1
    assert parse!("0.1") == 0.1
    assert parse!("-0.1") == -0.1
    assert parse!("0e0") == 0
    assert parse!("0E0") == 0
    assert parse!("1e0") == 1
    assert parse!("1E0") == 1
    assert parse!("1.0e0") == 1.0
    assert parse!("1e+0") == 1
    assert parse!("1.0e+0") == 1.0
    assert parse!("0.1e1") == 0.1e1
    assert parse!("0.1e-1") == 0.1e-1
    assert parse!("99.99e99") == 99.99e99
    assert parse!("-99.99e-99") == -99.99e-99
    assert parse!("123456789.123456789e123") == 123_456_789.123456789e123
  end

  test "strings" do
    assert_fail_with(~s("), :unexpected_end_of_input)
    assert_fail_with(~s("\\"), :unexpected_end_of_input)
    assert_fail_with(~s("\\k"), {:unexpected_byte, "0x6B", 2})
    assert_fail_with(<<?\", 128, ?\">>, {:unexpected_byte, "0x80", 1})
    assert_fail_with(~s("\\u2603\\"), :unexpected_end_of_input)

    assert_fail_with(
      ~s("Here's a snowman for you: ☃. Good day!),
      :unexpected_end_of_input
    )

    assert_fail_with(~s("𝄞), :unexpected_end_of_input)
    assert_fail_with(~s(\u001F), {:unexpected_byte, "0x1F", 0})
    assert_fail_with(~s("\\ud8aa\\udcxx"), {:unexpected_sequence, "\\udcxx", 7})

    assert_fail_with(
      ~s("\\ud8aa\\uda00"),
      {:unexpected_sequence, "\\ud8aa\\uda00", 1}
    )

    assert_fail_with(~s("\\uxxxx"), {:unexpected_sequence, "\\uxxxx", 1})

    assert parse!(~s("\\"\\\\\\/\\b\\f\\n\\r\\t")) == ~s("\\/\b\f\n\r\t)
    assert parse!(~s("\\u2603")) == "☃"
    assert parse!(~s("\\u2028\\u2029")) == "\u2028\u2029"
    assert parse!(~s("\\uD834\\uDD1E")) == "𝄞"
    assert parse!(~s("\\uD834\\uDD1E")) == "𝄞"
    assert parse!(~s("\\uD799\\uD799")) == "힙힙"
    assert parse!(~s("✔︎")) == "✔︎"
  end

  test "objects" do
    assert_fail_with("{", :unexpected_end_of_input)
    assert_fail_with("{,", {:unexpected_byte, "0x2C", 1})
    assert_fail_with(~s({"foo"}), {:unexpected_byte, "0x7D", 6})
    assert_fail_with(~s({"foo": "bar",}), {:unexpected_byte, "0x7D", 14})

    assert parse!("{}") == %{}
    assert parse!(~s({"foo": "bar"})) == %{"foo" => "bar"}
    assert parse!(~s({"foo"  : "bar"})) == %{"foo" => "bar"}

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert parse!(~s({"foo": "bar", "baz": "quux"})) == expected

    expected = %{"foo" => %{"bar" => "baz"}}
    assert parse!(~s({"foo": {"bar": "baz"}})) == expected
  end

  test "copying strings on decode" do
    assert parse!("{}", %{strings: :copy}) == %{}
    as = :binary.copy("a", 101)
    bs = :binary.copy("b", 102)

    # Copy decode, copies the key
    assert [{key, value}] =
             :maps.to_list(parse!(~s({"#{as}": "#{bs}"}), %{strings: :copy, keys: :copy}))

    assert key == as
    assert value == bs
    assert :binary.referenced_byte_size(key) == byte_size(as)
    assert :binary.referenced_byte_size(value) == byte_size(bs)

    # Regular decode references the original string
    assert [{key, value}] = :maps.to_list(parse!(~s({"#{as}": "#{bs}"})))
    assert key == as
    assert value == bs
    assert :binary.referenced_byte_size(key) > byte_size(as) + byte_size(bs)
    assert :binary.referenced_byte_size(value) > byte_size(bs) + byte_size(bs)
  end

  test "arrays" do
    assert_fail_with("[", :unexpected_end_of_input)
    assert_fail_with("[,", {:unexpected_byte, "0x2C", 1})
    assert_fail_with("[1,]", {:unexpected_byte, "0x5D", 3})

    assert parse!("[]") == []
    assert parse!("[1, 2, 3]") == [1, 2, 3]
    assert parse!(~s(["foo", "bar", "baz"])) == ["foo", "bar", "baz"]
    assert parse!(~s([{"foo": "bar"}])) == [%{"foo" => "bar"}]
  end

  test "whitespace" do
    assert_fail_with("", :unexpected_end_of_input)
    assert_fail_with("    ", :unexpected_end_of_input)

    assert parse!("  [  ]  ") == []
    assert parse!("  {  }  ") == %{}

    assert parse!("  [  1  ,  2  ,  3  ]  ") == [1, 2, 3]

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert parse!(~s(  {  "foo"  :  "bar"  ,  "baz"  :  "quux"  }  )) == expected
  end

  test "iodata" do
    body = String.split(~s([1,2,3,4]), "")
    expected = [1, 2, 3, 4]
    assert parse!(body) == expected
  end

  defp parse!(json, opts \\ %{}) do
    {:ok, x} = :thoas.decode(json, opts)
    x
  end

  defp assert_fail_with(string, error) do
    assert {:error, error} == :thoas.decode(string, %{})
  end
end
