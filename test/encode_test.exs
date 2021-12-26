defmodule Jason.EncoderTest do
  use ExUnit.Case, async: true

  test "atom" do
    assert to_json(nil) == "null"
    assert to_json(true) == "true"
    assert to_json(false) == "false"
    assert to_json(:poison) == ~s("poison")
  end

  test "integer" do
    assert to_json(42) == "42"
  end

  test "float" do
    assert to_json(99.99) == "99.99"
    assert to_json(9.9e100) == "9.9e100"
  end

  test "binaries" do
    assert to_json("hello world") == ~s("hello world")
    assert to_json("hello\nworld") == ~s("hello\\nworld")
    assert to_json("\nhello\nworld\n") == ~s("\\nhello\\nworld\\n")

    assert to_json("\"") == ~s("\\"")
    assert to_json("\0") == ~s("\\u0000")
    assert to_json(<<31>>) == ~s("\\u001F")
    assert to_json("☃a", escape: :unicode) == ~s("\\u2603a")
    assert to_json("𝄞b", escape: :unicode) == ~s("\\uD834\\uDD1Eb")
    assert to_json("\u2028\u2029abc", escape: :javascript) == ~s("\\u2028\\u2029abc")
    assert to_json("</script>", escape: :html) == ~s("<\\/script>")

    assert to_json(~s(<script>var s = "\u2028\u2029";</script>), escape: :html) ==
             ~s("<script>var s = \\\"\\u2028\\u2029\\\";<\\/script>")

    assert to_json("áéíóúàèìòùâêîôûãẽĩõũ") == ~s("áéíóúàèìòùâêîôûãẽĩõũ")
    assert to_json("a\u2028a", escape: :javascript) == ~s("a\\u2028a")
    assert to_json("a\u2028a", escape: :html) == ~s("a\\u2028a")
  end

  test "Map" do
    assert to_json(%{}) == "{}"
    assert to_json(%{"foo" => "bar"}) == ~s({"foo":"bar"})
    assert to_json(%{foo: :bar}) == ~s({"foo":"bar"})
    assert to_json(%{42 => :bar}) == ~s({"42":"bar"})
    assert to_json(%{'foo' => :bar}) == ~s({"foo":"bar"})

    multi_key_map = %{"foo" => "foo1", :foo => "foo2"}

    assert to_json(multi_key_map) == ~s({"foo":"foo2","foo":"foo1"})
  end

  test "list" do
    assert to_json([]) == "[]"
    assert to_json([1, 2, 3]) == "[1,2,3]"
  end

  test "EncodeError" do
    assert :jaserl.encode(<<0x80>>, []) == {:error, {:invalid_byte, "0x80", <<128>>}}

    assert :jaserl.encode(<<?a, 208>>, []) == {:error, {:invalid_byte, "0xD0", <<?a, 208>>}}
  end

  defp to_json(value, opts \\ []) do
    {:ok, json} = :jaserl.encode(value, opts)
    json
  end
end
