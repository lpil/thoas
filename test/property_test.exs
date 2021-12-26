if Code.ensure_loaded?(ExUnitProperties) do
  defmodule Jason.PropertyTest do
    use ExUnit.Case, async: true
    use ExUnitProperties

    property "string rountrip" do
      check all(string <- string(:printable)) do
        assert decode(:jaserl.encode(string, [])) == string
      end
    end

    property "integer roundtrip" do
      check all(integer <- integer()) do
        assert decode(:jaserl.encode(integer, [])) == integer
      end
    end

    property "float roundtrip" do
      check all(float <- float()) do
        assert decode(:jaserl.encode(float, [])) == float
      end
    end

    property "string-keyed objects roundrtip" do
      check all(json <- json(string(:printable))) do
        assert decode(:jaserl.encode(json, [])) == json
      end
    end

    property "html escaping" do
      check all(string <- string(:printable)) do
        encoded = :jaserl.encode(string, escape: :html)
        refute encoded =~ <<0x2028::utf8>>
        refute encoded =~ <<0x2029::utf8>>
        refute encoded =~ ~r"(?<!\\)/"
        assert decode(encoded) == string
      end
    end

    property "javascript escaping" do
      check all(string <- string(:printable)) do
        encoded = :jaserl.encode(string, escape: :javascript)
        refute encoded =~ <<0x2028::utf8>>
        refute encoded =~ <<0x2029::utf8>>
        assert decode(encoded) == string
      end
    end

    defp decode(data, opts \\ []) do
      {:ok, x} = :jaserl.decode(data, opts)
      x
    end

    defp json(keys) do
      simple = one_of([integer(), float(), string(:printable), boolean(), nil])

      tree(simple, fn json ->
        one_of([list_of(json), map_of(keys, json)])
      end)
    end
  end
end
