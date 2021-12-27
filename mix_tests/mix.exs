defmodule ThoasTests.Mixfile do
  use Mix.Project

  @version "1.1.2"

  def project() do
    [
      app: :thoas_tests,
      version: @version,
      elixir: "~> 1.5",
      start_permanent: Mix.env() == :prod,
      consolidate_protocols: Mix.env() != :test,
      deps: deps()
    ]
  end

  def application() do
    [extra_applications: []]
  end

  defp deps() do
    [
      {:thoas, path: "../"},
      {:stream_data, "~> 0.4", only: :test}
    ]
  end
end
