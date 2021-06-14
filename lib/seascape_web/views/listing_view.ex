defmodule SeascapeWeb.ListingView do
  use SeascapeWeb, :view

  def title("index.html", assigns) do
    assigns["q"]
  end
end
