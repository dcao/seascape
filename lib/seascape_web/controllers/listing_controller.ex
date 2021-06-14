defmodule SeascapeWeb.ListingController do
  use SeascapeWeb, :controller

  def index(conn, params) do
    q = params["q"]
    sortBy = params["sortBy"]

    render(conn, "index.html", q: q, sortBy: sortBy)
  end
end
