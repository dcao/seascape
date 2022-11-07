defmodule SeascapeWeb.ListingController do
  use SeascapeWeb, :controller
  import Seascape.Listing

  def index(conn, params) do
    q = params["q"]
    sortBy = params["sortBy"]

    results = filter_sections(q)
    profs = filter_profs(q)

    render(conn, "index.html", q: q, sortBy: sortBy, results: results, profs: profs)
  end
end
