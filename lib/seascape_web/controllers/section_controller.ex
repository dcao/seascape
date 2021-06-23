defmodule SeascapeWeb.SectionController do
  use SeascapeWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
