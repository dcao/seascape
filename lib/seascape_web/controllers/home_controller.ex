defmodule SeascapeWeb.HomeController do
  use SeascapeWeb, :controller

  def index(conn, _params) do
    ln = 16000
    render(conn, "index.html", ln: ln)
  end
end
