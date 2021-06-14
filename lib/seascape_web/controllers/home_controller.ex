defmodule SeascapeWeb.HomeController do
  use SeascapeWeb, :controller
  alias Seascape.Repo

  def index(conn, _params) do
    course_cnt = Repo.aggregate(Seascape.Course, :count, :code)
    instr_cnt = Repo.aggregate(Seascape.Instructor, :count, :first)

    render(conn, "index.html", course_cnt: course_cnt, instr_cnt: instr_cnt)
  end
end
