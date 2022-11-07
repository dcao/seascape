defmodule Seascape.Listing do
  @moduledoc """
  The Section context, which aggregates all of the CAPE data for a given
  course & instructor pairing.
  """

  defmodule Section do
    use TypedStruct

    typedstruct do
      field :instr, String.t(), enforce: true
      field :course, String.t(), enforce: true
      field :title, String.t(), enforce: true
      field :enrolled, integer(), enforce: true
      field :evals, integer(), enforce: true
      field :rec_class, integer(), enforce: true
      field :rec_instr, integer(), enforce: true

      field :grades_exp, list(float()), enforce: true
      field :grades_rcv, list(float())
      field :hours, list(float()), enforce: true
    end

    def aggregate({{first, last, course, title}, entries}) do
      enrolled = entries |> Enum.map(fn x -> x.enrolled end) |> Enum.sum()
      evals = entries |> Enum.map(fn x -> x.evals end) |> Enum.sum()

      rec_class = entries |> Enum.map(fn x -> x.rec_class end)
      rec_class = Enum.sum(rec_class) / length(rec_class)

      rec_instr = entries |> Enum.map(fn x -> x.rec_instr end) |> Enum.reject(&is_nil/1)
      rec_instr = if length(rec_instr) != 0 do
        Enum.sum(rec_instr) / evals
      else
        nil
      end

      # TODO: deal with array stuff properly
      grades_exp =
        entries |> Enum.map(fn x -> Enum.at(x.grades_rcv, 0) end) |> Enum.reject(&is_nil/1)

      grades_exp =
        if length(grades_exp) != 0 do
          (grades_exp |> Enum.sum()) / length(grades_exp)
        else
          nil
        end

      grades_rcv =
        entries |> Enum.map(fn x -> Enum.at(x.grades_rcv, 0) end) |> Enum.reject(&is_nil/1)

      grades_rcv =
        if length(grades_rcv) != 0 do
          (grades_rcv |> Enum.sum()) / length(grades_rcv)
        else
          nil
        end

      hours = entries |> Enum.map(fn x -> Enum.at(x.hours, 0) end) |> Enum.reject(&is_nil/1)

      hours =
        if length(hours) != 0 do
          (hours |> Enum.sum()) / length(hours)
        else
          0
        end

      %Section{
        instr: last <> ", " <> first,
        course: course,
        title: title,
        enrolled: enrolled,
        evals: evals,
        rec_class: rec_class,
        rec_instr: rec_instr,
        grades_exp: grades_exp,
        grades_rcv: grades_rcv,
        hours: hours
      }
    end
  end

  import Ecto.Query, warn: false
  alias Seascape.Repo, as: Repo

  def filter_sections(query) do
    # TODO: handle aggregation in query directly for performance?
    res =
      from(e in Seascape.CapeEntry,
        join: i in Seascape.Instructor,
        on: i.id == e.instr_id,
        join: c in Seascape.Course,
        on: e.course_code == c.code,
        preload: [instr: i, course: c],
        select: [
          :id,
          :section,
          :term,
          :enrolled,
          :evals,
          :rec_class,
          :rec_instr,
          :grades_exp,
          :grades_rcv,
          :hours,
          course: [:code, :title],
          instr: [:id, :first, :last]
        ],
        where:
          ilike(c.code, ^"%#{query}%") or ilike(i.first, ^"%#{query}%") or
            ilike(i.last, ^"%#{query}%")
      )
      |> Repo.all()

    grouped =
      Enum.group_by(res, fn x -> {x.instr.first, x.instr.last, x.course.code, x.course.title} end)

    grouped |> Enum.map(fn x -> Section.aggregate(x) end) |> Enum.group_by(fn x -> x.course end)
  end

  def filter_profs(query) do
    res =
      from(i in Seascape.Instructor,
        where: ilike(i.first, ^"%#{query}%") or ilike(i.last, ^"%#{query}%"))
      |> Repo.all()

    res
  end
end
