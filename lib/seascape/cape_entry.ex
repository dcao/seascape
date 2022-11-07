defmodule Seascape.CapeEntry do
  use Ecto.Schema
  import Ecto.Changeset

  schema "cape_entries" do
    belongs_to :instr, Seascape.Instructor
    belongs_to :course, Seascape.Course, references: :code, foreign_key: :course_code, type: :string

    field :section, :string
    field :term, :string
    field :enrolled, :integer
    field :evals, :integer
    field :rec_class, :integer
    field :rec_instr, :integer

    field :grades_exp, {:array, :float}
    field :grades_rcv, {:array, :float}
    field :hours, {:array, :float}

    timestamps()
  end

  @doc false
  def changeset(cape_entry, attrs) do
    cape_entry
    |> cast(attrs, [:instr_id, :course_code, :section, :term, :enrolled, :evals, :rec_class, :rec_instr, :grade_exp, :grade_rcv, :hours])
    |> validate_required([:instr_id, :course_code, :section, :term, :enrolled, :evals, :rec_class, :rec_instr, :grade_exp, :grade_rcv, :hours])
    |> assoc_constraint(:instr)
    |> assoc_constraint(:course)
  end
end
