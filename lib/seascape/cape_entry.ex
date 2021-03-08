defmodule Seascape.CapeEntry do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  schema "cape_entries" do
    field :instr, :string, primary_key: true
    field :course, :string, primary_key: true
    field :section, :string, primary_key: true
    field :term, :string, primary_key: true
    field :title, :string
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
    |> cast(attrs, [:instr, :course, :section, :term, :title, :enrolled, :evals, :rec_class, :rec_instr, :grade_exp, :grade_rcv, :hours])
    |> validate_required([:instr, :course, :section, :term, :enrolled, :evals, :rec_class, :rec_instr, :grade_exp, :grade_rcv, :hours])
  end
end
