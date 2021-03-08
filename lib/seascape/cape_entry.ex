defmodule Seascape.CapeEntry do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  schema "cape_entries" do
    field :instr, :string, primary_key: true
    field :course, :string, primary_key: true
    field :section, :string, primary_key: true
    field :term, :string, primary_key: true
    field :enrolled, :integer
    field :evals, :integer
    field :rec_class, :float
    field :rec_instr, :float

    field :grade_exp_avg, :float
    field :grade_exp_std, :float
    field :grade_rcv_avg, :float
    field :grade_rcv_std, :float

    field :hours_avg, :float
    field :hours_std, :float

    timestamps()
  end

  @doc false
  def changeset(cape_entry, attrs) do
    cape_entry
    |> cast(attrs, [:instr, :course, :section, :term, :enrolled, :evals, :rec_class, :rec_instr, :grade_exp_avg, :grade_exp_std, :grade_rcv_avg, :grade_rcv_std, :hours_avg, :hours_std])
    |> validate_required([:instr, :course, :section, :term, :enrolled, :evals, :rec_class, :rec_instr, :grade_exp_avg, :grade_rcv_avg, :hours_avg])
  end
end
