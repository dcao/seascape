defmodule Seascape.Repo.Migrations.CreateCapeEntries do
  use Ecto.Migration

  def change do
    create table(:cape_entries) do
      add :instr, :string
      add :course, :string
      add :term, :string
      add :enrolled, :integer
      add :evals, :integer
      add :rec_class, :float
      add :rec_instr, :float

      add :grade_exp_avg, :float
      add :grade_exp_std, :float
      add :grade_rcv_avg, :float
      add :grade_rcv_std, :float

      add :hours_avg, :float
      add :hours_stdev, :float

      timestamps()
    end

  end
end
