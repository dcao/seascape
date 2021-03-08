defmodule Seascape.Repo.Migrations.CreateCapeEntries do
  use Ecto.Migration

  def change do
    create table(:cape_entries, primary_key: false) do
      add :instr, :string, primary_key: true
      add :course, :string, primary_key: true
      add :section, :string, primary_key: true
      add :term, :string, primary_key: true
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
