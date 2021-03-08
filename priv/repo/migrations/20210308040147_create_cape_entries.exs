defmodule Seascape.Repo.Migrations.CreateCapeEntries do
  use Ecto.Migration

  def change do
    create table(:cape_entries, primary_key: false) do
      add :instr, :string, primary_key: true
      add :course, :string, primary_key: true
      add :section, :string, primary_key: true
      add :term, :string, primary_key: true
      add :title, :string
      add :enrolled, :integer
      add :evals, :integer
      add :rec_class, :integer
      add :rec_instr, :integer

      add :grades_exp, {:array, :float}
      add :grades_rcv, {:array, :float}
      add :hours, {:array, :float}

      timestamps()
    end

  end
end
