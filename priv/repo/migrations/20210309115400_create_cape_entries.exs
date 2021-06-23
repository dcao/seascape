defmodule Seascape.Repo.Migrations.CreateCapeEntries do
  use Ecto.Migration

  def change do
    create table(:cape_entries) do
      add :instr_id, references(:instructors)
      add :course_code, references(:courses, column: :code, type: :string)
      add :section, :string
      add :term, :string
      add :enrolled, :integer
      add :evals, :integer
      add :rec_class, :integer
      add :rec_instr, :integer

      add :grades_exp, {:array, :float}
      add :grades_rcv, {:array, :float}
      add :hours, {:array, :float}

      timestamps()
    end

    create unique_index(:cape_entries, [:instr_id, :course_code, :section, :term])
  end
end
