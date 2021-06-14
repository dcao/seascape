defmodule Seascape.Repo.Migrations.CreatePrereqs do
  use Ecto.Migration

  def change do
    create table(:prereqs) do
      add :course_code, references(:courses, column: :code, type: :string)
      add :requires_code, references(:courses, column: :code, type: :string)
    end

    create unique_index(:prereqs, [:course_code, :requires_code])
  end
end
