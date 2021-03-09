defmodule Seascape.Repo.Migrations.CreateInstructors do
  use Ecto.Migration

  def change do
    create table(:instructors) do
      add :first, :string
      add :last, :string

      timestamps()
    end

    create unique_index(:instructors, [:first, :last])
  end
end
