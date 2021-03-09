defmodule Seascape.Repo.Migrations.CreateCourses do
  use Ecto.Migration

  def change do
    create table(:courses, primary_key: false) do
      add :code, :string, primary_key: true
      add :title, :string

      timestamps()
    end

  end
end
