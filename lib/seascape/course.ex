defmodule Seascape.Course do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:code, :string, []}
  @derive {Phoenix.Param, key: :code}
  schema "courses" do
    field :title, :string

    has_many :cape_entries, Seascape.CapeEntry
    many_to_many :prereqs,
      Seascape.Course,
      join_through: "prereqs",
      join_keys: [course_code: :code, requires_code: :code]

    timestamps()
  end

  @doc false
  def changeset(course, attrs) do
    course
    |> cast(attrs, [:code, :title])
    |> validate_required([:code, :title])
  end
end
