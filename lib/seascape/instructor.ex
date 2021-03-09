defmodule Seascape.Instructor do
  use Ecto.Schema
  import Ecto.Changeset

  schema "instructors" do
    field :first, :string
    field :last, :string

    has_many :cape_entries, Seascape.CapeEntry

    timestamps()
  end

  @doc false
  def changeset(instructor, attrs) do
    instructor
    |> cast(attrs, [:first, :last])
    |> validate_required([:first, :last])
  end
end
