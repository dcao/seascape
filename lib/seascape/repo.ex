defmodule Seascape.Repo do
  use Ecto.Repo,
    otp_app: :seascape,
    adapter: Ecto.Adapters.Postgres
end
