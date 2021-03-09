defmodule Seascape.Section do
  @moduledoc """
  The Section context, which aggregates all of the CAPE data for a given
  course & instructor pairing.
  """
  
  use TypedStruct

  import Ecto.Query, warn: false

  typedstruct do
    field :instr, String.t(), enforce: true
    field :course, String.t(), enforce: true
    field :title, String.t(), enforce: true
    field :enrolled, integer(), enforce: true
    field :evals, integer(), enforce: true
    field :rec_class, integer(), enforce: true
    field :rec_instr, integer(), enforce: true

    field :grades_exp, list(float()), enforce: true
    field :grades_rcv, list(float()), enforce: true
    field :hours, list(float()), enforce: true
  end
end
