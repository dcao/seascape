defmodule SeascapeWeb.ListingView do
  use SeascapeWeb, :view

  def title("index.html", assigns) do
    assigns["q"]
  end

  def gpa_to_letter(gpa) do
    cond do
      gpa > 3.85 -> "A"
      gpa > 3.5 -> "A-"
      gpa > 3.15 -> "B+"
      gpa > 2.85 -> "B"
      gpa > 2.5 -> "B-"
      gpa > 2.15 -> "C+"
      gpa > 1.85 -> "C"
      gpa > 1.5 -> "C-"
      gpa > 1.15 -> "D+"
      gpa > 0.85 -> "D"
      gpa > 0.5 -> "D-"
      true -> "F"
    end
  end
end
