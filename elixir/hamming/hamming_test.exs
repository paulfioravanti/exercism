if !System.get_env("EXERCISM_TEST_EXAMPLES") do
  Code.load_file("hamming.exs", __DIR__)
end

ExUnit.start()
ExUnit.configure(exclude: :pending, trace: true)

defmodule HammingTest do
  use ExUnit.Case

  test "no difference between empty strands" do
    assert Hamming.hamming_distance('', '') == {:ok, 0}
  end

  test "no difference between identical strands" do
    assert Hamming.hamming_distance('GGACTGA', 'GGACTGA') == {:ok, 0}
  end

  test "small hamming distance in middle somewhere" do
    assert Hamming.hamming_distance('GGACG', 'GGTCG') == {:ok, 1}
  end

  test "distance with same nucleotides in different locations" do
    assert Hamming.hamming_distance('TAG', 'GAT') == {:ok, 2}
  end

  test "larger distance" do
    assert Hamming.hamming_distance('ACCAGGG', 'ACTATGG') == {:ok, 2}
  end

  test "hamming distance is undefined for strands of different lengths" do
    assert {:error, "Lists must be the same length"} =
             Hamming.hamming_distance('AAAC', 'TAGGGGAGGCTAGCGGTAGGAC')

    assert {:error, "Lists must be the same length"} =
             Hamming.hamming_distance('GACTACGGACAGGACACC', 'GACATCGC')
  end
end
