defmodule PerfectNumbersTest do
  use ExUnit.Case

  describe "Perfect numbers" do
    test "Smallest perfect number is classified correctly" do
      assert PerfectNumbers.classify(6) == {:ok, :perfect}
    end

    test "Medium perfect number is classified correctly" do
      assert PerfectNumbers.classify(28) == {:ok, :perfect}
    end

    test "Large perfect number is classified correctly" do
      assert PerfectNumbers.classify(33_550_336) == {:ok, :perfect}
    end
  end

  describe "Abundant numbers" do
    test "Smallest abundant number is classified correctly" do
      assert PerfectNumbers.classify(12) == {:ok, :abundant}
    end

    test "Medium abundant number is classified correctly" do
      assert PerfectNumbers.classify(30) == {:ok, :abundant}
    end

    test "Large abundant number is classified correctly" do
      assert PerfectNumbers.classify(33_550_335) == {:ok, :abundant}
    end
  end

  describe "Deficient numbers" do
    test "Smallest prime deficient number is classified correctly" do
      assert PerfectNumbers.classify(2) == {:ok, :deficient}
    end

    test "Smallest non-prime deficient number is classified correctly" do
      assert PerfectNumbers.classify(4) == {:ok, :deficient}
    end

    test "Medium deficient number is classified correctly" do
      assert PerfectNumbers.classify(32) == {:ok, :deficient}
    end

    test "Large deficient number is classified correctly" do
      assert PerfectNumbers.classify(33_550_337) == {:ok, :deficient}
    end

    test "Edge case (no factors other than itself) is classified correctly" do
      assert PerfectNumbers.classify(1) == {:ok, :deficient}
    end
  end

  describe "Invalid inputs" do
    test "Zero is rejected (not a natural number)" do
      assert PerfectNumbers.classify(0) ==
               {:error, "Classification is only possible for natural numbers."}
    end

    test "Negative integer is rejected (not a natural number)" do
      assert PerfectNumbers.classify(-1) ==
               {:error, "Classification is only possible for natural numbers."}
    end
  end
end
