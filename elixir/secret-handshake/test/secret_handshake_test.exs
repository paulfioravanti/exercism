defmodule SecretHandshakeTest do
  use ExUnit.Case

  describe "Create a handshake for a number" do
    test "wink for 1" do
      assert SecretHandshake.commands(1) == ["wink"]
    end

    test "double blink for 10" do
      assert SecretHandshake.commands(2) == ["double blink"]
    end

    test "close your eyes for 100" do
      assert SecretHandshake.commands(4) == ["close your eyes"]
    end

    test "jump for 1000" do
      assert SecretHandshake.commands(8) == ["jump"]
    end

    test "combine two actions" do
      assert SecretHandshake.commands(3) == ["wink", "double blink"]
    end

    test "reverse two actions" do
      assert SecretHandshake.commands(19) == ["double blink", "wink"]
    end

    test "reversing one action gives the same action" do
      assert SecretHandshake.commands(24) == ["jump"]
    end

    test "reversing no actions still gives no actions" do
      assert SecretHandshake.commands(16) == []
    end

    test "all possible actions" do
      assert SecretHandshake.commands(15) == ["wink", "double blink", "close your eyes", "jump"]
    end

    test "reverse all possible actions" do
      assert SecretHandshake.commands(31) == ["jump", "close your eyes", "double blink", "wink"]
    end

    test "do nothing for zero" do
      assert SecretHandshake.commands(0) == []
    end

    test "do nothing if lower 5 bits not set" do
      assert SecretHandshake.commands(32) == []
    end
  end
end
