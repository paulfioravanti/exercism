defmodule BankAccountTest do
  use ExUnit.Case

  setup do
    account = BankAccount.open_bank()
    {:ok, account: account}
  end

  test "initial balance is 0", %{account: account} do
    assert BankAccount.balance(account) == 0
  end

  test "incrementing and checking balance", %{account: account} do
    assert BankAccount.balance(account) == 0
    BankAccount.update(account, 10)
    assert BankAccount.balance(account) == 10
  end

  test "amount is added to balance", %{account: account} do
    assert BankAccount.balance(account) == 0
    BankAccount.update(account, 10)
    BankAccount.update(account, 10)
    assert BankAccount.balance(account) == 20
  end

  test "closing account rejects further inquiries", %{account: account} do
    assert BankAccount.balance(account) == 0
    BankAccount.close_bank(account)
    assert BankAccount.balance(account) == {:error, :account_closed}
    assert BankAccount.update(account, 10) == {:error, :account_closed}
  end

  test "incrementing balance from another process then checking it from test process", %{
    account: account
  } do
    assert BankAccount.balance(account) == 0
    this = self()

    spawn(fn ->
      BankAccount.update(account, 20)
      send(this, :continue)
    end)

    receive do
      :continue -> :ok
    after
      1000 -> flunk("Timeout")
    end

    assert BankAccount.balance(account) == 20
  end
end
