defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  use Agent

  @initial_balance 0

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank do
    {:ok, account} = Agent.start(fn -> @initial_balance end)
    account
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    Agent.stop(account)
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    account
    |> confirm_account_availability()
    |> Agent.get(& &1)
  catch
    error ->
      {:error, error}
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    account
    |> confirm_account_availability()
    |> Agent.update(&(&1 + amount))
  catch
    error ->
      {:error, error}
  end

  defp confirm_account_availability(account) do
    if Process.alive?(account), do: account, else: throw(:account_closed)
  end
end
