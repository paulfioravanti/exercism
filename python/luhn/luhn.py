import re


class Luhn:
    _TWO_OR_MORE_DIGITS = re.compile(r"^\d{2,}$")

    def __init__(self, card_number):
        self.card_number = re.sub(r"\s", "", card_number)

    def valid(self):
        if not self._TWO_OR_MORE_DIGITS.match(self.card_number):
            return False

        numbers = self._reversed_numbers()
        pairs = Luhn._chunks(numbers, 2)
        sum_of_pairs = sum(map(Luhn._calculate_pair, pairs))
        return sum_of_pairs % 10 == 0

    def _reversed_numbers(self):
        return [int(char) for char in reversed(self.card_number)]

    @staticmethod
    def _chunks(numbers, chunk_size):
        for index in range(0, len(numbers), chunk_size):
            yield numbers[index:index + chunk_size]

    @staticmethod
    def _calculate_pair(pair):
        if len(pair) == 1:
            return pair[0]

        first, second = pair
        second = second * 2
        second = second - 9 if second > 9 else second

        return first + second
