const TWO_OR_MORE_DIGITS = /^\d{2,}$/;
const CHUNKS_OF_TWO = /.{1,2}/g;

export const valid = string => {
  string = string.replace(/\s/g, "");

  if (!string.match(TWO_OR_MORE_DIGITS)) {
    return false;
  }

  return (
    convertToReversedChunks(string)
    .map(toNumbers)
    .reduce(calculatePair, 0)
    % 10 === 0
  );
};

function convertToReversedChunks(string) {
  return [...string].reverse().join("").match(CHUNKS_OF_TWO);
}

function toNumbers(chunk) {
  return [...chunk].map(Number);
}

function calculatePair(acc, [first, second]) {
  if (!second) {
    return acc + first;
  }

  second = second * 2;
  second = second > 9 ? second - 9 : second;

  return acc + first + second;
}
