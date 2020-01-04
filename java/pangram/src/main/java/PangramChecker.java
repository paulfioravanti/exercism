import java.util.HashSet;

public class PangramChecker {
    private static final int NUMBER_OF_LETTERS_IN_ALPHABET = 26;

    public boolean isPangram(String input) {
        if (input.isEmpty()) {
            return false;
        }

        HashSet<Character> letters = new HashSet<>();

        for (char letter : input.toCharArray()) {
            if (Character.isLetter(letter)) {
                letters.add(Character.toLowerCase(letter));
            }

            if (letters.size() == NUMBER_OF_LETTERS_IN_ALPHABET) {
                return true;
            }
        }

        return false;
    }
}
