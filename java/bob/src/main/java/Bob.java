class Bob {
    private static final String LETTER_INPUT = ".*[A-Za-z].*";

    String hey(String input) {
        String remark = input.trim();

        if (isSilence(remark)) {
            return "Fine. Be that way!";
        } else {
            return respondToVerbalRemark(remark);
        }
    }

    private boolean isSilence(String remark) {
        return remark.isEmpty();
    }

    private String respondToVerbalRemark(String remark) {
        boolean question = isQuestion(remark);
        boolean yelling = isYelling(remark);

        if (question && yelling) {
            return "Calm down, I know what I'm doing!";
        } else if (question) {
            return "Sure.";
        } else if (yelling) {
            return "Whoa, chill out!";
        } else {
            return "Whatever.";
        }
    }

    private boolean isQuestion(String remark) {
        return remark.endsWith("?");
    }

    private boolean isYelling(String remark) {
        String yelling = remark.toUpperCase();
        return hasLetters(remark) && yelling.equals(remark);
    }

    private boolean hasLetters(String remark) {
        return remark.matches(LETTER_INPUT);
    }
}
