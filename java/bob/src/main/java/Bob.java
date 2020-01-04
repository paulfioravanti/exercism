class Bob {
    String hey(String input) {
        final String remark = input.trim();

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
        final boolean question = isQuestion(remark);
        final boolean yelling = isYelling(remark);

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
        return hasLetters(remark) && remark.toUpperCase().equals(remark);
    }

    private boolean hasLetters(String remark) {
        return remark.chars().anyMatch(Character::isLetter);
    }
}
