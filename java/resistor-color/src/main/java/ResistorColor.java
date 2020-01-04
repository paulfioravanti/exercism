import java.util.List;

class ResistorColor {
    private static final String[] COLORS = {
        "black",
        "brown",
        "red",
        "orange",
        "yellow",
        "green",
        "blue",
        "violet",
        "grey",
        "white"
    };

    int colorCode(String color) {
        return List.<String>of(COLORS).indexOf(color);
    }

    String[] colors() {
        return COLORS;
    }
}
