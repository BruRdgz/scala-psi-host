package core;

public final class Shout {
    private Shout() {}

    public static String loud(String text) {
        return text.toUpperCase() + "!";
    }
}
