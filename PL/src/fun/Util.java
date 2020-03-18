package fun;

public final class Util {
    private Util() {}

    public static String rangeToString(int[] range) {
        if (range.length == 0) {
            return "";
        } else if (range.length == 1) {
            return String.valueOf(range[0]);
        } else  {
            return range[0] + ".." + range[1];
        }
    }

    public static String overlapErrorString(int[] left, int[] right) {
        return "case " + rangeToString(left) + " and case " + rangeToString(right) + " overlap";
    }
}
