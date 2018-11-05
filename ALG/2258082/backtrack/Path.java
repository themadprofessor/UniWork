import java.util.ArrayDeque;
import java.util.Objects;
import java.util.stream.Collectors;

public class Path {
    private ArrayDeque<Integer> path;
    int weight;

    public Path(Graph g, int start) {
        path = new ArrayDeque<>(g.size());
        path.push(start);
        weight = 0;
    }

    public static Path max_path() {
        return new Path(new ArrayDeque<>(), Integer.MAX_VALUE);
    }

    private Path(ArrayDeque<Integer> path, int weight) {
        this.path = path;
        this.weight = weight;
    }

    public void pushEdge(Node n) {
        path.push(n.getIndex());
        weight += n.getWeight();
    }

    public void popEdge(Node n) {
        path.pop();
        weight -= n.getWeight();
    }

    public int lastVertex() {
        return path.peek();
    }

    public String path() {
        StringBuilder builder = new StringBuilder();
        while (!path.isEmpty()) {
            builder.append(path.pollLast());
            builder.append(" ");
        }

        return builder.toString();
    }

    @Override
    protected Object clone() {
        return new Path(this.path.clone(), this.weight);
    }
}
