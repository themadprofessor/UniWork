import java.util.ArrayDeque;

public class Path {
    private ArrayDeque<Integer> path;
    int weight;

    public Path(Graph g, int start) {
        path = new ArrayDeque<>(g.size());
        path.push(start);
        weight = 0;
    }

    public void pushEdge(Node n) {
        path.push(n.getIndex());
        weight += n.getWeight();
    }

    public void popEdge(Node n) {
        path.pop();
        weight -= n.getWeight();
    }
}
