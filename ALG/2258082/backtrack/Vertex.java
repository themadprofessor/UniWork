import java.util.HashMap;
import java.util.Objects;

public class Vertex {
    private int index;
    private HashMap<Integer, Node> adjList;

    public Vertex(int index) {
        adjList = new HashMap<>();
        this.index = index;
    }

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public HashMap<Integer, Node> getAdjList() {
        return adjList;
    }

    public void addToAdjList(int index, int weight) {
        adjList.put(index, new Node(index, weight));
    }

    public int degree() {
        return adjList.size();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Vertex vertex = (Vertex) o;
        return index == vertex.index &&
                Objects.equals(adjList, vertex.adjList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(index, adjList);
    }

    @Override
    public String toString() {
        return "Vertex{" +
                "index=" + index +
                ", adjList=" + adjList +
                '}';
    }
}
