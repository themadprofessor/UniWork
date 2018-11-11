import java.util.ArrayList;
import java.util.Objects;

public class Vertex {
    private int index;
    private ArrayList<Node> adjList;

    public Vertex(int index) {
        adjList = new ArrayList<>();
        this.index = index;
    }

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public List<Node> getAdjList() {
        return adjList;
    }

    public void addToAdjList(int index, int weight) {
        adjList.add(new Node(index, weight));
    }

    public int degree() {
        return adjList.size();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Vertex vertex = (Vertex) o;
        return index == vertex.index;
    }

    @Override
    public int hashCode() {
        return index;
    }

    @Override
    public String toString() {
        return "Vertex{" +
                "index=" + index +
                ", adjList=" + adjList +
                '}';
    }
}
