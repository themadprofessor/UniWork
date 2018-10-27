import java.util.ArrayList;
import java.util.HashMap;
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

    public ArrayList<Node> getAdjList() {
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
        return index == vertex.index &&
                Objects.equals(adjList, vertex.adjList);
    }

    @Override
    public int hashCode() {
        // It can be assumed that a Vertex at a given index will be the only instance of this class with the given index.
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
