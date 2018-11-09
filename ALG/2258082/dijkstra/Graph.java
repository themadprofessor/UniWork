import java.util.Arrays;

public class Graph {
    private Vertex[] verts;

    public Graph(int num) {
        verts = new Vertex[num];
        for (int i = 0; i < num; i++) {
            verts[i] = new Vertex(i, num);
        }
    }

    public int size() {
        return verts.length;
    }

    public Vertex[] getVerts() {
        return verts;
    }

    @Override
    public String toString() {
        return "Graph{" +
                "verts=" + Arrays.toString(verts) +
                '}';
    }
}
