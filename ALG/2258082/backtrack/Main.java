import java.io.*;
import java.util.*;

/**
 program to find shortest path using the backtrack search algorithm
 */
public class Main {

	public static void main(String[] args) throws IOException {

		long start = System.currentTimeMillis();

		String inputFileName = args[0]; // input file name
  
		FileReader reader = new FileReader(inputFileName);
		Scanner in = new Scanner(reader);
		
		// read in the data here and create graph here

		Graph graph = new Graph(Integer.parseInt(in.nextLine()));
        for (int i = 0; i < graph.size(); i++) {
            String[] split = in.nextLine().split(" ");
            for (int j = 0; j < split.length; j++) {
                if (!split[j].equals("0")) {
                    graph.getVerts()[i].addToAdjList(j, Integer.parseInt(split[j]));
                }
            }
        }

        String[] split = in.nextLine().split(" ");
        int start_index = Integer.parseInt(split[0]);
        int end_index = Integer.parseInt(split[1]);

		reader.close();

		// do the work here
        Path p = new Path(graph, start_index);

		// end timer and print total time
		long end = System.currentTimeMillis();
		System.out.println("\nElapsed time: " + (end - start) + " milliseconds");
	}

}
