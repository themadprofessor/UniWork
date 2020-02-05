import java.util.*;
import java.io.*;

public class RandomInstance {

    public static void main(String[] args) throws IOException {
	String fname           = args[0];
	int n                  = Integer.parseInt(args[1]);
	int lwb                = Integer.parseInt(args[2]);
	int upb                = Integer.parseInt(args[3]);
	double pTogether       = Double.parseDouble(args[4]);
	double pApart          = Double.parseDouble(args[5]);  
	Scanner sc             = new Scanner(new File(fname));
	ArrayList<String> name = new ArrayList<String>();
	String[] sample        = new String[n];
	Random gen             = new Random();
	int[][] A              = new int[n][n];
	while (sc.hasNext()) name.add(sc.next());
	sc.close();
	for (int i=0;i<n;i++){
	    sample[i] = name.get(gen.nextInt(name.size()));
	    name.remove(sample[i]);
	}

	System.out.print("STUDENTS = {" + sample[0]);
	for (int i=1;i<n;i++) System.out.print(","+ sample[i]);
	System.out.println("};");
	System.out.println("T = "+ n/lwb +";");
	System.out.println("LWB = "+ lwb +";");
	System.out.println("UPB = "+ upb +";");
	for (int i=0;i<n-1;i++)
	    for (int j=i+1;j<n;j++)
		if (pApart >= gen.nextDouble()) A[i][j] = A[j][i] = -1;
	for (int i=0;i<n-1;i++)
	    for (int j=i+1;j<n;j++)
		if (pTogether >= gen.nextDouble()) A[i][j] = A[j][i] = 1;

	System.out.print("together = [");
	for (int i=0;i<n-1;i++)
	    for (int j=i+1;j<n;j++)
		if (A[i][j] == 1) System.out.print("|"+ sample[i] +","+ sample[j]);
	System.out.println("|];");

	System.out.print("apart = [");
	for (int i=0;i<n-1;i++)
	    for (int j=i+1;j<n;j++)
		if (A[i][j] == -1) System.out.print("|"+ sample[i] +","+ sample[j]);
	System.out.println("|];");
    }
}
