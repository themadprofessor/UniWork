import java.io.*;
import java.util.*;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.search.strategy.Search;

public class BACP {

	Model model;
	Solver solver;

	IntVar minCreditsInAllPeriods; // as it says
	IntVar maxCreditsInAllPeriods; // as it says
	IntVar imbalance; //to be maxCreditsInAllPeriods - minCreditsInAllPeriods

	boolean verbose;
	String[] name; // name[i] is name of the ith course
	int[] period; // for saving off a solution ... see show() below


	public BACP(int nCourses,int nPeriods,
				int minCredits,int maxCredits,
				int minCourses,int maxCourses,
				String[] name, int[] credits, ArrayList<Integer>[] prereq){

		this.name = name;

		model  = new Model();
		solver = model.getSolver();
		period = new int[nCourses];

		minCreditsInAllPeriods = model.intVar("minCredits",minCredits,maxCredits);
		maxCreditsInAllPeriods = model.intVar("maxCredits",minCredits,maxCredits);
		imbalance              = model.intVar("imbalance",0,maxCredits-minCredits);

		//
		// create necessary variables for your model
		//

		//
		// post constraints
		//


	}

	void optimize(){
		model.setObjective(Model.MINIMIZE,imbalance);
		//
		// optionally, set search strategy
		//
		while (solver.solve()){
			//
			// save off solution into
			// the period variables
			//
		}
	}

	void solve(){
		//
		// optionally, set search strategy
		//
		solver.solve();
		//
		// save off solution into
		// the period variables
		//
	}

	void show(){
		if (verbose)
			for (int i=0;i<name.length;i++)
				System.out.println(name[i] +" "+ period[i]);
		solver.printShortStatistics();
	}
}	
	
