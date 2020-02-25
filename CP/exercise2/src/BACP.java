import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.util.tools.ArrayUtils;

import java.util.ArrayList;

public class BACP {

	Model model;
	Solver solver;

	IntVar minCreditsInAllPeriods; // as it says
	IntVar maxCreditsInAllPeriods; // as it says
	IntVar imbalance; //to be maxCreditsInAllPeriods - minCreditsInAllPeriods

	boolean verbose;
	String[] name; // name[i] is name of the ith course
	int[] period; // for saving off a solution ... see show() below
	private int nPeriods;
	private int nCourses;

    private IntVar[][] table;

	public BACP(int nCourses,int nPeriods,
				int minCredits,int maxCredits,
				int minCourses,int maxCourses,
				String[] name, int[] credits, ArrayList<Integer>[] prereq){

		this.name = name;
		this.nPeriods = nPeriods;
		this.nCourses = nCourses;

		model  = new Model();
		solver = model.getSolver();
		period = new int[nCourses];

		minCreditsInAllPeriods = model.intVar("minCredits",minCredits,maxCredits);
		maxCreditsInAllPeriods = model.intVar("maxCredits",minCredits,maxCredits);
		imbalance              = model.intVar("imbalance",0,maxCredits-minCredits);

        IntVar[] creditsInPeriods = model.intVarArray(nPeriods, minCredits, maxCredits);

		//
		// create necessary variables for your model
		//
		table = model.intVarMatrix("table", nPeriods, nCourses, 0, 1);

		//
		// post constraints
		//

		for (int i = 0; i < nCourses; i++) {
			// Ensure all courses appear exactly once
			model.sum(ArrayUtils.getColumn(table, i), "=", 1).post();
		}

		for (int i = 0; i < nPeriods; i++) {
			// Ensure all periods have between the min and max course count
			model.sum(table[i], ">=", minCourses).post();
			model.sum(table[i], "<=", maxCourses).post();

			// Ensure all periods have between the min and max credit count
			model.scalar(table[i], credits, ">=", minCredits).post();
			model.scalar(table[i], credits, "<=", maxCredits).post();

			// Collect the credit counts for periods
			model.scalar(table[i], credits, "=", creditsInPeriods[i]).post();
		}

        model.max(maxCreditsInAllPeriods, creditsInPeriods).post();
		model.min(minCreditsInAllPeriods, creditsInPeriods).post();
		model.arithm(imbalance, "=", maxCreditsInAllPeriods, "-", minCreditsInAllPeriods).post();

		// Ensure all prerequisites come before the courses which require them
        for (int i = 0; i < prereq.length; i++) {
            for (int before : prereq[i]) {
                model.lexLess(ArrayUtils.getColumn(table, i), ArrayUtils.getColumn(table, before)).post();
            }
        }
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

			save();
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

		save();
	}

	void show(){
		if (verbose)
			for (int i=0;i<name.length;i++)
				System.out.println(name[i] +" "+ period[i]);
		solver.printShortStatistics();
	}

	private void save() {
		for (int periodNum = 0; periodNum < nPeriods; periodNum++) {
			for (int course = 0; course < nCourses; course++) {
				if (table[periodNum][course].getValue() == 1) {
					period[course] = periodNum;
				}
			}
		}
	}
}	
	
