package de.terministic.tinydes.start;

import java.io.IOException;

import de.terministic.tinydes.metamodel.Model;
import de.terministic.tinydes.modelexamples.Mm1Model;
import de.terministic.tinydes.modellibrary.FullReportGenerator;
import de.terministic.tinydes.simulatorcore.MainProgram;
import de.terministic.tinydes.simulatorcore.ReportGenerator;

/**
 * Starting the MM1 model, {@link Mm1Model}.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class StartMm1Model {

	public static void main(String[] args) throws IOException {
		Model model = new Mm1Model();
		Long endTime = 1000000L;
		ReportGenerator reportGenerator = new FullReportGenerator(System.out);

		MainProgram mainProgram = new MainProgram();
		mainProgram.runSimulation(model, endTime, reportGenerator);
	}
}