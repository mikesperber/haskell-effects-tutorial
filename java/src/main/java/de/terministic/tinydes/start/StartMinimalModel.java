package de.terministic.tinydes.start;

import java.io.IOException;

import de.terministic.tinydes.metamodel.Model;
import de.terministic.tinydes.modelexamples.MinimalModel;
import de.terministic.tinydes.modellibrary.FullReportGenerator;
import de.terministic.tinydes.simulatorcore.MainProgram;
import de.terministic.tinydes.simulatorcore.ReportGenerator;

/**
 * Starting the simple model, {@link MinimalModel}.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class StartMinimalModel {

	public static void main(String[] args) throws IOException {
		Model model = new MinimalModel();
		Long endTime = 100L;
		ReportGenerator reportGenerator = new FullReportGenerator(System.out);

		MainProgram mainProgram = new MainProgram();
		mainProgram.runSimulation(model, endTime, reportGenerator);
	}
}