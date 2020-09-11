package de.terministic.tinydes.simulatorcore;

import de.terministic.tinydes.metamodel.ModelState;

/**
 * Monitors statistical variables during the simulation through evaluation of
 * {@code ModelState}.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public interface ReportGenerator {

	/**
	 * Updates statistical variables with the help of {@code ModelState}.
	 * 
	 * @param time
	 *            simulation time
	 * @param modelState
	 *            simulation model state
	 */
	public void update(Long time, ModelState modelState);
	
	/**
	 * Generates the simulation report.
	 */
	public void writeReport();
}
