package de.terministic.tinydes.metamodel;

/**
 * Models a state change on the {@code ModelState}.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public interface StateChange {

	/**
	 * Changes the model state.
	 * 
	 * @param modelState
	 */
	public void changeState(ModelState modelState);

}
