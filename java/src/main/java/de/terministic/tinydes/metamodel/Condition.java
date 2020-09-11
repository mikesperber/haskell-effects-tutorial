package de.terministic.tinydes.metamodel;

/**
 * Conditions are employed to decide if a {@code Transition} can be used
 * depending on the the current {@code ModelState}.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public interface Condition {
	
	/**
	 * Evaluates whether a condition is fulfilled or not.
	 * 
	 * @param modelState State of the model.
	 * @return true if a certain conditions is met, false otherwise. 
	 */
	public boolean isTrue(ModelState modelState);

}