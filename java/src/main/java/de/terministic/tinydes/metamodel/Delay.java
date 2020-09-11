package de.terministic.tinydes.metamodel;

/**
 * Representation of a time delay for a {@code Transition}.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public interface Delay {
	
	/**
	 * Determines a time delay.
	 * 
	 * @return the delay.
	 */
	public Long getDelay();
	
}