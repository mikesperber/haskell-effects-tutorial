package de.terministic.tinydes.metamodel;

/**
 * Represents an event graph model.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public interface Model {

	/**
	 * Returns the name of the event graph model.
	 * 
	 * @return name of the model
	 */
	public String getModelName();
	
	/**
	 * Returns the start event.
	 * 
	 * @return initial event
	 */
	public Event getStartEvent();

}
