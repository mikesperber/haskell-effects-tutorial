package de.terministic.tinydes.metamodel;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Represents the model states as a collection of state variables.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class ModelState {
	
	/**
	 * The state variables.
	 */
	private Map<String, Object> states;
	
	/**
	 * Constructor.
	 */
	public ModelState() {
		this.states = new LinkedHashMap<String, Object>();
	}
	
	/**
	 * Returns the state variables.
	 * 
	 * @return
	 */
	public Map<String, Object> getStates() {
		return states;
	}
}
