package de.terministic.tinydes.modellibrary;

import de.terministic.tinydes.metamodel.Delay;

/**
 * Models a constant delay.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class ConstantDelay implements Delay {

	private final Long value;

	/**
	 * Creates the {@code ConstantDelay} with the given constant value.
	 * 
	 * @param value 
	 */
	public ConstantDelay(Long value) {
		this.value = value;
	}
	
	/*
	 * (non-Javadoc)
	 * @see de.terministic.tinydes.metamodel.Delay#getDelay()
	 */
	public Long getDelay() {
		return this.value;
	}

}