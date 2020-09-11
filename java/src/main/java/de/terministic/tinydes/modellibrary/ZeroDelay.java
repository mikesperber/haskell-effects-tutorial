package de.terministic.tinydes.modellibrary;

import de.terministic.tinydes.metamodel.Delay;

/**
 * Representation used for immediate transitions with zero delay.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public final class ZeroDelay implements Delay {
	
	/**
	 * @see de.terministic.tinydes.metamodel.Delay#getDelay()
	 * 
	 * @return zero
	 */
	public Long getDelay() {
		return 0L;
	}

}
