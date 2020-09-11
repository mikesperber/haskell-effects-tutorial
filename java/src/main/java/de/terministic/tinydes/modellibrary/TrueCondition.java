package de.terministic.tinydes.modellibrary;

import de.terministic.tinydes.metamodel.Condition;
import de.terministic.tinydes.metamodel.ModelState;

/**
 * Representation of a tautology.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public final class TrueCondition implements Condition {
	
	/**
	 * @see de.terministic.tinydes.metamodel.Condition#isTrue(de.terministic.tinydes.metamodel.ModelState)
	 * 
	 * @return true
	 */
	public boolean isTrue(ModelState modelState) {
		return true;
	}
}
