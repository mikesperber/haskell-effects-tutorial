package de.terministic.tinydes.modellibrary;

import de.terministic.tinydes.metamodel.Condition;
import de.terministic.tinydes.metamodel.ModelState;

/**
 * Condition to test whether a certain state variable is larger than a given
 * reference.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class LargerThanValueCondition implements Condition {

    private String name;
    private Long value;

    /**
     * Initializes the {@code LargerThanValueCondition} with the given name of
     * the variable and the constant value to compare with.
     *
     * @param name
     *            of the state variable
     * @param value
     *            to compare with
     */
    public LargerThanValueCondition(String name, Long value) {
        this.name = name;
        this.value = value;
    }

    /*
     * (non-Javadoc)
     * @see de.terministic.tinydes.metamodel.Condition#isTrue(de.terministic.tinydes.metamodel.ModelState)
     */
    public boolean isTrue(ModelState modelState) {
        return (Long) modelState.getStates().get(this.name) > this.value;
    }
}