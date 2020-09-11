package de.terministic.tinydes.modellibrary;

import de.terministic.tinydes.metamodel.ModelState;
import de.terministic.tinydes.metamodel.StateChange;

/**
 * Operator used to change a state variable by increment.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class IncrementValueStateChange implements StateChange {

    private String name;
    private long increment;

    /**
     * Creates a {@code IncrementValueStateChange}. Default value for the
     * increment is 1.
     *
     * @param name of the state variable
     */
    public IncrementValueStateChange(String name) {
        this.name = name;
        this.increment = 1;
    }

    /**
     * Creates a {@code IncrementValueStateChange}.
     *
     * @param name of the state variable
     * @param increment value
     */
    public IncrementValueStateChange(String name, long increment) {
        this.name = name;
        this.increment = increment;
    }

    /*
     * (non-Javadoc)
     * @see de.terministic.tinydes.metamodel.StateChange#changeState(de.terministic.tinydes.metamodel.ModelState)
     */
    @Override
    public void changeState(ModelState modelState) {
        modelState.getStates().put(name, ((Long) modelState.getStates().get(name) + increment));
    }
}