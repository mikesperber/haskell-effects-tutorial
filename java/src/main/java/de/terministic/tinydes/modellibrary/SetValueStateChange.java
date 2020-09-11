package de.terministic.tinydes.modellibrary;

import de.terministic.tinydes.metamodel.ModelState;
import de.terministic.tinydes.metamodel.StateChange;

/**
 * Operator used to set a state variable.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class SetValueStateChange implements StateChange {

    private final String name;
    private final Long value;

    /**
     * Initializes the state variable with the defined name and value.
     *
     * @param name
     * @param value
     */
    public SetValueStateChange(String name, Long value) {
        this.name = name;
        this.value = value;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * de.terministic.tinydes.metamodel.StateChange#changeState(de.terministic
     * .tinydes.metamodel.ModelState)
     */
    @Override
    public void changeState(ModelState modelState) {
        modelState.getStates().put(this.name, this.value);
    }
}