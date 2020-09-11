package de.terministic.tinydes.simulatorcore;

import de.terministic.tinydes.metamodel.Delay;
import de.terministic.tinydes.metamodel.Event;
import de.terministic.tinydes.metamodel.Model;
import de.terministic.tinydes.metamodel.ModelState;
import de.terministic.tinydes.metamodel.StateChange;
import de.terministic.tinydes.metamodel.Transition;

/**
 * Manages the simulation of an event graph of type {@code Model}.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class MainProgram {

    private Model model;
    private ModelState modelState;
    private EventList eventList;
    private Clock clock;
    private ReportGenerator reportGenerator;

    /**
     * Executes the simulation with given parameters until given end time or
     * until no further events are scheduled.
     *
     * @param model
     *            to execute
     * @param endTime
     *            of the simulation
     * @param reportGenerator
     *            statistical counter evaluation
     */
    public void runSimulation(Model model, Long endTime, ReportGenerator reportGenerator) {
        this.model = model;

        // 0. Invoke the initialization routines.
        initializationRoutine(reportGenerator);

        while (this.clock.getCurrentTime() <= endTime && this.eventList.getSize() > 0) {

            // 1. Invoke the timing routine
            EventInstance currentEvent = this.timingRoutine();

            // 2. Invoke event routine
            eventRoutine(currentEvent);

        }
    }

    private void eventRoutine(EventInstance eventInstance) {

        // 1. Update system state.
        Event event = eventInstance.getEvent();
        for (StateChange stateChange : event.getStateChanges()) {
            stateChange.changeState(this.modelState);
        }

        // 2. update statistical counters.
        this.reportGenerator.update(eventInstance.getTime(), this.modelState);

        // 3. Generate future events and add to event list.
        for (Transition transition : event.getTransitions()) {

            if (transition.getCondition().isTrue(this.modelState)) {
                Event targetEvent = transition.getTargetEvent();
                Delay delay = transition.getDelay();

                this.eventList.addEvent(new EventInstance(this.clock.getCurrentTime() + delay.getDelay(), targetEvent));
            }

        }

    }

    private EventInstance timingRoutine() {
        // 1. Determine the next event type, say
        EventInstance result = this.eventList.removeNextEvent();

        // 2. Advance the simulation clock
        this.clock.setCurrentTime(result.getTime());
        return result;
    }

    private void initializationRoutine(ReportGenerator reportGenerator) {
        // 1. Set simulation clock = 0
        this.clock = new Clock(0L);

        // 2. Initialize system state and statistical counters
        this.modelState = new ModelState();
        this.reportGenerator = reportGenerator;

        // 3. Initialize event list
        this.eventList = new EventList();
        EventInstance initialEvent = new EventInstance(this.clock.getCurrentTime(), this.model.getStartEvent());
        this.eventList.addEvent(initialEvent);
    }
}
