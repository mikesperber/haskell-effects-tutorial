package de.terministic.tinydes.modelexamples;

import de.terministic.tinydes.metamodel.Delay;
import de.terministic.tinydes.metamodel.Event;
import de.terministic.tinydes.metamodel.Model;
import de.terministic.tinydes.metamodel.StateChange;
import de.terministic.tinydes.metamodel.Transition;
import de.terministic.tinydes.modellibrary.ConstantDelay;
import de.terministic.tinydes.modellibrary.IncrementValueStateChange;
import de.terministic.tinydes.modellibrary.LargerThanValueCondition;
import de.terministic.tinydes.modellibrary.SetValueStateChange;

/**
 * Simple model. Delays and arrival times are constant.
 * <a href="http://sigmawiki.com/sigma/index.php?title=Image:Carwash1.png" target="_blank">Event graph of simple model.</a>
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class MinimalModel implements Model {

	private static final String QUEUE = "Queue";
	private static final String SERVER_CAPACITY = "ServerCapacity";
	private static final Delay SERVICE_TIME = new ConstantDelay(6L);
	private static final Delay INTERARRIVAL_TIME = new ConstantDelay(5L);

	private Event initialEvent;
	
	/**
	 * Creates the simple model.
	 */
	public MinimalModel() {
		this.buildModel();
	}
	
	/*
	 * (non-Javadoc)
	 * @see de.terministic.tinydes.metamodel.Model#getModelName()
	 */
	public String getModelName() {
		return "MinimalModel";
	}
	
	/*
	 * (non-Javadoc)
	 * @see de.terministic.tinydes.metamodel.Model#getStartEvent()
	 */
	public Event getStartEvent() {
		return this.initialEvent;
	}

	private void buildModel() {

		// Creating the RUN event.
		Event runEvent = new Event("RUN");
		// Initialization of the parameter Queue and ServerCapacity.
		StateChange runSC1 = new SetValueStateChange(SERVER_CAPACITY, 1L);
		StateChange runSC2 = new SetValueStateChange(QUEUE, 0L);
		runEvent.addStateChange(runSC1);
		runEvent.addStateChange(runSC2);

		// Creating the ENTER event.
		Event enterEvent = new Event("ENTER");
		// Increase the queue parameter.
		StateChange enterSC1 = new IncrementValueStateChange(QUEUE, 1L);
		enterEvent.addStateChange(enterSC1);

		// Creating the START event.
		Event startEvent = new Event("START");
		// Start event has to have the highest priority.
		startEvent.setPriority(-1);
		// Decrease the queue parameter and set the server capacity.
		StateChange startSC1 = new SetValueStateChange(SERVER_CAPACITY, 0L);
		StateChange startSC2 = new IncrementValueStateChange(QUEUE, -1L);
		startEvent.addStateChange(startSC1);
		startEvent.addStateChange(startSC2);

		// Creating the LEAVE event.
		Event leaveEvent = new Event("LEAVE");
		// Set the server capacity.
		StateChange leaveSC1 = new SetValueStateChange(SERVER_CAPACITY, 1L);
		leaveEvent.addStateChange(leaveSC1);

		// Edge between RUN and ENTER
		Transition runToEnter = new Transition(enterEvent);

		// Edge between ENTER and ENTER with delay (interarrival time)
		Transition enterToEnter = new Transition(enterEvent);
		enterToEnter.setDelay(INTERARRIVAL_TIME);

		// Edge between ENTER and START
		Transition enterToStart = new Transition(startEvent);
		enterToStart.setCondition(new LargerThanValueCondition(SERVER_CAPACITY, 0L));

		// Edge between START and LEAVE with delay (service)
		Transition startToLeave = new Transition(leaveEvent);
		startToLeave.setDelay(SERVICE_TIME);

		// Edge between LEAVE and START
		Transition leaveToStart = new Transition(startEvent);
		leaveToStart.setCondition(new LargerThanValueCondition(QUEUE, 0L));

		// Assign edges to the events.
		runEvent.addTransition(runToEnter);
		enterEvent.addTransition(enterToEnter);
		enterEvent.addTransition(enterToStart);
		startEvent.addTransition(startToLeave);
		leaveEvent.addTransition(leaveToStart);
		
		// Defining the initial event.
		this.initialEvent = runEvent;
	}
}
