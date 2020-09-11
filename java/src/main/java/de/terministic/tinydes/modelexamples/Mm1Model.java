package de.terministic.tinydes.modelexamples;

import java.util.Random;

import de.terministic.tinydes.metamodel.Delay;
import de.terministic.tinydes.metamodel.Event;
import de.terministic.tinydes.metamodel.Model;
import de.terministic.tinydes.metamodel.StateChange;
import de.terministic.tinydes.metamodel.Transition;
import de.terministic.tinydes.modellibrary.ExponentialDelay;
import de.terministic.tinydes.modellibrary.IncrementValueStateChange;
import de.terministic.tinydes.modellibrary.LargerThanValueCondition;
import de.terministic.tinydes.modellibrary.SetValueStateChange;

/**
 * Simple MM1 model. Delays and arrival times are exponentially distributed.
 * <a href="http://sigmawiki.com/sigma/index.php?title=Image:Carwash1.png" target="_blank">Event graph of simple model.</a>
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class Mm1Model implements Model {

	private static final String QUEUE = "Queue";
	private static final String SERVER_CAPACITY = "ServerCapacity";
	private static final String COMPLETED = "Completed";
	private static final Delay SERVICE_TIME = new ExponentialDelay(8000.0, new Random());
	private static final Delay INTERARRIVAL_TIME = new ExponentialDelay(10000.0, new Random());

	private Event initialEvent;
	
	/**
	 * Creates the simple MM1 model.
	 */
	public Mm1Model() {
		this.buildModel();
	}
	
	/*
	 * (non-Javadoc)
	 * @see de.terministic.tinydes.metamodel.Model#getModelName()
	 */
	public String getModelName() {
		return "MM1Model";
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
		// Initialization of the parameter Queue, ServerCapacity, and Complete counter.
		StateChange runSC1 = new SetValueStateChange(SERVER_CAPACITY, 1L);
		StateChange runSC2 = new SetValueStateChange(QUEUE, 0L);
		StateChange runSC3 = new SetValueStateChange(COMPLETED, 0L);
		runEvent.addStateChange(runSC1);
		runEvent.addStateChange(runSC2);
		runEvent.addStateChange(runSC3);

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
		StateChange leaveSC2 = new IncrementValueStateChange("Completed");
		leaveEvent.addStateChange(leaveSC1);
		leaveEvent.addStateChange(leaveSC2);

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
