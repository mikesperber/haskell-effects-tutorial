package de.terministic.tinydes.simulatorcore;

import de.terministic.tinydes.metamodel.Event;

/**
 * Tuple containing simulation time and event.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class EventInstance implements Comparable<EventInstance> {

	private final Event event;
	private final Long time;

	/**
	 * Creates an instance of tuple time and {@code Event}.
	 * 
	 * @param time
	 *            of the event
	 * @param event
	 */
	protected EventInstance(Long time, Event event) {
		this.event = event;
		this.time = time;
	}

	protected Event getEvent() {
		return event;
	}

	protected Long getTime() {
		return time;
	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 * 
	 *      An {@code EventInstance} is first sorted after
	 *      {@code EventInstance#getTime()} (simulation time - early time first)
	 *      and second sorted after {@code Event#getPriority()} (event priority
	 *      - smaller value first).
	 * 
	 */
	public int compareTo(EventInstance eventInstance) {
		int result = 0;
		if (this.time.compareTo(eventInstance.getTime()) == 0) {
			result = Integer.compare(this.getEvent().getPriority(), eventInstance.getEvent().getPriority());
		} else {
			result = this.time.compareTo(eventInstance.getTime());
		}
		return result;
	}

}
