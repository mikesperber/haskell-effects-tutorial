package de.terministic.tinydes.simulatorcore;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * List containing all scheduled simulation events of type {@code EventInstance}.
 *
 * @author de.terministic group
 * @version 1.0
 */
public class EventList {

	private List<EventInstance> eventList;

	/**
	 * Creates the event list.
	 */
	public EventList() {
		this.eventList = new ArrayList<EventInstance>();
	}

	/**
	 * Adds an event to the event list.
	 * 
	 * @param eventinstance
	 */
	public void addEvent(EventInstance eventinstance) {
		this.eventList.add(eventinstance);
	}

	/**
	 * Returns and removes the next scheduled simulation event.
	 * 
	 * @return next event
	 */
	public EventInstance removeNextEvent() {
		Collections.sort(this.eventList);
		return this.eventList.remove(0);
	}
	
	/**
	 * @return event list size
	 */
	public int getSize() {
		return this.eventList.size();
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		Collections.sort(this.eventList);

		StringBuffer sb = new StringBuffer();
		for (EventInstance eventInstance : this.eventList) {
			sb.append("(" + eventInstance.getTime() + ")");
			sb.append(eventInstance.getEvent().getName() + ",");
		}

		return sb.toString().length() > 1 ? "{" + sb.toString().substring(0, sb.toString().length() - 1) + "}" : "{}";
	}

}
