package de.terministic.tinydes.simulatorcore;

/**
 * Representation of the simulation clock. 
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class Clock {
	
	private Long time;
	
	/**
	 * Initializes the simulation clock with given start time.
	 * 
	 * @param time start time.
	 */
	public Clock(Long time) {
		this.time = time;
	}

	/**
	 * Returns the current time.
	 * 
	 * @return
	 */
	public Long getCurrentTime() {
		return time;
	}
	
	/**
	 * Setter for the simulation time.
	 * 
	 * @param currentTime simulation time
	 */
	public void setCurrentTime(Long currentTime) {
		this.time = currentTime;
	}
	
}