package de.terministic.tinydes.modellibrary;

import java.util.Random;

import de.terministic.tinydes.metamodel.Delay;

/**
 * Models a exponentially distributed delay.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class ExponentialDelay implements Delay {

	private final double mean;
	private final Random random;

	/**
	 * Creates the {@code ExponentialDelay} with given mean for the
	 * exponential distribution.
	 * 
	 * @param mean
	 * @param random
	 */
	public ExponentialDelay(double mean, Random random) {
		this.mean = mean;
		this.random = random;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see de.terministic.tinydes.metamodel.Delay#getDelay()
	 */
	public Long getDelay() {
		Long result = 0L;
		double u = random.nextDouble();
		double x = -mean * Math.log(u);
		result = Math.round(x);
		return result;
	}

}
