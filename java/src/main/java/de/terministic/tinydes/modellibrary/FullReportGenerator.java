package de.terministic.tinydes.modellibrary;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import de.terministic.tinydes.metamodel.ModelState;
import de.terministic.tinydes.simulatorcore.ReportGenerator;

/**
 * Monitors all state changes and writes the changes in given stream.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class FullReportGenerator implements ReportGenerator {

	private Map<String, Value> values;
	private boolean headerWritten;
	private OutputStream outputStream;

	/**
	 * Initiates the {@code FullReportGenerator}.
	 * 
	 * @throws IOException
	 */
	public FullReportGenerator(OutputStream outputStream) throws IOException {
		this.values = new HashMap<String, Value>();
		this.headerWritten = false;
		this.outputStream = outputStream;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * de.terministic.tinydes.simulatorcore.ReportGenerator#update(java.lang
	 * .Long, de.terministic.tinydes.metamodel.ModelState)
	 */
	@Override
	public void update(Long time, ModelState modelState) {
		for (String key : modelState.getStates().keySet()) {
			if (modelState.getStates().get(key) instanceof Long) {
				if (!this.values.containsKey(key)) {
					this.values.put(key, new Value());
				}
				this.values.get(key).setCurrent(time, (Long) modelState.getStates().get(key));
			}
		}

		this.writeReport();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see de.terministic.tinydes.simulatorcore.ReportGenerator#writeReport()
	 */
	@Override
	public void writeReport() {
		String toWrite = new String();
		if (!headerWritten) {
			toWrite = "name" + Value.SEPERATOR + Value.getHeader() + "\n";
			this.writeInStream(toWrite);
			headerWritten = true;
		}

		for (String key : this.values.keySet()) {
			toWrite = key + Value.SEPERATOR + this.values.get(key).toString() + "\n";
			this.writeInStream(toWrite);
		}
	}

	private void writeInStream(String toWrite) {
		try {
			this.outputStream.write(toWrite.getBytes());
			this.outputStream.flush();
		} catch (IOException ioException) {
			System.out.println("ERROR during writing report. Can not access Stream " + this.outputStream.toString());
		}
	}

	private static class Value {

		private final static String SEPERATOR = ";\t";

		private Long time;
		private Long value;
		private Long min;
		private Long max;
		private double average;

		public Value() {
			this.min = Long.MAX_VALUE;
			this.max = Long.MIN_VALUE;
			this.average = 0.0;
			this.time = 0L;
		}

		public void setCurrent(Long time, Long value) {

			this.setMax(value);
			this.setMin(value);
			this.updateAvg(time, value);

			this.time = time;
			this.value = value;
		}

		private void updateAvg(Long currentTime, Long currentValue) {
			this.average = (!currentTime.equals(0L) && this.value != null)
					? (this.average * this.time + (currentTime - this.time) * this.value) / currentTime : this.average;
		}

		private void setMin(Long current) {
			this.min = Math.min(current, this.min);
		}

		private void setMax(Long current) {
			this.max = Math.max(current, this.max);
		}

		public static String getHeader() {
			StringBuilder sb = new StringBuilder();
			sb.append("Time");
			sb.append(SEPERATOR);
			sb.append("Value");
			sb.append(SEPERATOR);
			sb.append("Min;");
			sb.append(SEPERATOR);
			sb.append("Max");
			sb.append(SEPERATOR);
			sb.append("Avg");
			sb.append(SEPERATOR);
			return sb.toString();
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			sb.append(this.time);
			sb.append(SEPERATOR);
			sb.append(this.value);
			sb.append(SEPERATOR);
			sb.append(this.min);
			sb.append(SEPERATOR);
			sb.append(this.max);
			sb.append(SEPERATOR);
			sb.append(this.average);
			sb.append(SEPERATOR);
			return sb.toString();
		}

	}
}
