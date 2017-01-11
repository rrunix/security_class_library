import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class VigenereCracker {

	/**
	 * Encrypted word
	 */
	private String encrypt;
	/**
	 * Possible keys
	 */
	private String[] possibleKeys;
	/**
	 * Active (true) or desactive(false) the information on screen when the
	 * program is running.
	 * 
	 * Active = true : Shows divisors and repeated words
	 * 
	 * Desactive = false : Shows only possible keys
	 */
	private static boolean ACTIVE_DEBUG_STDIO = true;
	/**
	 * Max size of repeated words
	 */
	private static int MAX_REPEATED_WORD_SIZE = 10;
	/**
	 * Min size of repeated word
	 */
	private static int MIN_REPEATED_WORD_SIZE = 3;
	/**
	 * Character to replace the words when a repeated words had been found.
	 */
	private static final char REPLACE_CHARECTER = '#';

	/**
	 * Main, program's start point
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		// Text to decrypt
		String encrypt = "J OQWMZZJ,VANFTLOWMZZJ,V";
		// Possible keys
	MAX_REPEATED_WORD_SIZE = encrypt.length() -1;		

		String possibleKeys[] = new String[] { "ANILLO","MITESORO","MITESSORO","ELANILLOUNICO" };
		System.out.println("\nExecuting kasiski cracker...\n");
		// Create a new ViginiereCrack object
		VigenereCracker vc = new VigenereCracker(encrypt, possibleKeys);
		System.out.println("Findvalid keys with min word size = "
				+ MIN_REPEATED_WORD_SIZE + "\n");
		// find and print possible keys
		String[] findKeys = vc.getPossibleKeys();
		System.out.println(Arrays.toString(findKeys));
	}

	/**
	 * Create a new VigenereCracker
	 * 
	 * @param encrypt
	 *            ( word to decrypt)
	 * @param possibleKeys
	 *            ( possible keys )
	 */
	public VigenereCracker(String encrypt, String[] possibleKeys) {
		this.encrypt = encrypt;
		this.possibleKeys = possibleKeys;
	}

	/**
	 * Get the possible keys
	 * 
	 * @return
	 */
	public String[] getPossibleKeys() {
		DuplicatedWords words = findDuplicated(this.encrypt);
		if (ACTIVE_DEBUG_STDIO) {
			System.out.println("DUPLICATED WORDS\n");
			for (Duplicated d : words.duplicated) {
				System.out.print(d.word + " ");
				System.out.print(d.distance + " ");
				System.out.println(Arrays.toString(d.getDivisors()));
			}
			System.out.println();
		}
		List<String> possibleKeys = new ArrayList<String>();
		List<Integer> mcds = new ArrayList<Integer>();
		for (int z = 0; z < words.duplicated.size(); z++) {
			Integer[] values = words.duplicated.get(z).getDivisors();
			for (int j = 0; j < values.length; j++) {
				if (values[j] != 1) {
					if (!mcds.contains(values[j])) {
						mcds.add(values[j]);
					}
				}
			}
		}
		String[] resultAsString = new String[possibleKeys.size()];
		for (int i = 0; i < this.possibleKeys.length; i++) {
			int length = this.possibleKeys[i].length();
			for (int z = 0; z < mcds.size(); z++) {
				if (mcds.get(z) == length) {
					possibleKeys.add(this.possibleKeys[i]);
					break;
				}
			}
		}
		resultAsString = new String[possibleKeys.size()];
		possibleKeys.toArray(resultAsString);
		return resultAsString;
	}

	/**
	 * Find duplicated words
	 * 
	 * @param encrypt
	 * @return
	 */
	public DuplicatedWords findDuplicated(String encrypt) {
		String workingWord = new String(encrypt);
		DuplicatedWords words = new DuplicatedWords();
		for (int i = MAX_REPEATED_WORD_SIZE; i >= MIN_REPEATED_WORD_SIZE; i--) {
			for (int j = 0; j < workingWord.length() - i; j++) {
				if (workingWord.charAt(j) != REPLACE_CHARECTER) {
					String actual = workingWord.substring(j, j + i);
					if (!actual.contains(String.valueOf(REPLACE_CHARECTER))) {
						int indexOfRepetead = workingWord
								.indexOf(actual, j + i);
						if (indexOfRepetead != -1) {
							Duplicated d = new Duplicated();
							d.word = actual;
							d.distance = indexOfRepetead - j;
							words.addDuplicated(d);
							workingWord = workingWord.replaceAll(actual,
									generateString(actual.length()));
							j = 0;
						}
					}
				}
			}
		}
		return words;
	}

	/**
	 * Generate a new filler String
	 * 
	 * @param length
	 * @return
	 */
	public static String generateString(int length) {
		String generated = "";
		for (int i = 0; i < length; i++) {
			generated += REPLACE_CHARECTER;
		}
		return generated;
	}

	/**
	 * Class to manage duplicated words
	 * 
	 */
	class DuplicatedWords {
		/**
		 * List of duplicated words
		 */
		List<Duplicated> duplicated = new ArrayList<VigenereCracker.Duplicated>();

		/**
		 * Add a new duplicated word
		 * 
		 * @param d
		 *            ( word to add )
		 */
		public void addDuplicated(Duplicated d) {
			for (Duplicated actual : duplicated) {
				if (actual.word.equals(d.word) || actual.word.contains(d.word)) {
					return;
				}
			}
			duplicated.add(d);
		}
	}

	/**
	 * Class to wrap duplicated words
	 *
	 */
	class Duplicated {
		/**
		 * Duplicated word
		 */
		public String word;
		/**
		 * Duplicated word distance
		 */
		public int distance;

		/**
		 * Get the distance's divisors
		 * 
		 * @return
		 */
		public Integer[] getDivisors() {
			List<Integer> list = new ArrayList<Integer>();
			for (int i = 1; i <= distance; i++) {
				if (distance % i == 0) {
					list.add(i);
				}
			}
			Integer[] divisors = new Integer[list.size()];
			list.toArray(divisors);
			return divisors;
		}
	}
}
