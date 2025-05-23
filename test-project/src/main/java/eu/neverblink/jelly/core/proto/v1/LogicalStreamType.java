// Code generated by NeverBlink protoc-java-fast. Do not edit!
package eu.neverblink.jelly.core.proto.v1;

import eu.neverblink.protoc.java.runtime.ProtoEnum;

/**
 * Protobuf enum {@code LogicalStreamType}
 * DO NOT INHERIT FROM THIS CLASS!
 * It's not <code>final</code> only to facilitate the Mutable nested subclass.
 */
public enum LogicalStreamType implements ProtoEnum<LogicalStreamType> {
  /**
   * <code>UNSPECIFIED = 0;</code>
   */
  UNSPECIFIED("UNSPECIFIED", 0),

  /**
   * <code>FLAT_TRIPLES = 1;</code>
   */
  FLAT_TRIPLES("FLAT_TRIPLES", 1),

  /**
   * <code>FLAT_QUADS = 2;</code>
   */
  FLAT_QUADS("FLAT_QUADS", 2),

  /**
   * <code>GRAPHS = 3;</code>
   */
  GRAPHS("GRAPHS", 3),

  /**
   * <code>DATASETS = 4;</code>
   */
  DATASETS("DATASETS", 4),

  /**
   * <code>SUBJECT_GRAPHS = 13;</code>
   */
  SUBJECT_GRAPHS("SUBJECT_GRAPHS", 13),

  /**
   * <code>NAMED_GRAPHS = 14;</code>
   */
  NAMED_GRAPHS("NAMED_GRAPHS", 14),

  /**
   * <code>TIMESTAMPED_NAMED_GRAPHS = 114;</code>
   */
  TIMESTAMPED_NAMED_GRAPHS("TIMESTAMPED_NAMED_GRAPHS", 114);

  /**
   * <code>UNSPECIFIED = 0;</code>
   */
  public static final int UNSPECIFIED_VALUE = 0;

  /**
   * <code>FLAT_TRIPLES = 1;</code>
   */
  public static final int FLAT_TRIPLES_VALUE = 1;

  /**
   * <code>FLAT_QUADS = 2;</code>
   */
  public static final int FLAT_QUADS_VALUE = 2;

  /**
   * <code>GRAPHS = 3;</code>
   */
  public static final int GRAPHS_VALUE = 3;

  /**
   * <code>DATASETS = 4;</code>
   */
  public static final int DATASETS_VALUE = 4;

  /**
   * <code>SUBJECT_GRAPHS = 13;</code>
   */
  public static final int SUBJECT_GRAPHS_VALUE = 13;

  /**
   * <code>NAMED_GRAPHS = 14;</code>
   */
  public static final int NAMED_GRAPHS_VALUE = 14;

  /**
   * <code>TIMESTAMPED_NAMED_GRAPHS = 114;</code>
   */
  public static final int TIMESTAMPED_NAMED_GRAPHS_VALUE = 114;

  private final String name;

  private final int number;

  private LogicalStreamType(String name, int number) {
    this.name = name;
    this.number = number;
  }

  /**
   * @return the string representation of enum entry
   */
  @Override
  public String getName() {
    return name;
  }

  /**
   * @return the numeric wire value of this enum entry
   */
  @Override
  public int getNumber() {
    return number;
  }

  /**
   * @return a converter that maps between this enum's numeric and text representations
   */
  public static ProtoEnum.EnumConverter<LogicalStreamType> converter() {
    return LogicalStreamTypeConverter.INSTANCE;
  }

  /**
   * @param value The numeric wire value of the corresponding enum entry.
   * @return The enum associated with the given numeric wire value, or null if unknown.
   */
  public static LogicalStreamType forNumber(int value) {
    return LogicalStreamTypeConverter.INSTANCE.forNumber(value);
  }

  /**
   * @param number The numeric wire value of the corresponding enum entry.
   * @param other Fallback value in case the value is not known.
   * @return The enum associated with the given numeric wire value, or the fallback value if unknown.
   */
  public static LogicalStreamType forNumberOr(int number, LogicalStreamType other) {
    LogicalStreamType value = forNumber(number);
    return value == null ? other : value;
  }

  enum LogicalStreamTypeConverter implements ProtoEnum.EnumConverter<LogicalStreamType> {
    INSTANCE;

    @Override
    public final LogicalStreamType forNumber(final int value) {
      switch(value) {
        case 0: return UNSPECIFIED;
        case 1: return FLAT_TRIPLES;
        case 2: return FLAT_QUADS;
        case 3: return GRAPHS;
        case 4: return DATASETS;
        case 13: return SUBJECT_GRAPHS;
        case 14: return NAMED_GRAPHS;
        case 114: return TIMESTAMPED_NAMED_GRAPHS;
        default: return null;
      }
    }

    @Override
    public final LogicalStreamType forName(final CharSequence value) {
      switch (value.length()) {
        case 6: {
          if ("GRAPHS" == value) {
            return GRAPHS;
          }
          break;
        }
        case 8: {
          if ("DATASETS" == value) {
            return DATASETS;
          }
          break;
        }
        case 10: {
          if ("FLAT_QUADS" == value) {
            return FLAT_QUADS;
          }
          break;
        }
        case 11: {
          if ("UNSPECIFIED" == value) {
            return UNSPECIFIED;
          }
          break;
        }
        case 12: {
          if ("FLAT_TRIPLES" == value) {
            return FLAT_TRIPLES;
          }
          if ("NAMED_GRAPHS" == value) {
            return NAMED_GRAPHS;
          }
          break;
        }
        case 14: {
          if ("SUBJECT_GRAPHS" == value) {
            return SUBJECT_GRAPHS;
          }
          break;
        }
        case 24: {
          if ("TIMESTAMPED_NAMED_GRAPHS" == value) {
            return TIMESTAMPED_NAMED_GRAPHS;
          }
          break;
        }
      }
      return null;
    }
  }
}
