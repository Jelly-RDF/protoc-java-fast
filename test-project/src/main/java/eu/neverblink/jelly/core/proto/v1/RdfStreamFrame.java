// Code generated by NeverBlink protoc-java-fast. Do not edit!
package eu.neverblink.jelly.core.proto.v1;

import com.google.protobuf.ByteString;
import com.google.protobuf.CodedInputStream;
import com.google.protobuf.CodedOutputStream;
import com.google.protobuf.Descriptors;
import com.google.protobuf.InvalidProtocolBufferException;
import eu.neverblink.protoc.java.runtime.LimitedCodedInputStream;
import eu.neverblink.protoc.java.runtime.MessageFactory;
import eu.neverblink.protoc.java.runtime.ProtoMessage;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Protobuf type {@code RdfStreamFrame}
 * DO NOT INHERIT FROM THIS CLASS!
 * It's not <code>final</code> only to facilitate the Mutable nested subclass.
 */
@SuppressWarnings("hiding")
public abstract class RdfStreamFrame extends ProtoMessage<RdfStreamFrame> implements Cloneable {
  /**
   * <code>repeated .eu.ostrzyciel.jelly.core.proto.v1.RdfStreamRow rows = 1;</code>
   */
  protected final List<RdfStreamRow> rows = new ArrayList<>();

  /**
   * <code>repeated .eu.ostrzyciel.jelly.core.proto.v1.RdfStreamFrame.MetadataEntry metadata = 15;</code>
   */
  protected final List<MetadataEntry> metadata = new ArrayList<>();

  private RdfStreamFrame() {
  }

  /**
   * @return a new empty instance of {@code Mutable}
   */
  public static Mutable newInstance() {
    return new Mutable();
  }

  /**
   * <code>repeated .eu.ostrzyciel.jelly.core.proto.v1.RdfStreamRow rows = 1;</code>
   *
   * @return value for this field
   */
  public List<RdfStreamRow> getRows() {
    return rows;
  }

  /**
   * <code>repeated .eu.ostrzyciel.jelly.core.proto.v1.RdfStreamFrame.MetadataEntry metadata = 15;</code>
   *
   * @return value for this field
   */
  public List<MetadataEntry> getMetadata() {
    return metadata;
  }

  @Override
  public boolean equals(Object o) {
    if (o == this) {
      return true;
    }
    if (!(o instanceof RdfStreamFrame)) {
      return false;
    }
    RdfStreamFrame other = (RdfStreamFrame) o;
    return rows.equals(other.rows)
      && metadata.equals(other.metadata);
  }

  @Override
  public void writeTo(final CodedOutputStream output) throws IOException {
    if (!rows.isEmpty()) {
      for (final var _field : rows) {
        output.writeRawByte((byte) 10);
        output.writeUInt32NoTag(_field.getCachedSize());
        _field.writeTo(output);
      }
    }
    if (!metadata.isEmpty()) {
      for (final var _field : metadata) {
        output.writeRawByte((byte) 122);
        output.writeUInt32NoTag(_field.getCachedSize());
        _field.writeTo(output);
      }
    }
  }

  @Override
  protected int computeSerializedSize() {
    int size = 0;
    if (!rows.isEmpty()) {
      size += rows.size() + ProtoMessage.computeRepeatedMessageSizeNoTag(rows);
    }
    if (!metadata.isEmpty()) {
      size += metadata.size() + ProtoMessage.computeRepeatedMessageSizeNoTag(metadata);
    }
    return size;
  }

  @Override
  public Mutable clone() {
    return newInstance().copyFrom(this);
  }

  public static RdfStreamFrame parseFrom(final byte[] data) throws InvalidProtocolBufferException {
    return ProtoMessage.mergeFrom(newInstance(), data);
  }

  public static RdfStreamFrame parseFrom(final LimitedCodedInputStream input) throws IOException {
    return ProtoMessage.mergeFrom(newInstance(), input);
  }

  public static RdfStreamFrame parseDelimitedFrom(final InputStream input) throws IOException {
    return ProtoMessage.parseDelimitedFrom(input, RdfStreamFrame.getFactory());
  }

  /**
   * @return factory for creating RdfStreamFrame messages
   */
  public static MessageFactory<RdfStreamFrame> getFactory() {
    return RdfStreamFrameFactory.INSTANCE;
  }

  /**
   * @return this type's descriptor.
   */
  public static Descriptors.Descriptor getDescriptor() {
    return Rdf.eu_ostrzyciel_jelly_core_proto_v1_RdfStreamFrame_descriptor;
  }

  /**
   * Protobuf type {@code MetadataEntry}
   * DO NOT INHERIT FROM THIS CLASS!
   * It's not <code>final</code> only to facilitate the Mutable nested subclass.
   */
  public abstract static class MetadataEntry extends ProtoMessage<MetadataEntry> implements Cloneable {
    /**
     * <code>optional string key = 1;</code>
     */
    protected String key = "";

    /**
     * <code>optional bytes value = 2;</code>
     */
    protected ByteString value_ = ByteString.EMPTY;

    private MetadataEntry() {
    }

    /**
     * @return a new empty instance of {@code Mutable}
     */
    public static Mutable newInstance() {
      return new Mutable();
    }

    /**
     * <code>optional string key = 1;</code>
     * @return the key
     */
    public String getKey() {
      return key;
    }

    /**
     * <code>optional bytes value = 2;</code>
     *
     * @return value for this field
     */
    public ByteString getValue() {
      return value_;
    }

    @Override
    public boolean equals(Object o) {
      if (o == this) {
        return true;
      }
      if (!(o instanceof MetadataEntry)) {
        return false;
      }
      MetadataEntry other = (MetadataEntry) o;
      return key.equals(other.key)
        && value_.equals(other.value_);
    }

    @Override
    public void writeTo(final CodedOutputStream output) throws IOException {
      if (!key.isEmpty()) {
        output.writeRawByte((byte) 10);
        output.writeStringNoTag(key);
      }
      if (value_.size() > 0) {
        output.writeRawByte((byte) 18);
        output.writeBytesNoTag(value_);
      }
    }

    @Override
    protected int computeSerializedSize() {
      int size = 0;
      if (!key.isEmpty()) {
        size += 1 + CodedOutputStream.computeStringSizeNoTag(key);
      }
      if (value_.size() > 0) {
        size += 1 + CodedOutputStream.computeBytesSizeNoTag(value_);
      }
      return size;
    }

    @Override
    public Mutable clone() {
      return newInstance().copyFrom(this);
    }

    public static MetadataEntry parseFrom(final byte[] data) throws InvalidProtocolBufferException {
      return ProtoMessage.mergeFrom(newInstance(), data);
    }

    public static MetadataEntry parseFrom(final LimitedCodedInputStream input) throws IOException {
      return ProtoMessage.mergeFrom(newInstance(), input);
    }

    public static MetadataEntry parseDelimitedFrom(final InputStream input) throws IOException {
      return ProtoMessage.parseDelimitedFrom(input, MetadataEntry.getFactory());
    }

    /**
     * @return factory for creating MetadataEntry messages
     */
    public static MessageFactory<MetadataEntry> getFactory() {
      return MetadataEntryFactory.INSTANCE;
    }

    /**
     * @return this type's descriptor.
     */
    public static Descriptors.Descriptor getDescriptor() {
      return Rdf.eu_ostrzyciel_jelly_core_proto_v1_RdfStreamFrame_MetadataEntry_descriptor;
    }

    private enum MetadataEntryFactory implements MessageFactory<MetadataEntry> {
      INSTANCE;

      @Override
      public MetadataEntry create() {
        return MetadataEntry.newInstance();
      }
    }

    /**
     * Mutable subclass of the parent class.
     * You can call setters on this class to set the values.
     * When passing the constructed message to the serializer,
     * you should use the parent class (using .asImmutable()) to
     * ensure the message won't be modified by accident.
     */
    public static final class Mutable extends MetadataEntry {
      private Mutable() {
      }

      /**
       * <code>optional string key = 1;</code>
       * @param value the key to set
       * @return this
       */
      public Mutable setKey(final String value) {
        key = value;
        return this;
      }

      /**
       * <code>optional bytes value = 2;</code>
       * @param values the value_ to set
       * @return this
       */
      public Mutable setValue(final ByteString values) {
        value_ = values;
        return this;
      }

      @Override
      public Mutable copyFrom(final MetadataEntry other) {
        cachedSize = other.cachedSize;
        key = other.key;
        value_ = other.value_;
        return this;
      }

      @Override
      public Mutable mergeFrom(final MetadataEntry other) {
        cachedSize = -1;
        key = other.key;
        value_ = other.value_;
        return this;
      }

      @Override
      @SuppressWarnings("fallthrough")
      public Mutable mergeFrom(final LimitedCodedInputStream inputLimited) throws IOException {
        // Enabled Fall-Through Optimization
        final CodedInputStream input = inputLimited.in();
        int tag = input.readTag();
        while (true) {
          switch (tag) {
            case 10: {
              // key
              key = input.readStringRequireUtf8();
              tag = input.readTag();
              if (tag != 18) {
                break;
              }
            }
            case 18: {
              // value_
              value_ = input.readBytes();
              tag = input.readTag();
              if (tag != 0) {
                break;
              }
            }
            case 0: {
              return this;
            }
            default: {
              if (!input.skipField(tag)) {
                return this;
              }
              tag = input.readTag();
              break;
            }
          }
        }
      }

      /**
       * Returns this message as an immutable message, without any copies.
       */
      public MetadataEntry asImmutable() {
        return this;
      }
    }
  }

  private enum RdfStreamFrameFactory implements MessageFactory<RdfStreamFrame> {
    INSTANCE;

    @Override
    public RdfStreamFrame create() {
      return RdfStreamFrame.newInstance();
    }
  }

  /**
   * Mutable subclass of the parent class.
   * You can call setters on this class to set the values.
   * When passing the constructed message to the serializer,
   * you should use the parent class (using .asImmutable()) to
   * ensure the message won't be modified by accident.
   */
  public static final class Mutable extends RdfStreamFrame {
    private Mutable() {
    }

    /**
     * <code>repeated .eu.ostrzyciel.jelly.core.proto.v1.RdfStreamRow rows = 1;</code>
     * @param value the rows to add
     * @return this
     */
    public Mutable addRows(final RdfStreamRow value) {
      rows.add(value);
      return this;
    }

    /**
     * <code>repeated .eu.ostrzyciel.jelly.core.proto.v1.RdfStreamFrame.MetadataEntry metadata = 15;</code>
     * @param value the metadata to add
     * @return this
     */
    public Mutable addMetadata(final MetadataEntry value) {
      metadata.add(value);
      return this;
    }

    @Override
    public Mutable copyFrom(final RdfStreamFrame other) {
      cachedSize = other.cachedSize;
      rows.clear();
      rows.addAll(other.rows);
      metadata.clear();
      metadata.addAll(other.metadata);
      return this;
    }

    @Override
    public Mutable mergeFrom(final RdfStreamFrame other) {
      cachedSize = -1;
      getRows().addAll(other.rows);
      getMetadata().addAll(other.metadata);
      return this;
    }

    @Override
    @SuppressWarnings("fallthrough")
    public Mutable mergeFrom(final LimitedCodedInputStream inputLimited) throws IOException {
      // Enabled Fall-Through Optimization
      final CodedInputStream input = inputLimited.in();
      int tag = input.readTag();
      while (true) {
        switch (tag) {
          case 10: {
            // rows
            tag = ProtoMessage.readRepeatedMessage(rows, RdfStreamRow.getFactory(), inputLimited, tag);
            if (tag != 122) {
              break;
            }
          }
          case 122: {
            // metadata
            tag = ProtoMessage.readRepeatedMessage(metadata, MetadataEntry.getFactory(), inputLimited, tag);
            if (tag != 0) {
              break;
            }
          }
          case 0: {
            return this;
          }
          default: {
            if (!input.skipField(tag)) {
              return this;
            }
            tag = input.readTag();
            break;
          }
        }
      }
    }

    /**
     * Returns this message as an immutable message, without any copies.
     */
    public RdfStreamFrame asImmutable() {
      return this;
    }
  }
}
