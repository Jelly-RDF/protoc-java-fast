package eu.neverblink.protoc.java.runtime;

import com.google.protobuf.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Abstract interface implemented by Protocol Message objects.
 * <p>
 * API partially copied from Google's MessageNano
 *
 * @author Florian Enner
 */
public abstract class ProtoMessage<MessageType extends ProtoMessage<?>> {
    
    protected int cachedSize = -1;

    // Keep the first bitfield in the parent class so that it
    // is likely in the same cache line as the object header
    protected int bitField0_;

    protected ProtoMessage() {
    }

    /**
     * Copies all fields and data from another message of the same
     * type into this message.
     *
     * @param other message with the contents to be copied
     * @return this
     */
    public abstract MessageType copyFrom(MessageType other);

    /**
     * Sets all fields and data to their default values. Does not
     * get rid of memory that was allocated.
     *
     * @return this
     */
    public abstract MessageType clear();

    /**
     * Clears all has state so that the message would serialize empty,
     * but does not set field default values and does not get rid of
     * memory that was allocated for repeated types.
     * <p>
     * Use this if you use this message for serialization purposes or
     * if you do not require default values for unset fields.
     *
     * @return this
     */
    public MessageType clearQuick() {
        return clear();
    }

    /**
     * @return true if none of the fields in this message are set
     */
    public boolean isEmpty() {
        throw new RuntimeException("Generated message does not implement isEmpty");
    }

    /**
     * @return true if all required fields in this message and in nested messages are set
     */
    public boolean isInitialized() {
        return true;
    }

    /**
     * Helper method to check if this message is initialized, i.e.,
     * if all required fields are set.
     * <p>
     * Message content is not automatically checked after merging
     * new data. This method should be called manually as needed.
     *
     * @return this
     * @throws InvalidProtocolBufferException if it is not initialized.
     */
    public final MessageType checkInitialized() throws InvalidProtocolBufferException {
        if (!isInitialized()) {
            throw new UninitializedMessageException(new ArrayList<>())
                .asInvalidProtocolBufferException();
        }
        return getThis();
    }

    /**
     * Get the number of bytes required to encode this message.
     * Returns the cached size or calls getSerializedSize which
     * sets the cached size. This is used internally when serializing
     * so the size is only computed once. If a member is modified
     * then this could be stale call getSerializedSize if in doubt.
     *
     * @return the cached size of the serialized proto form
     */
    public int getCachedSize() {
        if (cachedSize < 0) {
            // getSerializedSize sets cachedSize
            getSerializedSize();
        }
        return cachedSize;
    }

    /**
     * Computes the number of bytes required to encode this message.
     * The size is cached and the cached result can be retrieved
     * using getCachedSize().
     *
     * @return the size of the serialized proto form
     */
    public int getSerializedSize() {
        int size = computeSerializedSize();
        cachedSize = size;
        return size;
    }

    /**
     * Computes the number of bytes required to encode this message. This does
     * not update the cached size.
     *
     * @return the size of the serialized proto form
     */
    protected abstract int computeSerializedSize();

    /**
     * Serializes the message and writes it to {@code output}.
     *
     * @param output the output to receive the serialized form.
     * @throws IOException if an error occurred writing to {@code output}.
     */
    public abstract void writeTo(CodedOutputStream output) throws IOException;

    /**
     * Serializes the message and writes it to the {@code output} in
     * length delimited form.
     *
     * @return this
     */
    public MessageType writeDelimitedTo(CodedOutputStream output) throws IOException {
        // Force cached size to be recomputed
        // TODO: is this needed?
        output.writeUInt32NoTag(getSerializedSize());
        this.writeTo(output);
        return getThis();
    }

    /**
     * Merges the contents for one message written in length delimited form.
     *
     * @return this
     */
    public MessageType mergeDelimitedFrom(InputStream input) throws IOException {
        int size;
        try {
            int firstByte = input.read();
            if (firstByte == -1) {
                return null;
            }
            size = CodedInputStream.readRawVarint32(firstByte, input);
        } catch (IOException e) {
            throw new InvalidProtocolBufferException(e);
        }
        InputStream limitedInput = new LimitedInputStream(input, size);
        mergeFrom(CodedInputStream.newInstance(limitedInput));
        return getThis();
    }

    /**
     * Parse {@code input} as a message of this type and merge it with the
     * message being built.
     *
     * @return this
     */
    public abstract MessageType mergeFrom(CodedInputStream input) throws IOException;

    /**
     * Merge {@code other} into the message being built. {@code other} must have the exact same type
     * as {@code this}.
     *
     * <p>Merging occurs as follows. For each field:<br>
     * * For singular primitive fields, if the field is set in {@code other}, then {@code other}'s
     * value overwrites the value in this message.<br>
     * * For singular message fields, if the field is set in {@code other}, it is merged into the
     * corresponding sub-message of this message using the same merging rules.<br>
     * * For repeated fields, the elements in {@code other} are concatenated with the elements in
     * this message.<br>
     * * For oneof groups, if the other message has one of the fields set, the group of this message
     * is cleared and replaced by the field of the other message, so that the oneof constraint is
     * preserved.
     *
     * <p>This is equivalent to the {@code Message::MergeFrom} method in C++.
     *
     * @return this
     */
    public MessageType mergeFrom(MessageType other) {
        throw new RuntimeException("Generated message does not implement mergeFrom");
    }

    /**
     * Serialize to a byte array.
     *
     * @return byte array with the serialized data.
     */
    public final byte[] toByteArray() {
        return ProtoMessage.toByteArray(this);
    }

    /**
     * Serialize to a byte array.
     *
     * @return byte array with the serialized data.
     */
    public static byte[] toByteArray(ProtoMessage<?> msg) {
        final byte[] result = new byte[msg.getSerializedSize()];
        toByteArray(msg, result, 0, result.length);
        return result;
    }

    /**
     * Serialize to a byte array starting at offset through length. The
     * method getSerializedSize must have been called prior to calling
     * this method so the proper length is know.  If an attempt to
     * write more than length bytes OutOfSpaceException will be thrown
     * and if length bytes are not written then IllegalStateException
     * is thrown.
     */
    public static void toByteArray(ProtoMessage<?> msg, byte[] data, int offset, int length) {
        try {
            final CodedOutputStream output = CodedOutputStream.newInstance(data, offset, length);
            msg.writeTo(output);
            output.checkNoSpaceLeft();
        } catch (IOException e) {
            throw new RuntimeException("Serializing to a byte array threw an IOException "
                    + "(should never happen).", e);
        }
    }

    /**
     * Parse {@code data} as a message of this type and merge it with the message being built.
     */
    public static <T extends ProtoMessage<T>> T mergeFrom(T msg, final byte[] data) throws InvalidProtocolBufferException {
        return mergeFrom(msg, data, 0, data.length);
    }

    /**
     * Parse {@code data} as a message of this type and merge it with the message being built.
     */
    public static <T extends ProtoMessage<T>> T mergeFrom(T msg, final byte[] data, final int off, final int len)
            throws InvalidProtocolBufferException {
        try {
            return ProtoMessage.mergeFrom(msg, CodedInputStream.newInstance(data, off, len));
        } catch (InvalidProtocolBufferException e) {
            throw e;
        } catch (IOException e) {
            throw new RuntimeException("Reading from a byte array threw an IOException (should never happen).");
        }
    }

    /**
     * Parse {@code input} as a message of this type and merge it with the message being built.
     */
    public static <T extends ProtoMessage<T>> T mergeFrom(T msg, CodedInputStream input) throws IOException {
        msg.mergeFrom(input);
        input.checkLastTagWas(0);
        return msg;
    }

    /**
     * Indicates whether another object is "equal to" this one.
     * <p>
     * An object is considered equal when it is of the same message
     * type, contains the same fields (same has state), and all the
     * field contents are equal.
     * <p>
     * This comparison ignores unknown fields, so the serialized binary
     * form may not be equal.
     *
     * @param obj the reference object with which to compare
     * @return {@code true} if this object is the same as the obj
     * argument; {@code false} otherwise.
     */
    @Override
    public abstract boolean equals(Object obj);

    /**
     * Messages have no immutable state and should not
     * be used in hashing structures. This implementation
     * returns a constant value in order to satisfy the
     * contract.
     */
    @Override
    public final int hashCode() {
        return 0;
    }

    /**
     * Creates a new instance of this message with the same content
     */
    @Override
    public abstract MessageType clone();

    /**
     * @return the full path to all missing required fields in the message
     */
    public List<String> getMissingFields() {
        List<String> results = new ArrayList<String>();
        getMissingFields("", results);
        return results;
    }

    /**
     * Adds the full path to all missing required fields in the message
     */
    protected void getMissingFields(String prefix, List<String> results) {
    }

    protected static void getMissingFields(String prefix, String fieldName, ProtoMessage<?> field, List<String> results) {
        if (!field.isInitialized()) {
            field.getMissingFields(prefix + fieldName + ".", results);
        }
    }

    // TODO :(

    protected static void getMissingFields(String prefix, String fieldName, RepeatedMessage<?> field, List<String> results) {
        for (int i = 0; i < field.length; i++) {
            if (!field.array[i].isInitialized()) {
                field.array[i].getMissingFields(prefix + fieldName + "[" + i + "].", results);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private MessageType getThis() {
        return (MessageType) this;
    }
}
