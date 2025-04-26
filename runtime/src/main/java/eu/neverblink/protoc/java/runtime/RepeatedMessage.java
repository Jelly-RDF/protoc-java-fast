package eu.neverblink.protoc.java.runtime;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.UninitializedMessageException;

import java.util.List;

/**
 * @author Florian Enner
 * @since 09 Aug 2019
 */
public final class RepeatedMessage<MessageType
        extends ProtoMessage<MessageType>>
        extends RepeatedObject<RepeatedMessage<MessageType>, MessageType, MessageType, MessageType> {

    @SuppressWarnings("unchecked")
    public static <T extends ProtoMessage<T>> RepeatedMessage<T> newEmptyInstance(MessageFactory<T> factory) {
        return new RepeatedMessage(factory);
    }

    private RepeatedMessage(MessageFactory<MessageType> factory) {
        this.factory = factory;
    }

    @Override
    @SuppressWarnings("unchecked")
    protected final void setIndex0(int index, MessageType value) {
        array[index].copyFrom(value);
    }

    @Override
    protected MessageType getIndex0(int index) {
        return array[index];
    }

    @Override
    protected final void clearIndex0(int index) {
        array[index].clear();
    }

    @Override
    protected void copyFrom0(MessageType store, MessageType other) {
        store.copyFrom(other);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected final MessageType[] allocateArray0(int desiredSize) {
        return (MessageType[]) new ProtoMessage[desiredSize];
    }

    public final RepeatedMessage<MessageType> clearQuick() {
        for (int i = 0; i < length; i++) {
            array[i].clearQuick();
        }
        length = 0;
        return this;
    }

    /**
     * @return true if all contained messages are initialized
     */
    public final boolean isInitialized() {
        for (int i = 0; i < length; i++) {
            if (!array[i].isInitialized())
                return false;
        }
        return true;
    }

    /**
     * Helper method to check whether all contained messages are
     * initialized, i.e., if all required fields are set.
     * <p>
     * Message content is not automatically checked after merging
     * new data. This method should be called manually as needed.
     *
     * @return this
     * @throws InvalidProtocolBufferException if one or more messages are not initialized.
     */
    public RepeatedMessage<MessageType> checkInitialized() throws InvalidProtocolBufferException {
        if (!isInitialized()) {
            throw new UninitializedMessageException(List.of())
                    .asInvalidProtocolBufferException();
        }
        return this;
    }

    @Override
    protected MessageType createEmpty() {
        return factory.create();
    }

    final MessageFactory<MessageType> factory;

}
