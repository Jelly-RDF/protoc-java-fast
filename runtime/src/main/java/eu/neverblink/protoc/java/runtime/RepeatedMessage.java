package eu.neverblink.protoc.java.runtime;

import com.google.protobuf.CodedOutputStream;

import java.util.List;

/**
 * @author Florian Enner
 * @since 09 Aug 2019
 */
public final class RepeatedMessage {
    public static <T extends ProtoMessage<T>> int computeRepeatedMessageSizeNoTag(final List<T> values) {
        int dataSize = 0;
        for (final ProtoMessage<?> value : values) {
            int valSize = value.getSerializedSize();
            dataSize += CodedOutputStream.computeUInt32SizeNoTag(valSize) + valSize;
        }
        return dataSize;
    }
}
