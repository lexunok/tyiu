package com.tyiu.corn.model.entities;

import java.util.List;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;


@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Chat {
    @Id
    private String id;
    private List<String> messagesId;
    private List<String> membersId;
}
