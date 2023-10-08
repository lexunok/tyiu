package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.Instant;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class TeamInvitation {
    @Id
    private String id;
    private String teamName;
    @Indexed
    private String teamId;
    private String receiverEmail;
    private Instant createdAt;
}
