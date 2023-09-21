package com.tyiu.corn.model.entities;

import lombok.*;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.annotation.Id;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Document
public class IdeaStack {
    @Id
    private String id;
    private String ideaId;
    private String skillId;
}
