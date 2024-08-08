package com.tyiu.ideas.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class TestAnswer {
    @Id
    private String id;
    private String testName;
    private String userId;
    private String questionName;
    private Integer questionModuleNumber;
    private Integer questionNumber;
    private String answer;
}
