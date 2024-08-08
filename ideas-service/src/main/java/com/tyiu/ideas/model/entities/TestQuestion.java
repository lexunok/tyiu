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
public class TestQuestion {
    @Id
    private String id;
    private String testName;
    private Integer questionNumber;
    private String questionName;
    private Integer questionModuleNumber;
    private String questionModule;
    private String question;
}
