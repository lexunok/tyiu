package com.tyiu.ideas.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TestQuestionDTO {
    private String id;
    private String testName;
    private Integer questionNumber;
    private String questionName;
    private Integer questionModuleNumber;
    private String questionModule;
    private String question;
}
