package com.tyiu.ideas.model.dto;

import com.tyiu.client.models.UserDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TestAnswerDTO {
    private String id;
    private String testName;
    private UserDTO user;
    private String questionName;
    private Integer questionModuleNumber;
    private Integer questionNumber;
    private String answer;
}
