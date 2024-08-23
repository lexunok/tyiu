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
public class TestResultDTO {
    private String id;
    private UserDTO user;
    private String testName;
    private String testResult;
}
