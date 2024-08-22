package com.tyiu.ideas.model.responses;

import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.dto.TestResultDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TestAllResponse {
    private UserDTO user;
    private TestResultDTO belbinTest;
    private TestResultDTO temperTest;
    private TestResultDTO mindTest;
}
