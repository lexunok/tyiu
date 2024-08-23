package com.tyiu.ideas.model.responses;

import com.tyiu.client.models.UserDTO;
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
    private String belbinResult;
    private String temperResult;
    private String mindResult;
}
