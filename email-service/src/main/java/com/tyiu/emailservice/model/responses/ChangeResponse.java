package com.tyiu.emailservice.model.responses;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ChangeResponse {
    private String newEmail;
    private String oldEmail;
}