package com.tyiu.corn.model.responses;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ChangeEmailResponse {
    private String email;
    private String oldEmail;
}