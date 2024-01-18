package com.tyiu.emailservice.model.responses;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ChangeDataEmailResponse {
    private String subject;
    private String text;
    private String to;
    private String code;
}
