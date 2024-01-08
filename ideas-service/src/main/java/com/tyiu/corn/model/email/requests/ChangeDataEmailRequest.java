package com.tyiu.corn.model.email.requests;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ChangeDataEmailRequest {
    private String subject;
    private String text;
    private String to;
    private String code;
}
