package com.tyiu.corn.model.requests;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ChangeEmailRequest {
    private String newEmail;
    private String oldEmail;
    private String url;
    private int code;
}
