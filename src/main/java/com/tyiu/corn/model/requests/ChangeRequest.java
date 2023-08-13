package com.tyiu.corn.model.requests;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ChangeRequest {
    private String newEmail;
    private String oldEmail;
    private String url;
    private int code;
    private String email;
    private String password;
    private String key;
}
