package com.tyiu.corn.model.requests;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ChangeRequest {
    private String newEmail;
    private String oldEmail;
    private String url;
    private Integer code;
    private String email;
    private String password;
    private String key;
}
