package com.tyiu.ideas.model.requests;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class OauthLogin {
    private String login;
    private String password;
}
