package com.tyiu.ideas.config.exception;

import lombok.*;

@Getter
@Setter
@AllArgsConstructor
public class CustomHttpException extends RuntimeException{
    private String message;
    private int statusCode;
}
