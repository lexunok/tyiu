package com.tyiu.authorizationservice.config.exception;

public class ExistException extends RuntimeException {
    public ExistException(String message){
        super(message);
    }
}
