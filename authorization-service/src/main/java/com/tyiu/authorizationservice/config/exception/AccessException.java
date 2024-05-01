package com.tyiu.authorizationservice.config.exception;

public class AccessException extends RuntimeException{
    public AccessException(String message){
        super(message);
    }
}
