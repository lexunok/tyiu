package com.tyiu.corn.exception;


public class AuthorizationNotSuccessException extends RuntimeException{
    public AuthorizationNotSuccessException(String message){
        super(message);
    }
}
