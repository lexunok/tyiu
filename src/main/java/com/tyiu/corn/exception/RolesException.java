package com.tyiu.corn.exception;

import org.springframework.http.converter.HttpMessageNotReadableException;

@Deprecated
public class RolesException extends HttpMessageNotReadableException{
    public RolesException(String message){
        super(message);
    }
}
