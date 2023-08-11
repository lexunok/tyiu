package com.tyiu.corn.exception;

public class DateExpiredException extends RuntimeException{
    public DateExpiredException(String message){
        super(message);
    }
}
