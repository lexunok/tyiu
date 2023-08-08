package com.tyiu.corn.exception;


public class EmailSendException extends RuntimeException{
    public EmailSendException(String message){
        super(message);
    }
}
