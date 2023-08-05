package com.tyiu.corn.exception;

import org.springframework.mail.MailSendException;

public class EmailSendException extends MailSendException{
    public EmailSendException(String message){
        super(message);
    }
}
