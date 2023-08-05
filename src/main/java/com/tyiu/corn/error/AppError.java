package com.tyiu.corn.error;

public class AppError {
    private int statusCode;
    private String error;

    public int getStatusCode() {
        return this.statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public String getMessage() {
        return this.error;
    }

    public void setMessage(String message) {
        this.error = message;
    }

    public AppError() {
    }

    public AppError(int statusCode, String message) {
        this.statusCode = statusCode;
        this.error = message;
    }
}
