package com.tyiu.corn.model.email.requests;

import lombok.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InvitationEmailRequest {
    private String title;
    private String to;
    private String from;
    private String message;
    private String link;

    public boolean checkMessageAndLinkNull(){
        return !((this.message == null) && (this.link == null));
    }
}
