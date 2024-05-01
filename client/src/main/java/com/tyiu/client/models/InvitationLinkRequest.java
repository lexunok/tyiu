package com.tyiu.client.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InvitationLinkRequest {
    private String linkId;
    private String receiver;
    private String senderFirstName;
    private String senderLastName;
}
