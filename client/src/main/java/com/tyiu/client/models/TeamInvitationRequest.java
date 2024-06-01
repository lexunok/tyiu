package com.tyiu.client.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TeamInvitationRequest {
    private String teamId;
    private String teamName;
    private String receiver;
    private String senderFirstName;
    private String senderLastName;
}
