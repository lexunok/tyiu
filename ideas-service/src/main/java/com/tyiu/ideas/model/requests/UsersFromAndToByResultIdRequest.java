package com.tyiu.ideas.model.requests;

import lombok.*;

@Data
@Builder
public class UsersFromAndToByResultIdRequest {
    private Integer resultId;
    private String toIdToUser;
    private String fromIdUser;
}
