package com.tyiu.corn.model.dto;

import java.time.Instant;
import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CommentDTO {
    private String id;
    private String comment;
    private String sender;
    private List<String> checkedBy;
    private Instant createdAt;
    private String ideaId;
}
