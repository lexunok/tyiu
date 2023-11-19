package com.tyiu.corn.model.dto;

import java.time.LocalDateTime;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CommentDTO {
    private String id;
    private String text;
    private String senderEmail;
    private List<String> checkedBy;
    private LocalDateTime createdAt;
    private String ideaId;
}
