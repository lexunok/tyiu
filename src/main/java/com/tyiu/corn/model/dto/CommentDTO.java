package com.tyiu.corn.model.dto;

import java.time.LocalDateTime;
import lombok.Getter;


@Getter
public class CommentDTO {
    private Long id;
    private String text;
    private String senderEmail;
    private LocalDateTime createdAt;
    private Long ideaId;
}
