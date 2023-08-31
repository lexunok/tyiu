package com.tyiu.corn.model.entities;

import java.util.Date;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Notification {
    @Id
    private String id;
    private String message;
    private Date date;
    private int receiver;

}