package com.tyiu.corn.model.entities;

import java.util.Date;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Notification {
    @Id
    private Long id;    
    private String message;
    private Date date;
    private int receiver;

}