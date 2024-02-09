package com.tyiu.tgbotservice.model.entities;

import lombok.*;
import org.springframework.data.relational.core.mapping.Table;

@Data
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Table("users_telegram")
public class UserTelegram {

    private String userEmail;
    private String userTag;
    private Long chatId;
    private Boolean isVisible;
}
