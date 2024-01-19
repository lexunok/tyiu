package com.tyiu.tgbotservice.model;

import lombok.Data;
import org.springframework.data.relational.core.mapping.Table;

@Data
@Table("users_telegram")
public class UserTelegram {

    private String userEmail;
    private String userTag;
    private Long chatId;
    private Boolean isVisible;
}
