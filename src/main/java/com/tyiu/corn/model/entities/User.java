package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.Role;
import jakarta.persistence.*;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.*;

import java.util.List;

@Entity
@Setter
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue
    private Long id;
    @Column(unique = true)
    @NotEmpty(message = "Почта не может быть пустым")
    @Email(message = "Неверная форма")
    private String email;
    @NotEmpty(message = "Поле фамилии не может быть пустым")
    @Size(min = 2, max = 25, message = "Ошибка")
    private String lastName;
    @NotEmpty(message = "Поле имени не может быть пустым")
    @Size(min = 2, max = 25, message = "Ошибка")
    private String firstName;
    @Enumerated(EnumType.STRING)
    private List<Role> roles;
    @NotEmpty(message = "Введите пароль")
    @Size(min = 8, message = "Слишком маленький пароль")
    private String password;
}
