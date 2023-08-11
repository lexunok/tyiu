package com.tyiu.corn.repository;

import com.tyiu.corn.PostgresTest;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.UserInfoRequest;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import java.util.List;
import java.util.NoSuchElementException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@EnableTransactionManagement
class UserRepositoryTest extends PostgresTest {
    @Autowired
    private UserRepository underTest;

    @Test
    void updateUserEmail(){
        //Before
        User user = User.builder()
        .email("wgwegw@gmail.com")
        .password("1234")
        .roles(List.of(Role.ADMIN))
        .firstName("Timur")
        .lastName("Minyazeff")
        .build();
        underTest.save(user);
        String oldEmail = "wgwegw@gmail.com";
        //Given
        String email = "gwhgwr@gmail.com";
        //When
        underTest.setEmail(email, user.getId());
        //Then
        assertDoesNotThrow(() ->{
            underTest.findByEmail(email).get();
        });
        assertThrows(NoSuchElementException.class, () ->{
            underTest.findByEmail(oldEmail).get();
        }, "No such element");
        assertEquals(underTest.findByEmail(email).get().getId(), underTest.findById(user.getId()).get().getId());
        assertEquals(underTest.findByEmail(email).get().getEmail(), underTest.findById(user.getId()).get().getEmail());
        assertEquals(email, underTest.findByEmail(email).get().getEmail());
        assertNotEquals(oldEmail, underTest.findByEmail(email).get().getEmail());    
    }
    @Test
    void updateUserPassword(){
        //Before
        User user = User.builder()
        .email("wgwegw@gmail.com")
        .password("1234")
        .roles(List.of(Role.ADMIN))
        .firstName("Timur")
        .lastName("Minyazeff")
        .build();
        underTest.save(user);
        String email = user.getEmail();
        String oldPassword = user.getPassword();
        //Given
        String newPassword = "12345";
        //When
        underTest.setPassword(newPassword, user.getId());
        //Then
        assertEquals(underTest.findByEmail(email).get().getPassword(), newPassword);
        assertNotEquals(oldPassword, underTest.findByEmail(email).get().getPassword());
    }
    @Test
    void updateUserFullInfo(){
        //Before
        User user = User.builder()
        .email("wgwegw@gmail.com")
        .password("1234")
        .roles(List.of(Role.ADMIN))
        .firstName("Timur")
        .lastName("Minyazeff")
        .build();
        underTest.save(user);
        //Given
        UserInfoRequest userInfoRequest = UserInfoRequest.builder()
        .email(user.getEmail())
        .newEmail("Email@gmail.com")
        .newLastName("Федоров")
        .newFirstName("Владислав")
        .newRoles(List.of(Role.PROJECT_OFFICE, Role.EXPERT))
        .build();
        //When
        underTest.setUserInfo(
            userInfoRequest.getNewEmail(), 
            userInfoRequest.getNewFirstName(),
            userInfoRequest.getNewLastName(),
            userInfoRequest.getNewRoles(),
            user.getId()
        );
        //Then
        assertDoesNotThrow(() -> underTest.findByEmail(userInfoRequest.getNewEmail()).get());
        assertThrows(NoSuchElementException.class, () ->{
            underTest.findByEmail(user.getEmail()).get();
        }, "No such element");
        assertEquals(userInfoRequest.getNewEmail(), underTest.findById(user.getId()).get().getEmail());
        assertNotEquals(user.getEmail(), underTest.findById(user.getId()).get().getEmail());

        assertEquals(userInfoRequest.getNewFirstName(), underTest.findById(user.getId()).get().getFirstName());
        assertNotEquals(user.getFirstName(), underTest.findById(user.getId()).get().getFirstName());

        assertEquals(userInfoRequest.getNewLastName(), underTest.findById(user.getId()).get().getLastName());
        assertNotEquals(user.getLastName(), underTest.findById(user.getId()).get().getLastName());

        assertEquals(userInfoRequest.getNewRoles(), underTest.findById(user.getId()).get().getRoles());
        assertNotEquals(user.getRoles(), underTest.findById(user.getId()).get().getRoles());
    }
}