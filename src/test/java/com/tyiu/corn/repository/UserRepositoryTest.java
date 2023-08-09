package com.tyiu.corn.repository;

import com.tyiu.corn.PostgresTest;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;

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
        String oldEmail = user.getEmail();
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
    }
}
