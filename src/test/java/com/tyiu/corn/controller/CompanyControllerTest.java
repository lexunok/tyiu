package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Company;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.junit.jupiter.api.Assertions.*;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class CompanyControllerTest {
    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
    @BeforeAll
    public void setUp(){
        RegisterRequest request = new RegisterRequest(
                "fakekeemail","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        jwt = response.getToken();
    }

    @Test
    void testGetCompanyList(){
        Company company = Company.builder().name("company").build();
        Company response = webTestClient
                .post()
                .uri("/api/v1/company/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(company), Company.class)
                .exchange()
                .expectBody(Company.class)
                .returnResult().getResponseBody();
        List<Company> response3 = webTestClient
                .get()
                .uri("/api/v1/company")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Company.class)
                .returnResult().getResponseBody();
        assertNotNull(response3);
        List<Company> actualCompanys = response3.stream().filter(u -> company.getName().equals(response.getName())).toList();
        assertTrue(actualCompanys.size() >= 1);
    }

    @Test
    void testGetCompanyStaff(){
        Company company = Company.builder().name("company").staff(null).build();
        Company response = webTestClient
                .post()
                .uri("/api/v1/company/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(company), Company.class)
                .exchange()
                .expectBody(Company.class)
                .returnResult().getResponseBody();
        Long id = response.getId();
        List<Company> response3 = webTestClient
                .get()
                .uri("/api/v1/company/staff/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Company.class)
                .returnResult().getResponseBody();
        // assertNotNull(response3);
        // List<Company> actualCompanys = response3.stream().filter(u -> company.getStaff().equals(response.getStaff())).toList();
        // assertTrue(actualCompanys.size() >= 1);
    }
    

    @Test
    void testAddCompany(){
        Company company = Company.builder().name("company").build();
        Company response = webTestClient
                .post()
                .uri("/api/v1/company/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(company), Company.class)
                .exchange()
                .expectBody(Company.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(company.getName(),response.getName());
    }

    @Test
    void testDeleteCompany(){
        Company company = Company.builder().name("title").build();
        Company response = webTestClient
                .post()
                .uri("/api/v1/company/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(company), Company.class)
                .exchange()
                .expectBody(Company.class)
                .returnResult().getResponseBody();
        Long id = response.getId();
        webTestClient
                .delete()
                .uri("/api/v1/company/delete/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUpdateCompany(){
        Company company = Company.builder().name("title").build();
        Company response = webTestClient
                .post()
                .uri("/api/v1/company/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(company), Company.class)
                .exchange()
                .expectBody(Company.class)
                .returnResult().getResponseBody();
        Long id = response.getId();
        company = Company.builder().name("title2").build();
        Company response2 = webTestClient
                .put()
                .uri("/api/v1/company/update/{Id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(company), Company.class)
                .exchange()
                .expectBody(Company.class)
                .returnResult().getResponseBody();
        assertNull(response2);
    }
}
