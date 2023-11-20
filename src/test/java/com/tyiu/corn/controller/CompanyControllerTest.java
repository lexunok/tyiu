package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CompanyDTO;
import com.tyiu.corn.model.dto.UserDTO;
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
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

import reactor.core.publisher.Mono;

import java.util.List;


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class CompanyControllerTest extends TestContainers{

    @Autowired
    private WebTestClient webTestClient;
    private UserDTO owner;
    private UserDTO member;
    private String jwt1;
    private String jwt2;

    private CompanyDTO createCompany() {
        CompanyDTO company = CompanyDTO.builder()
                .name("company")
                .users(List.of(owner))
                .owner(owner)
                .build();
        CompanyDTO responseCreateCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany);
        assertEquals(company.getName(), responseCreateCompany.getName());
        return responseCreateCompany;
    }

    private CompanyDTO getCompany(String id, String name){
        CompanyDTO responseGetCompany = webTestClient
                .get()
                .uri("/api/v1/company/{id}", id)
                .header("Authorization", "Bearer " + jwt1)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetCompany);
        assertEquals(name, responseGetCompany.getName());
        return responseGetCompany;
    }

    private AuthenticationResponse register(String email, String lastName, String firstName, String password){
        RegisterRequest request = new RegisterRequest(
                email, lastName, firstName, password,
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        return response;
    }

    @BeforeAll
    public void setUp() {

        AuthenticationResponse response1 = register("company.test@gmail.com", "company", "test", "company-test");
        jwt1 = response1.getToken();

        AuthenticationResponse response2 = register("test.company@gmail.com", "test", "company", "test-company");
        jwt2 = response2.getToken();

        owner = UserDTO.builder()
                .id(response1.getId())
                .email(response1.getEmail())
                .lastName(response1.getLastName())
                .firstName(response1.getFirstName())
                .roles(response1.getRoles())
                .build();

        member = UserDTO.builder()
                .id(response2.getId())
                .email(response2.getEmail())
                .lastName(response2.getLastName())
                .firstName(response2.getFirstName())
                .roles(response2.getRoles())
                .build();
    }

    @Test
    void testCreateCompany() {
        createCompany();
    }

    @Test
    void testUpdateCompany() {
        CompanyDTO companyDTO = createCompany();
        String id = companyDTO.getId();
        assertTrue(1 <= getCompany(id, companyDTO.getName()).getUsers().size());

        CompanyDTO updateCompany = CompanyDTO.builder()
                .id(id)
                .name("companyAddUser")
                .users(List.of(owner, member))
                .owner(owner)
                .build();

        CompanyDTO updatedCompany = webTestClient
                .put()
                .uri("/api/v1/company/update/{id}", id)
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(updateCompany), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(updatedCompany);
        assertEquals(updateCompany.getName(), updatedCompany.getName());
        assertTrue(2 <= getCompany(id, "company").getUsers().size());

        CompanyDTO updateAgainCompany = CompanyDTO.builder()
                .id(id)
                .name("companyAddUser")
                .users(List.of(member))
                .owner(owner)
                .build();

        CompanyDTO updatedAgainCompany = webTestClient
                .put()
                .uri("/api/v1/company/update/{id}", id)
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(updateAgainCompany), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(updatedAgainCompany);
        assertEquals(updateAgainCompany.getName(), updatedAgainCompany.getName());
        assertTrue(1 <= getCompany(id, "company").getUsers().size());
    }

    @Test
    void testDeleteCompany() {
        webTestClient
                .delete()
                .uri("/api/v1/company/delete/{id}", createCompany().getId())
                .header("Authorization", "Bearer " + jwt1)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testGetCompanyById() {
        getCompany(createCompany().getId(), "company");
    }

    @Test
    void testGetMembersListCompany() {

        CompanyDTO company1 = CompanyDTO.builder()
                .name("company1")
                .users(List.of(owner))
                .owner(owner)
                .build();

        CompanyDTO responseCreateCompany1 = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company1), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany1);
        assertEquals(company1.getName(), responseCreateCompany1.getName());

        CompanyDTO company2 = CompanyDTO.builder()
                .name("company1")
                .users(List.of(owner, member))
                .owner(owner)
                .build();

        CompanyDTO responseCreateCompany2 = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company2), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany2);
        assertEquals(company2.getName(), responseCreateCompany2.getName());

        List<CompanyDTO> getOwnerListCompany = webTestClient
                .get()
                .uri("api/v1/company/owner")
                .header("Authorization", "Bearer " + jwt1)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(getOwnerListCompany);
        assertTrue(getOwnerListCompany.size() > 1);

        List<CompanyDTO> getMemberListCompany = webTestClient
                .get()
                .uri("api/v1/company/owner")
                .header("Authorization", "Bearer " + jwt2)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(getMemberListCompany);
        assertEquals(1, getMemberListCompany.size());
    }

    @Test
    void testGetCompanyList() {
        createCompany();
        createCompany();
        List<CompanyDTO> listCompany = webTestClient
                .get()
                .uri("api/v1/company/all")
                .header("Authorization", "Bearer " + jwt2)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(listCompany);
        assertTrue(listCompany.size() > 1);
    }

    @Test
    void testGetCompanyStaff() {

        CompanyDTO company = CompanyDTO.builder()
                .name("company 1")
                .users(List.of(owner, member))
                .owner(owner)
                .build();

        webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange();

        webTestClient
                .get()
                .uri("/api/v1/company/all/{userId}", member.getId())
                .header("Authorization", "Bearer " + jwt2)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult();
    }
}
