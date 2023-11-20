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

    private CompanyDTO createCompany(String name, List<UserDTO> users, UserDTO owner) {
        CompanyDTO company = CompanyDTO.builder()
                .name(name)
                .users(users)
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

    private List<CompanyDTO> getCompanyOwnerMemberList(String jwt){
         List<CompanyDTO> companyList = webTestClient
                .get()
                .uri("api/v1/company/owner")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(companyList);
        return companyList;
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

    private void updateCompany(String id, String name, List<UserDTO> users, UserDTO owner){
        CompanyDTO updateCompany = CompanyDTO.builder()
                .id(id)
                .name(name)
                .users(users)
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
        createCompany("company",List.of(member),owner);
    }

    @Test
    void testUpdateCompany() {
        String id = createCompany("company",List.of(member),owner).getId();
        assertTrue(1 <= getCompany(id, "company").getUsers().size());

        updateCompany(id,"companyAddUser",List.of(owner, member),owner);
        assertTrue(2 <= getCompany(id, "company").getUsers().size());

        updateCompany(id,"companyDeleteUser",List.of(member),owner);
        assertTrue(1 <= getCompany(id, "company").getUsers().size());
    }

    @Test
    void testDeleteCompany() {
        webTestClient
                .delete()
                .uri("/api/v1/company/delete/{id}", createCompany("company",List.of(member),owner).getId())
                .header("Authorization", "Bearer " + jwt1)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testGetCompanyById() {
        getCompany(createCompany("company",List.of(member),owner).getId(), "company");
    }

    @Test
    void testGetMembersListCompany() {
        createCompany("company1",List.of(owner),owner);
        createCompany("company1",List.of(owner, member),owner);
        assertTrue(getCompanyOwnerMemberList(jwt1).size() > 1);
        assertTrue(1 <= getCompanyOwnerMemberList(jwt2).size());
    }

    @Test
    void testGetCompanyList() {
        createCompany("company",List.of(member),owner);
        createCompany("company",List.of(member),owner);
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
        List<CompanyDTO> members = webTestClient
                .get()
                .uri("/api/v1/company/staff/{companyId}", createCompany("company 1",List.of(owner, member),owner).getId())
                .header("Authorization", "Bearer " + jwt2)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(members);
        assertTrue(members.size() >= 2);
    }
}
