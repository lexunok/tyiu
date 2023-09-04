package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Company;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.repository.CompanyRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class CompanyServiceTest {
    @Mock
    private CompanyRepository companyRepository;

    @Mock
    private CompanyService companyService;


    @BeforeEach
    void setUp() {
        companyService = new CompanyService(companyRepository);
    }

    @Test
    void testGetListCompany() {
        // Given
        List<Company> companies = new ArrayList<>();
        Company company1 = Company.builder()
                .name("Company 1")
                .build();
        Company company2 = Company.builder()
                .name("Company 2")
                .build();
        companies.add(company1);
        companies.add(company2);

        when(companyRepository.findAll()).thenReturn(Flux.fromIterable(companies));

        // Act
        Flux<Company> result = companyService.getListCompany();

        // Assert
        assertEquals(companies, result.collectList().block());
        verify(companyRepository).findAll();
    }
    @Test
    void testGetListStaff(){
        // Given
        Company company = Company.builder()
                .name("Company 1")
                .build();

        UserDTO user1 = UserDTO.builder()
                .firstName("Ivan")
                .lastName("Ivanov")
                .email("example@example.com")
                .roles(List.of(Role.PROJECT_OFFICE))
                .build();
        UserDTO user2 = UserDTO.builder()
                .firstName("Petr")
                .lastName("Petrov")
                .email("example@example.com")
                .roles(List.of(Role.PROJECT_OFFICE))
                .build();

        company.setStaff(new ArrayList<>());
        company.getStaff().add(user1);
        company.getStaff().add(user2);

        Mono<Company> monoCompany = Mono.just(company);
        when(companyRepository.findById(company.getId())).thenReturn(monoCompany);

        // Act
        Flux<UserDTO> result = companyService.getListStaff(company.getId());

        // Assert
        assertEquals(2, result.collectList().block().size());
        verify(companyRepository).findById(company.getId());
    }
    @Test
    void testAddCompany(){
        Company company = Company.builder()
                .name("Компания 1")
                .staff(new ArrayList<>())
                .build();

        when(companyRepository.save(company)).thenReturn(Mono.just(company));

        Mono<Company> result = companyService.addCompany(company);

        assertEquals(company, result.block());
        verify(companyRepository).save(company);
    }

    @Test
    void testDeleteCompany(){
        Company company = Company.builder()
                .name("Компания 1")
                .staff(new ArrayList<>())
                .build();

        doNothing().when(companyRepository).deleteById(company.getId());

        verify(companyRepository).deleteById(company.getId());
    }

    @Test
    void testUpdateCompany(){
        Company updatedCompany = Company.builder()
                .name("Компания 1")
                .staff(new ArrayList<>())
                .build();

        Company company = Company.builder()
                .name("Компания 2")
                .build();

        Mono<Company> monoCompany = Mono.just(company);
        when(companyRepository.findById(company.getId())).thenReturn(monoCompany);
        when(companyRepository.save(any(Company.class))).thenReturn(Mono.just(updatedCompany));
        // Act
        companyService.updateCompany(company.getId(), updatedCompany);

        // Assert
        verify(companyRepository).findById(company.getId());
    }
}
