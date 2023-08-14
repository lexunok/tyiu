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


import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
    void testGetListCompany(){
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
        //When
        when(companyRepository.findAll()).thenReturn(companies);
        List<Company> result = companyService.getListCompany();

        //Then
        assertEquals(companies.size(), result.size());
        verify(companyRepository).findAll();
    }

    @Test
    void testGetListStaff(){
        // Given
        Company company = Company.builder()
                .name("Company 1")
                .build();

        List<User> staff = new ArrayList<>();

        User user1 = User.builder()
                .firstName("Ivan")
                .lastName("Ivanov")
                .email("example@example.com")
                .roles(List.of(Role.PROJECT_OFFICE))
                .build();
        User user2 = User.builder()
                .firstName("Petr")
                .lastName("Petrov")
                .email("example@example.com")
                .roles(List.of(Role.PROJECT_OFFICE))
                .build();

        staff.add(user1);
        staff.add(user2);

        company.setStaff(staff);
        // When
        when(companyRepository.findById(company.getId())).thenReturn(Optional.of(company));
        List<UserDTO> result = companyService.getListStaff(company.getId());

        // Then
        assertEquals(staff.size(), result.size());
        assertEquals(staff.get(0).getEmail(), result.get(0).getEmail());
        assertEquals(staff.get(1).getEmail(), result.get(1).getEmail());
    }
    @Test
    void testAddCompany(){
        Company company = Company.builder()
                .name("Компания 1")
                .staff(new ArrayList<>())
                .build();

        when(companyRepository.save(company)).thenReturn(company);

        companyService.addCompany(company);
        verify(companyRepository).save(company);
    }

    @Test
    void testDeleteCompany(){
        Company company = Company.builder()
                .name("Компания 1")
                .staff(new ArrayList<>())
                .build();

        companyService.deleteCompany(company.getId());

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

        when(companyRepository.findById(company.getId())).thenReturn(Optional.of(company));
        when(companyRepository.save(company)).thenReturn(company);
        companyService.updateCompany(company.getId(), updatedCompany);


        assertEquals(updatedCompany.getName(), company.getName());
    }
}
