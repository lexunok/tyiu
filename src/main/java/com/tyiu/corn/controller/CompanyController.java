package com.tyiu.corn.controller;

import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.CompanyService;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Company;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@RequestMapping("/api/v1/company")
@RequiredArgsConstructor
public class CompanyController {

    private final CompanyService companyService;

    @GetMapping("/all")
    public Flux<Company> getCompanyList(){
        return companyService.getListCompany();
    }

    @GetMapping("/staff/{id}")
    public Flux<UserDTO> getCompanyStaff(@PathVariable String id){
        return companyService.getListStaff(id);
    }

    @PostMapping("/add")
    public Mono<Company> addCompany(@RequestBody Company company){
        return companyService.addCompany(company);
    }
    @DeleteMapping("/delete/{id}")
    public void deleteCompany(@PathVariable String id){
        companyService.deleteCompany(id);
    }
    @PutMapping("/update/{id}")
    public void updateCompany(@PathVariable String id, @RequestBody Company company){
        companyService.updateCompany(id, company);
    }
}
