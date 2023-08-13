package com.tyiu.corn.controller;

import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.CompanyService;
import com.tyiu.corn.model.entities.Company;
import com.tyiu.corn.model.entities.User;

import lombok.RequiredArgsConstructor;

import java.util.List;

@RestController
@RequestMapping("/api/v1/company")
@RequiredArgsConstructor
public class CompanyController {

    private final CompanyService companyService;

    @GetMapping()
    public List<Company> getCompanyList(){
        return companyService.getListCompany();
    }

    @GetMapping("/staff/{id}")
    public List<User> getCompanyStaff(@PathVariable Long id){
        return companyService.getListStaff(id);
    }

    @PostMapping("/add")
    public Company addCompany(@RequestBody Company company){
        return companyService.addCompany(company);
    }
    @DeleteMapping("/delete/{id}")
    public void deleteCompany(@PathVariable Long id){
        companyService.deleteCompany(id);
    }
    @PutMapping("/update/{id}")
    public void updateCompany(@PathVariable Long id, @RequestBody Company company){
        companyService.updateCompany(id, company);
    }
}
