package com.tyiu.corn.model.enums;

public enum Feasibility {
    POSSIBLE, //техническая реализуемость такого решения, возможно, существует
    POSSIBLY_ABSENT, //техническая реализуемость такого решения, возможно, не существует"
    DEFINITELY_ABSENT, //техническая реализуемость точно не существует"
    DEFINITELY_EXISTS // точно существует
}
