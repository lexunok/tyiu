package com.tyiu.ideas.model.enums;

public enum PortalLinks {
    IDEAS_LIST("ideas/list/"),
    IDEAS_MARKET("market/");

    private final String val;

    PortalLinks(String val) {
        this.val = val;
    }

    @Override
    public String toString() {
        return val;
    }
}
