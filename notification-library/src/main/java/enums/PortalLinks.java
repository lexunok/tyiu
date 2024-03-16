package enums;

public enum PortalLinks {

    IDEA_INVITATION("team/all/"),

    IDEAS_LIST("ideas/list/"),
    IDEAS_MARKET("market/"),
    TEAM("team/"),
    TEAM_INVITES("team/invites/"),
    TEAM_REQUESTS("team/users/requests/");

    private final String val;

    PortalLinks(String val) {
        this.val = val;
    }

    @Override
    public String toString() {
        return val;
    }
}
