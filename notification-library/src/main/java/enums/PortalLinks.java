package enums;

public enum PortalLinks {

    IDEAS_LIST("ideas/list/"),
    IDEAS_MARKET("market/"),
    TEAM("team/"),
    TEAM_LIST("team/all/"),
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
