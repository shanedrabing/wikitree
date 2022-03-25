rsconnect::setAccountInfo(
    name = "shanedrabing",
    token = "E146F010FB6CD871F4EBF2E3B115037B",
    secret = readLines("data/secret.txt")[1]
)

rsconnect::deployApp("wikitree")
