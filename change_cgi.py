import requests

BASE = "http://cmisim1"

auth_url = BASE + "/INCLUDE/devpagexUser.cgi?changeuserx2=010257ff128"
change_url = BASE + "/INCLUDE/change.cgi?changeadrx2={}&changetox2={}".format("0100014414D101", 40.0)

auth = requests.auth.HTTPBasicAuth("admin", "admin")

login = requests.get(auth_url, auth=auth)
changed = requests.get(change_url, auth=auth, cookies={"canremote1": login.text})
print(changed.text)
