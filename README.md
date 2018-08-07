# camilla

## Lesen

Um die CMI JSON API anzufragen, wird eine HTTP-Anfrage an `api.cgi` –
beispielsweise `/INCLUDE/api.cgi?jsonnode=1&jsonparam=I,O` – getätigt.
Die Syntax des zurückgegebenen JSON und der Parameter ist in [den
API-Docs](./doc/CMI_JSON_API_V3.pdf) beschrieben.

## Schreiben

Um in Ausgänge zu schreiben, benötigt man zunächst ein
Authentifikations-Token.  Dies erhält man als Antwort auf eine Anfrage
an `/INCLUDE/devpagexUser.cgi` mit dem Parameter `changeuserx2` auf
`010257ff<PASSWORD>` gesetzt, wobei `<PASSWORD>` das Expertenpasswort ist.

Das Schreiben geschieht mit einer Anfrage an `/INCLUDE/change.cgi` mit
den Parametern `changeadrx2` gesetzt auf die Adresse des Ausgangs und
`changetox2` gesetzt auf den Wert. Dieser Anfrage muss als Cookie unter dem
Namen `canremote1` das im vorigen Schritt erhaltene Experten-Token angefügt
werden.

### Adressen
* Output 5: 01000440DA1200
