openssl genrsa -out taa.key 1024
openssl req -new -key taa.key -out taa.csr
openssl x509 -req -days 1095 -in taa.csr -signkey taa.key -out taa.crt
cat taa.key taa.crt > taa.pem
rm -f taa.key taa.crt taa.csr
