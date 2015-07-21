openssl genrsa -out taa.key 1024
openssl req -new -key taa.key -out taa.csr
openssl x509 -req -days 365 -in taa.csr -signkey taa.key -out taa.crt
cat taa.key taa.crt > taa.pem
