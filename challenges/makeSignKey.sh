#!/bin/sh


pubdir=publish
privkey=lolchallenge.key
certname=lolchallenge.crt
pubkey=lolchallenge.pub
mkdir -p $pubdir
mkdir -p ~/.ssh
echo "Installing private key to ~/.ssh/$privkey"
openssl req -nodes -x509 -sha512 -newkey rsa:4096 -keyout ~/.ssh/$privkey -out $pubdir/$certname -days 365
openssl x509 -in $pubdir/$certname -pubkey -noout > $pubdir/$pubkey