## How to run the application:

### In a Linux Terminal:

1. Open the shell environment
    ```
    $ nix-shell
    ```

2. Start the application
    ```
    $ cabal run
    ```


## How to interact with the application:

### In a Linux Terminal (or in Postman):

1. Create a Shop
    ```
    curl --location 'http://localhost:8081/api/shop/create' \
    --header 'Content-Type: application/json' \
    --data '{"shopNumber": "1432", "shopLocationCode": "ON", "shopName": "Shop"}'
    ```

2. Create a Product
    ```
    curl --location 'http://localhost:8081/api/product/create' \
    --header 'Content-Type: application/json' \
    --data '{"serialNumber": "GSFDEA1G", "shop": {"shopNumber": "1432", "shopLocationCode": "ON", "shopName": "Shop"}, "productName": "TEST PRODUCT NAME"}'
    ```

## How to view the database:

1. [Download pgadmin4](https://www.pgadmin.org/download/pgadmin-4-apt/)
2. Create a connection to the database using the details from the connection string in Database.hs
    - "host=localhost dbname=dbserialization user=rhemsuda password=password123 port=5432"
3. Select the table and right click to view
    - Databases -> dbserialization -> Schemas -> public -> Tables