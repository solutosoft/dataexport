CREATE TABLE invoices (
  
  id integer NOT NULL,
  
  person_id integer NOT NULL,
  
  created_at date NOT NULL,
  
  number varchar(30) DEFAULT NULL,
  
  description text,
  
PRIMARY KEY (id)
);



INSERT INTO invoices (id, person_id, created_at, number, description) VALUES

(1, 1, '2015-11-10', '001', 'The first order'),

(2, 2, '2015-11-10', '002', 'Second order');



CREATE TABLE  invoice_details (
  
  id integer NOT NULL,
  
  invoice_id integer NOT NULL,
  
  product_id integer NOT NULL,
  
  quantity integer NOT NULL,
  
  price decimal(18,2) NOT NULL,
  
  total decimal(18,2) NOT NULL,
  
PRIMARY KEY (id)
);



INSERT INTO invoice_details (id, invoice_id, product_id, quantity, price, total) VALUES

(1, 1, 1, 2, '10.00', '20.00'),

(2, 1, 2, 5, '20.00', '100.00'),

(3, 2, 2, 1, '10.00', '10.00');




CREATE TABLE  people (

  id integer NOT NULL,
  
  firstName varchar(255) NOT NULL,
  
  lastName varchar(255) NOT NULL,
 
  birthDate timestamp DEFAULT NULL,
  
  salary decimal(18,2) DEFAULT NULL,
  
  active smallint DEFAULT NULL,
  
  PRIMARY KEY (id)
);



INSERT INTO people (id, firstName, lastName, birthDate, salary, active) VALUES

(1, 'Administrator', 'Root', '1983-04-20 00:00:00', '1530.00', 1),

(2, 'John', 'Smith', '1983-05-22 00:00:00', '2000.00', 1),

(3, 'Paul', 'Walker', '1985-08-07 00:00:00', '3000.00', 1);




CREATE TABLE  products (
  
  id integer NOT NULL,
  
  name varchar(255) NOT NULL,
  
  description text,
  
PRIMARY KEY (id)
);



INSERT INTO products (id, name, description) VALUES

(1, 'Data export extension', 'Extension to export data from database'),

(2, 'Tree grid extension', 'Widget for tree grid view');