process.env.NODE_ENV = 'test';

let chai = require('chai');
let should = chai.should();
let expect = chai.expect;
let chaiHttp = require('chai-http');
chai.use(chaiHttp);

let customerID = 0;
let customerName = 'my name';
let customerDescription = 'this is description.';

let Config = require('./Config');
environment = Config['environment'];
config = Config[environment];

let BaseURL = config.baseURL;

describe('Customer: API Testing', () => {

  let customerDetails = {
    'name': customerName,
    'description': customerDescription
  };

  it('it should success to create new customer', (done) => {
    chai.request(BaseURL)
      .post('/')
      .set('Content-Type', 'application/x-www-form-urlencoded')
      .send(customerDetails)
      .end((err, res) => {
        customerID = res.body.data.customer_id;
        done();
      });
  });

  it('it should success to retrive customer data.', (done) => {
    chai.request(BaseURL)
      .get('/?id=' + customerID)
      .end((err, res) => {
        expect(res.body.code).to.equal(200);
        expect(res.body.data.name).to.equal(customerName.toUpperCase());
        done();
      });
  });

  it('it should success to update PARTIAL customer data', (done) => {
    customerDetails.name = 'replace';
    customerDetails.description = '';
    chai.request(BaseURL)
      .patch('/?id=' + customerID)
      .set('Content-Type', 'application/x-www-form-urlencoded')
      .send(customerDetails)
      .end((err, res) => {
        done();
      });
  });
  it('it should success to retrive customer data after partial update.', (done) => {
    chai.request(BaseURL)
      .get('/?id=' + customerID)
      .end((err, res) => {
        expect(res.body.code).to.equal(200);
        expect(res.body.data.name).to.equal('REPLACE');
        done();
      });
  });

  it('it should success to retrive ALL customers data.', (done) => {
    chai.request(BaseURL)
      .get('/')
      .end((err, res) => {
        res.should.have.status(200);
        res.should.be.json;
        it('wow');
        expect(res.body.code).to.equal(200);
        res.body.should.have.property('count');
        res.body.should.have.property('data');
        res.body.data.should.be.an('array');
        done();
      });
    //done();
  });


});

