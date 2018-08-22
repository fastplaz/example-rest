process.env.NODE_ENV = 'test';

let chai = require('chai');
let should = chai.should();
let expect = chai.expect;
let chaiHttp = require('chai-http');
let randomstring = require("randomstring");
chai.use(chaiHttp);

let customerID = 0;

let Config = require('./Config');
environment = Config['environment'];
config = Config[environment];

let BaseURL = config.baseURL;

describe('Customer: API Testing', () => {

  let customerDetails = {
    'name': randomstring.generate(10),
    'description': randomstring.generate({ length: 50, charset: 'alphabetic' })
  };

  it('it should success to create new customer: ' + customerDetails.name, (done) => {
    chai.request(BaseURL)
      .post('/')
      .set('Content-Type', 'application/x-www-form-urlencoded')
      .send(customerDetails)
      .end((err, res) => {
        customerID = res.body.response.customer_id;
        done();
      });
  });

  it('it should success to retrive customer data.', (done) => {
    chai.request(BaseURL)
      .get('/?id=' + customerID)
      .end((err, res) => {
        expect(res.body.code).to.equal(200);
        expect(res.body.response.data.name).to.equal(customerDetails.name.toUpperCase());
        done();
      });
  });

  customerDetails.name = randomstring.generate(10);
  it('it should success to update PARTIAL customer data: ' + customerDetails.name, (done) => {
    customerDetails.name = randomstring.generate(10);
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
        expect(res.body.response.data.name).to.equal(customerDetails.name.toUpperCase());
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
        res.body.response.should.have.property('count');
        res.body.response.should.have.property('data');
        res.body.response.data.should.be.an('array');
        done();
      });
    //done();
  });


});

