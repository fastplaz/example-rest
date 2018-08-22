let Config = {
  environment: 'development',
  production: {
    baseURL: 'https://your-url'
  },
  beta: {
    baseURL: 'https://your-url'
  },
  development: {
    baseURL: 'http://localhost/api-test/customer'
  }
}

module.exports = Config;
