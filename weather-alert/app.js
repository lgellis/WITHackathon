// app.js
//* ************************************************************************
// Copyright 2016 IBM Corp.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//* ************************************************************************
'use strict'

const express = require('express')
const request = require('request')
const fs = require('fs')

// Security - helmet
const helmet = require('helmet')

// setup middleware
const app = express()
const ninetyDaysInMilliseconds = 7776000000

app.use(express.static(__dirname + '/public'))
// set the HTTP Strict Transport Security (HSTS) header for 90 days
app.use(helmet.hsts({
  maxAge: ninetyDaysInMilliseconds,
  includeSubdomains: true,
  force: true
}))
// Prevent Cross-site scripting (XSS) attacks
app.use(helmet.xssFilter())

const rawdata = fs.readFileSync('vcap_creds.json')
const vcapCreds = JSON.parse(rawdata)
const weatherHost = vcapCreds['weatherinsights'][0].credentials.url

function weatherAPI (path, qs, doneCallback) {
  var url = weatherHost + path
  console.log(url, qs)
  request({
    url: url,
    method: 'GET',
    headers: {
      'Content-Type': 'application/json;charset=utf-8',
      'Accept': 'application/json'
    },
    qs: qs
  }, function (err, req, data) {
    if (err) {
      doneCallback(err)
    } else {
      if (req.statusCode >= 200 && req.statusCode < 400) {
        try {
          doneCallback(null, JSON.parse(data))
        } catch (e) {
          console.log(e)
          doneCallback(e)
        }
      } else {
        console.log(err)
        doneCallback({ message: req.statusCode, data: data })
      }
    }
  })
}

app.get('/api/forecast/daily', (req, res) => {
  //austin '30.267153,-97.743057'
  var geocode = (req.query.geocode || '30.271,-91.704').split(',')
  weatherAPI('/api/weather/v1/geocode/' + geocode[0] + '/' + geocode[1] + '/forecast/daily/3day.json', {
    units: req.query.units || 'm',
    language: req.query.language || 'en'
  }, function (err, result) {
    if (err) {
      console.log(err)
      res.send(err).status(400)
    } else {
      console.log('3 days Forecast')
      res.json(result)
    }
  })
})

app.get('/api/alerts', (req, res) => {
  let queryURI = null
  console.log('test')
  console.log(req.query)
  if ('geocode' in req.query) {
    let geocode = (req.query.geocode || '30.267153,-97.743057').split(',')
    queryURI = '/api/weather/v1/geocode/' + geocode[0] + '/' + geocode[1] + '/alerts.json?language=en-US'
  } else if ('countryCode' in req.query && 'state' in req.query) {
    let countryCode = (req.query.countryCode || 'US')
    let state = (req.query.state || 'TX')
    queryURI = '/api/weather/v1/country/' + countryCode + '/state/' + state + '/alerts.json?language=en-US'
  } else if ('countryCode' in req.query) {
    let countryCode = (req.query.countryCode || 'US')
    queryURI = '/api/weather/v1/country/' + countryCode + '/alerts.json?language=en-US'
  }
  weatherAPI(queryURI, {
    language: req.query.language || 'en'
  }, function (err, result) {
    if (err) {
      console.log(err)
      res.send(err).status(400)
    } else {
      console.log('Current alerts')
      res.json(result)
    }
  })
})

app.get('/api/alert/details', (req, res) => {
  var key = (req.query.key)
  weatherAPI('/api/weather/v1/alert/' + key + '/details.json?language=en-US', {
    language: req.query.language || 'en'
  }, function (err, result) {
    if (err) {
      console.log(err)
      res.send(err).status(400)
    } else {
      console.log('Current alert details')
      res.json(result)
    }
  })
})

app.get('/api/geolocation', (req, res) => {
  var city = (req.query.city || 'Austin')
  var type = (req.query.type || 'city')
  var state = (req.query.state || 'TX')
  var country = (req.query.country || 'US')
  weatherAPI('/api/weather/v3/location/search?query=' + encodeURI(city) + '&locationType=' + type +
    '&adminDistrictCode=' + state + '&countryCode=' + country, {
    language: req.query.language || 'en'
  }, function (err, result) {
    if (err) {
      console.log(err)
      res.send(err).status(400)
    } else {
      console.log('City location')
      res.json(result)
    }
  })
})

app.listen(3000, () => console.log('Weather alert is listening on port 3000!'))
