// Sending commands in response to other commands.
// This example runs "type" against every key in the database

// Edit the config Redis File to match the folder that is going to be queried
var fs = require('fs');

var sys = require('sys');
var exec = require('child_process').exec;

// Creates the Redis Client and all other global variables needed
var util = require("util");
var multi = new Array();
var types = new Array();
var time = new Array();
var numCores = 16;
var total = 0;
var max = 0;
var counts = 0;
var finished = false;
var quant = new Array();
var cpu = new Array(numCores);
var client;

function createClient()
{
  client = require("redis").createClient(6380);
}

function startServer(i) {
  console.log("Start Server");
  var a = fs.readdirSync('/opt/tools/redis/redis-2.6.14/src/snapshots');
  fs.open('/opt/tools/redis/redis-2.6.14/redis.conf2', 'a', function(err, fd) {
    fs.write(fd, a[i], 5183, 18);
  fs.close(fd);
  });
  
  var cmd = "sh /opt/tools/redis/redis-2.6.14/src/run2";
  var child = exec(cmd, function(error, stdout, stderr) { });
  createClient();
}

// For the CallHeat function. Creates the chart to display the quantization
function appendArray(pose, res, pos, size) {

  if (multi[pose] == undefined) {
    multi[pose] = new Array(size);
    for(var i=0; i<multi[pose].length; i++) {
      multi[pose][i] = 0;
    } 
  }

  if(types[pose] == undefined) {
    types[pose] = pose;
  }

  multi[pose][pos] = res;
  
  return;
}

function addQuantize(pos)
{
  if(quant[pos] == undefined) {
    quant[pos] = 0;
  }
  quant[pos]++;
  return;
}

function addCPU(cp, val, pos, size)
{

  if(cpu[cp] == undefined) {
  cpu[cp] = new Array(size);
  for(var i = 0; i < cpu[cp].length; i++)
    {
    cpu[cp][i] = 0;
    }
  }

  cpu[cp][pos] = val;
  return;

}

// Returns the over/under of a specific stat, and returns a different section
function getStat(section, stat, type, value, want) {

  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) { 

      // Goes through the key to find the specific stat 
      for(var str in res) {
        if(str.substring(0,3) == section) {
          if(str.substring(4,7) == stat.substring(0,3)) {
            //Checks to see if the stat is above or below (specified) the value
            if(type == 0) {
            if(res[str] >= value) {
              counts++;
              console.log(key);
              console.log(stat + " " + res[str]);
              for (var sstr in res) {
                if(sstr.substring(0,3) == want) {
                  console.log(sstr + "   " +  res[sstr]);
                }
              }
            } 
          } else {
            if(res[str] <= value) {
              console.log(key);
              console.log(stat + " " + res[str]);
              for (var sstr in res) {
                if(sstr.substring(0,3) == want) console.log(res[sstr]);
              }
            }
          }

          }
        }  
      }

      //Shuts down Redis Client
      if(pos == keys.length -1) shutDown();

      });
    });
  });
}

// Generates Max, Average and counts for dataset

function genStats(section, stat, instance) {
  client.keys("*", function (err, keys) {
  keys.sort(function(a,b){return a - b});   

  keys.forEach(function(key , pos) {
  client.hgetall(key,  function (err, res) {

  //Searches for the specific stat               

  for(var str in res) {
    if(str.substring(0,3) == section) {
      if(str.substring(4,7) == stat.substring(0,3)) {                        

        // Add up the data
        var number = Number(res[str]);
  
        if(instance == undefined) {
          total += number;
          if(number > max) max = number;
          counts++;
        } else {
          if (str.substring(str.length - 2) == instance) {
            total += number;
            if(number > max) max = number;
            counts++;
          }  
        }
      }
    }
  }
  // Print the totals and close the redis server

  if(pos == keys.length -1) {              
    if(instance == undefined) {
      console.log("average " + section + " " + stat + " " + (total/counts));
    }
    if(instance != undefined) {
      console.log("average " + section + " " + stat + " Instance" + instance  + ": " + (total/counts));
    }
    console.log("max   " + max); 
    console.log("count " + (counts));
    shutDown(); 
  }
  });      
  });
  });
}

function shutDown() {
  client.shutdown();
  client.quit();
}

//Print on quantized chart
function callHeat() {
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      // Print out quantization
      client.hgetall(key,  function (err, res) {
        time.push(key);
        console.log(key);
        var  pose = "";
        for (var str in res) {
          if(str.substring(0,8) == "CallHeat" && str.substring(9,13) == "lowt") {
            pose = res[str];
          }

          if(str.substring(0,8) == "CallHeat" && str.substring(9,14) == "value") {
            appendArray(Number(pose), res[str], pos, keys.length);
            pose = ""; 
          }
        }

        if (pos == (keys.length - 1)) { 
          console.log("times       " + time);
          for(var pose in types) {
            console.log(pose + " - " + (2* pose) + "             " +  multi[pose]);
          }
          shutDown();
        }
      }); 
    });
  });
}

//Create a quantization for a given data set
function quantize(section, stat, instance) {

  client.keys("*", function (err, keys) {
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) {
        for(var str in res) {
          if(str.substring(0,3) == section) {
            if(str.substring(4,7) == stat.substring(0,3)) {
              if(instance == undefined) {
              var i = 1; 
              var number = Number(res[str]);
              if(number == 0) addQuantize(0);
              else
                {
                while(number >= i) {
                  i *= 2;
                  }
                addQuantize(i/2);
                }
              } 
              else {
                if (str.substring(str.length - 2) == instance) {
                  var i = 1;
                  var number = Number(res[str]);
                  if(number == 0) addQuantize(0);
                  else {
                    while(number >= i) {
                      i *= 2;
                    }
                  addQuantize(i/2);
                  }
                }
              }
            }
          }
        }
        if (pos == (keys.length - 1)) {
          for(var pose in quant) {
            console.log(pose + " - " + (2* pose) + "             " +  quant[pose]);
          }
          shutDown();
        }
      });
    });
  });
}

function cpuStat()
{
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) {
        time.push(key);
        var cp = 0;
        var val = 0;
        var ticks = 0;
        for(var str in res) {
          if(str.substring(0,3) == "Sys" && str.substring(4,9) == "ticks") {
            var ticks = res[str];
          }
          if(str.substring(0,3) == "CPU" && str.substring(4,8) == "core") {   
            cp = res[str];
          }
          if(str.substring(0,3) == "CPU" && str.substring(4,9) == "usage") {
            val = res[str];
            val = Math.floor(val* 10000 / ticks) / 100;
            addCPU(cp, val, pos, keys.length);
          }
        }
        if (pos == (keys.length - 1)) {
          console.log("times       " + time);
          for(var cp in cpu) {
            console.log("cpu " +  cp  + "             " +  cpu[cp]);
          }
          shutDown();
        }
      });
    });
  });
}

function process(cp)
{
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) {
        var b = false;
        var pose = "";
        var ticks = 0;
        for(var str in res) {
          if(str.substring(0,3) == "Sys" && str.substring(4,9) == "ticks") {
          ticks = res[str];
          }
          if(str.substring(0,3) == "Pro" && str.substring(4,7) == "CPU") {
            for (var c in cp) {
            if(cp[c] == str.substring(str.length - 2)) {
                b = true;
                pose += "CPU " + cp[c];
              }
            }
          }
          if(b && str.substring(0,3) == "Pro" && str.substring(4,7) == "PID") {
            pose += "    PID " + res[str];
          }
          if(b && str.substring(0,3) == "Pro" && str.substring(4,7) == "exe") {
            pose += "    execname " + res[str];
          }
          if(b && str.substring(0,3) == "Pro" && str.substring(4,7) == "Usa") {
            pose += "     usage " + Math.floor(res[str]* 10000 / ticks) / 100  + "    time " + key;
            console.log(pose);
            b = false;
            pose = "";
          }
        }
        if(pos == keys.length -1) shutDown();
      });      
    });
  });
}

// Straight up prints the data for a given time
function printData(times) {
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    times.forEach(function(time , pos) {
      client.hgetall(keys[time],  function (err, res) {
      console.log(keys[time]);
      console.log(res);
      if(pos == times.length -1) shutDown();
      });
    });
  });
}

var arr = [7, 12];

startServer(3);
process(arr);
//startServer(2);
//process(arr);
//cpuStat();
//genStats("Dis", "nwritten", 1);
