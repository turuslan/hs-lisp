#!/usr/bin/env node

const Fs = require('fs');

// https://gist.github.com/espadrine/172658142820a356e1e0
const SyncStdin = {
  getStdin(endByte) {
    var BUFSIZE = 256;
    var buf = Buffer.alloc(BUFSIZE);
    var totalBuf = Buffer.alloc(BUFSIZE);
    var totalBytesRead = 0;
    var bytesRead = 0;
    var endByteRead = false;

    var fd = process.stdin.fd;
    // Linux and Mac cannot use process.stdin.fd (which isn't set up as sync).
    var usingDevice = false;
    try {
      fd = Fs.openSync('/dev/stdin', 'rs');
      usingDevice = true;
    } catch (e) {}

    for (;;) {
      try {
        bytesRead = Fs.readSync(fd, buf, 0, BUFSIZE, null);

        // Copy the new bytes to totalBuf.
        var tmpBuf = Buffer.alloc(totalBytesRead + bytesRead);
        totalBuf.copy(tmpBuf, 0, 0, totalBytesRead);
        buf.copy(tmpBuf, totalBytesRead, 0, bytesRead);
        totalBuf = tmpBuf;
        totalBytesRead += bytesRead;

        // Has the endByte been read?
        for (var i = 0; i < bytesRead; i++) {
          if (buf[i] === endByte) {
            endByteRead = true;
            break;
          }
        }
        if (endByteRead) { break; }
      } catch (e) {
        if (e.code === 'EOF') { break; }
        throw e;
      }
      if (bytesRead === 0) { break; }
    }
    if (usingDevice) { Fs.closeSync(fd); }
    return totalBuf;
  },
  buffered: '',
  getline() {
    if (this.buffered.length === 0) {
      this.buffered = this.getStdin('\n'.charCodeAt(0)).toString('utf-8');
    }
    const newline = this.buffered.search('\n') + 1;
    const line = this.buffered.slice(0, newline);
    this.buffered = this.buffered.slice(newline);
    return line;
  },
};

function make_global() {
  function g([name]) {
    if (name in g._global) {
      return g._global[name];
    } else {
      throw new Error(`name "${name}" not found`);
    }
  }
  g.set = (name, value) => {
    g._global[name] = value;
  };
  g._global = {
    ['read-int']() {
      const value = +SyncStdin.getline();
      if (isNaN(value)) {
        throw new Error('not integer');
      } else {
        return value;
      }
    },
    print(x) {
      process.stdout.write(x + '\n');
      return x;
    },
    princ(x) {
      process.stdout.write(x + '');
      return x;
    },
    floor(x) {
      return Math.floor(x);
    },
  };
  return g;
}

program(make_global());
