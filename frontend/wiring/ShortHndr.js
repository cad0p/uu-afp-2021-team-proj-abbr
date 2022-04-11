const { exec } = require("child_process");

// FIXME: replace with installation path.
const executable = "/Users/dmitriiorlov/.local/bin/shorthndr";

module.exports.ShortHndr = {
  call: (command, args, onError, onSuccess) => {
    exec(
      `${executable} ${command} ${args.join(" ")}`,
      (error, stdout, stderr) => {
        return !!error ? onError(error) : onSuccess({ stdout, stderr });
      }
    );
  },
};
