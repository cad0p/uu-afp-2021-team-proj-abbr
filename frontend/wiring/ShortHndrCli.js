const { exec } = require("child_process");

// FIXME: replace with installation path.
const executable = "~/.local/bin/shorthndr";

/**
 * ShortHndr CLI sidecar.
 */
module.exports.ShortHndrCli = {
  // TODO: add argument types.
  call: (command, args, onError, onSuccess) => {
    exec(
      `${executable} ${command} ${args.join(" ")}`,
      (error, stdout, stderr) => {
        return !!error ? onError(error) : onSuccess({ stdout, stderr });
      }
    );
  },
};
