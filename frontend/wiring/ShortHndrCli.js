/**
 * This module configures a side-car wrapper for the ShortHndr CLI app.
 */

const { exec } = require("child_process");

// TODO(tech debt): add support for installation paths on other systems
//                  see: https://docs.haskellstack.org/en/stable/install_and_upgrade/#path
// TODO(future feature): make executable path a part of the Extension Config.
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
        return !!error
          ? onError({ error, stderr })
          : onSuccess({ stdout, stderr });
      }
    );
  },
};
