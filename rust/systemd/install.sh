#!/bin/bash

set -euxo pipefail

# check dependencies
which xprintidle &>/dev/null || ( echo "xprintidle not installed" && exit 1 )
which pacmd &>/dev/null || ( echo "pacmd (pulseaudio) not installed" && exit 1 )
cwd=$(pwd)

for f in powerman@.service 99-powerman.rules; do
	[[ ! -f ./systemd/$f ]] && echo "$f not in current working directory" && exit 1
done

ln -vsfT $cwd/systemd/powerman@.service $HOME/.config/systemd/user/powerman@.service

cfg_dest=$HOME/.config/powerman.yaml
[[ -f $cfg_dest ]] || cp examples/config.yaml $cfg_dest

systemctl --user daemon-reload
cargo install --bins --path .

runsudostuff() {
	sudo ln -vsfT $cwd/systemd/99-powerman.rules /etc/udev/rules.d/99-powerman.rules
	sudo ln -vsfT $cwd/systemd/resume-lock@.service /etc/systemd/system/resume-lock@.service

	sudo systemctl enable resume-lock@$(logname).service

	echo "reloading udev rules"
	sudo udevadm control --reload-rules && sudo udevadm trigger
}

runsudostuff
