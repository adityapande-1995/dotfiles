# Basic setup
sudo apt-get update; sudo apt-get -y upgrade

sudo apt install vim-gtk xclip git curl silversearcher-ag ripgrep neofetch htop zsh ranger fzf ctags python3-pip terminator

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# zsh-autosugestions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Starship prompt
curl -sS https://starship.rs/install.sh | sh

# Download zshrc from github
cd ~
rm .zshrc
wget https://raw.githubusercontent.com/adityapande-1995/dotfiles/master/.zshrc

# Install fonts
cd ~/Downloads
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/DroidSansMono.zip
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip
unzip DroidSansMono.zip -d ~/.fonts
unzip FiraCode.zip -d ~/.fonts
fc-cache -fv
sudo apt-get install fonts-powerline

# Install neovim
cd ~/Downloads
wget https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage
chmod +x nvim.appimage
mv nvim.appimage ..

# init.vim from github
cd ~/.config ; mkdir nvim; cd nvim
wget https://raw.githubusercontent.com/adityapande-1995/dotfiles/master/init.vim

# Installing nodejs for neovim CoC
sudo su; curl -sL install-node.now.sh | bash 
sudo apt-get install -y npm
pip3 install jedi
