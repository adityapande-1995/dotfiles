# Basic setup
sudo apt-get update; sudo apt-get -y upgrade

sudo apt install vim-gtk git curl silversearcher-ag ripgrep neofetch htop zsh ranger fzf ctags

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# zsh-autosugestions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Download zshrc from github
cd ~
rm .zshrc
wget https://raw.githubusercontent.com/adityapande-1995/dotfiles/master/.zshrc

# Install fonts
cd ~/Downloads
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/DroidSansMono.zip
unzip DroidSansMono.zip -d ~/.fonts
fc-cache -fv

# Install neovim
cd ~/Downloads
wget https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage
chmod +x nvim.appimage
mv nvim.appimage ..

# Vim plug
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# init.vim from github
cd ~/.config ; mkdir nvim; cd nvim
wget https://raw.githubusercontent.com/adityapande-1995/dotfiles/master/init.vim

# Installing nodejs for neovim CoC
curl -sL https://deb.nodesource.com/setup_17.x | sudo -E bash -
sudo apt-get install -y nodejs npm
