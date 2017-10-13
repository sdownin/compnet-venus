#!/bin/bash
yum update
#install R
yum install -y R
#install RStudio-Server
wget https://download2.rstudio.org/rstudio-server-rhel-0.99.465-x86_64.rpm
yum install -y --nogpgcheck rstudio-server-rhel-0.99.465-x86_64.rpm
#install shiny and shiny-server
R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
wget https://download3.rstudio.org/centos5.9/x86_64/shiny-server-1.4.0.718-rh5-x86_64.rpm
yum install -y --nogpgcheck shiny-server-1.4.0.718-rh5-x86_64.rpm
#---------install libxml2 for igraph---------------
wget http://xmlsoft.org/sources/libxml2-sources-2.9.3.tar.gz
tar -xvzf libxml2-sources-2.9.3.tar.gz
cd libxml2-2.9.3
./configure --prefix=/usr/local/libxml2
make
# install  the made libxml2
#With sudo,
sudo make install
#Without sudo,
make install
#--------end install libxml2--------------------------
#install network analysis and parallelization
R -e "install.packages(c('ggplot2', 'ergm', 'xergm.common', 'snow', 'igraph', 'btergm', 'sna', 'plyr', 'reshape2', 'texreg', 'data.table','lattice','latticeExtra', 'statnet', 'statnet.common', 'network', 'Matrix', 'boot', 'coda', 'ROCR', 'speedglm', 'RSiena','RMySQL','RODBC','devtools', 'Rmpi'), repos='http://cran.us.r-project.org')"
#add user(s)
useradd username
echo username:password | chpasswd