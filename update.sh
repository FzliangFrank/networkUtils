echo "Enter Your Message\n"
read message

R -e "roxygen2::roxygenize()"
R -e "pkgdown::build_site_github_pages()"
git add .
git commit -m"${message}"
git status
echo "Pushing data to development branch!!!"
git push -u origin dev

