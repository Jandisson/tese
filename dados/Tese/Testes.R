
projectsDb  = dbConnect (RMariaDB::MariaDB(), dbname="ghtorrent", password ="Voip_Nexus1",user="root",host="18.228.191.170") 
projetos_pesquisa = dbReadTable(projectsDb,name="PROJETOS_PESQUISA")
cor(projetos_pesquisa$linhas,projetos_pesquisa$final_pagerank_sum_users)


