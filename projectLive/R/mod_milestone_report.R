# ##primary functions to retrieve project information
# 
# ##to run on ntap:
# ## Rscript milestoneTools.R -p syn10147576 -c "cNF Initiative"
# 
# pvid='syn10147576'
# con='cNF Initiative'
# fvid='syn16858331'
# 
# parseArgs<-function(){
#   require(optparse)
#   option_list<-list(
#     make_option(c('-p','--projectview'),dest='pvid',help='Project view to query',default=pvid),
#     make_option(c('-c','--consortium'),dest='cons',help='Name of consortium',default=con),
#     make_option(c('-f','--fileview'),dest='fvid',help='File view id',default=fvid),
#     make_option(c('-t','--table'),dest='updatetable',default=TRUE,help='UpdateTable'))
#   args=parse_args(OptionParser(option_list = option_list))
#   return(args)
#   
# }
# 
# # We need to list all projects to generate reports for
# # @export
# getProjectsByConsortium<-function(pvid,consortium){
#   require(synapser)
#   synLogin()
#   query=paste("SELECT id,name FROM ",pvid)
#   if(!missing(consortium))
#     query=paste0(query," WHERE consortium = '",consortium,"'")
#   #print(query)
#   res=synTableQuery(query)$asDataFrame()
#   return(res)
#   
# }
# 
# #get the distinct milsteons for each project id
# # @export
# 
# sid='11374339'
# getMilestonesForProject<-function(fvid,sid){
#   require(synapser)
#   synapser::synLogin()
#   query=paste0("SELECT distinct studyId,reportMilestone from ",fvid," where studyId = '",sid,"'")
#   return(synapser::synTableQuery(query)$asDataFrame())
# }
# 
# getReportsDir<-function(synid){
#   
#   children=synGetChildren(synid, includeTypes=list( "folder"), sortBy="NAME", sortDirection="ASC")$asList()
#   
#   for(c in children)
#     if(c$name=='Reports')
#       return(c$id)
#   
#   res=synStore(Folder('Reports',parent=synid))
#   return(res$properties$id)
# }
# 
# makeMilestoneReport<-function(study,milestone,name,fvid,cons){
#   library(rmarkdown)
#   this.script='https://raw.githubusercontent.com/Sage-Bionetworks/nfResources/master/milestoneReports/R/milestoneTools.R'
#   if(is.na(milestone))
#     return(NA)
#   rmd="milestoneReports/generic_nf_milestone_report.Rmd"
#   fname=paste(getwd(),'/NFOSI_',milestone,'monthMilestoneReport_',study,'.html',sep='')
#   f<-NA
#   print(fname)
#   
#   try(f<-rmarkdown::render(rmd,rmarkdown::html_document(),
#                            output_file=fname,
#                            params=list(projectid=study,
#                                        projectname=name,
#                                        fvid=fvid,
#                                        milestone=milestone)))
#   if(is.na(f))
#     return(f)
#   parentid=getReportsDir(study)
#   
#   #res=synapser::synStore(synapser::File(path=fname,
#                          #                parent=parentid,
#                          #                annotations=list(consortium=cons,
#                          #                                 resourceType='report',
#                          #                                 fileFormat='html',
#                          #                                 studyId=study,
#                          #                                 studyName=name,
#                          #                                 reportMilestone=milestone)),
#                          # executed=this.script,used=fvid)
#   
#   #return(res$properties$id)
# }
# 
# # generate rmd files, upload to projects, and link
# # @export
# main<-function(){
# 
#   args<-parseArgs()
# 
#   #what are all the projects?
#   res=getProjectsByConsortium(args$pvid,args$cons)
# 
#   require(dplyr)
#   #what milestones do we have?
#   mstons=do.call(rbind,lapply(res$id,function(x) getMilestonesForProject(fvid=args$fvid,sid=x)))%>%
#     left_join(select(res,studyId='id',name),by='studyId')%>%
#     subset(!is.na(reportMilestone))
# 
#   #now for each project, get milestone reports and upload
#   files=apply(mstons,1,function(x) makeMilestoneReport(study=x[["studyId"]],milestone=x[["reportMilestone"]],name=x[["name"]],fvid=args$fvid,cons=args$cons))
#   tab<-data.frame(mstons,reports=files)
# 
#   # if(args$updatetable){
#   #   #tab<-synBuildTable(paste(args$cons,'Milestone Reports'),parent='syn4939478',values=tab)
#   #   #synStore(Table("syn20174143",tab))
#   # }
# }
# 
# main()