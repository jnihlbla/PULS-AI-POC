//W551J009  JOB (650W5510100W551J009,W100),'RTN W551B9',                        
//             USER=?,PASSWORD=?,                                               
//          CLASS=K                                                             
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W551  EXEC W551P009                                                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W551J009                                         
