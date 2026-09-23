//W461V1RE JOB (650W4610100W461V1RE,W100),'RTN W461V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE  EXEC WFREE,NAME=W461V1,MAXRC=8                                          
//SOP     EXEC WSOPEND,PROCESS=W461V1RE                                         
