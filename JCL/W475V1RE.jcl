//W475V1RE JOB (650W4750100W475V1RE,W100),'RTN W475V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W475V1,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W475V1RE                                         
