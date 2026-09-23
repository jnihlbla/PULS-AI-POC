//W463S3RE JOB (650W4630100W463S3RE,W100),'RTN W463S3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W463S3,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S3RE                                         
