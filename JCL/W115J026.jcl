//W115J026 JOB (650W1150100W115J026,W100),'RTN W115D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W115    EXEC W115P026                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W115J026                                         
/*                                                                              
