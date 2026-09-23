//W115J002 JOB (670W1150100W115J002,W100),'RTN W115V1',                         
//             CLASS=1,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE    XEQ LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//W115    EXEC W115P002                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W115J002                                         
