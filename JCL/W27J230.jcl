//W27J230 JOB (640W27J0100W27J230,W100),'RTN W27JV1',                           
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W27J    EXEC W27JP30,TYP=DAY                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W27J230                                          
