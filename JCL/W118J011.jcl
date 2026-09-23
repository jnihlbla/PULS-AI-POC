//W118J011 JOB (670W1180100W118J011,W100),'RTN W118D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W118    EXEC W118P011                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W118J011                                         
