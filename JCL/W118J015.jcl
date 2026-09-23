//W118J015 JOB (670W1180100W118J015,W100),'RTN W118D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=300                                                
//*                                                                             
//W118    EXEC W118P015                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W118J015                                         
