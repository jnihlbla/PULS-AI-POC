//W613J028 JOB (640W6130100W613J028,W100),'RTN W613S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=DESTN                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W613     EXEC W613P028,DEST1=&PRT.                                            
//*                                                                             
//W61328.W61328D1 DD *                                                          
&URVAL.                                                                         
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W613J028                                         
