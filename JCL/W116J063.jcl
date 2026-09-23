//W116J063 JOB (640W1160100W116J063,W100),'RTN W116S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* EXPEDITER=&VCOM                                                             
//* COUNTRYX2=&COUNTRYX2                                                        
//*                                                                             
//W116    EXEC W116P063                                                         
//*                                                                             
//W11663.SYSINPUT DD *                                                          
&COUNTRYX2                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J063                                         
