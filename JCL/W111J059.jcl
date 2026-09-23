//W111J059 JOB (640W1110100W111J059,W100),'RTN W111B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W111    EXEC W111P059                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J059                                         
