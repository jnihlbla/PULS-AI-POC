//W271JDAP JOB (640W2710100W271JDAP,W100),'RTN W271V4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W271.W271V4.W27107(+0)                               
//*                                                                             
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//      EXEC WZ14PDAP,DSIN=W271.W271V4.W27107(+0)                               
OLDBO                                                                           
A                                                                               
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271JDAP                                         
