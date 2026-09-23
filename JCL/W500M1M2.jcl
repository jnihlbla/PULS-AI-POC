//W500M1M2 JOB (640W5100100W500M1M2,W100),'RTN W500V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//********************************************************************          
//*                                                                  *          
//* MEMO TILL EKONOMERNA, SUMMA KALKYLPÅLÄGG                         *          
//* W512.W500M1.W51202(0)                                            *          
//********************************************************************          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W512.W500M1.W51202(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W512.W500M1.W51202(+0)                                    
//SYSIN           DD *                                                          
W51202-001                                                                      
W51202                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W500M1M2                                         
