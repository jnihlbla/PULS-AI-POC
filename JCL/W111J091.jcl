//W111J091 JOB (640W1110100W111J091,W100),'RTN W100V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W111.W100V1.W11190(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W111.W100V1.W11190(+0)                                    
//SYSIN           DD *                                                          
W11190-001                                                                      
W11190                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J091                                         
