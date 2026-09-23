//W111J051 JOB (640W1110100W111J051,W100),'RTN W111D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W111    EXEC W111P051                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W111.W111D2.W11154(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W111.W111D2.W11154(+1)                                    
//SYSIN           DD *                                                          
MIC-ERROR                                                                       
W11151                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J051                                         
