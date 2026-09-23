//W111J075 JOB (650W1110100W111J075,W100),'RTN W111V5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W111    EXEC W111P075                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W111.W111V5.W1117B(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W111.W111V5.W1117B(+1)                                    
//SYSIN           DD *                                                          
W1117B-001                                                                      
W1117B                                                                          
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W111J075                                         
