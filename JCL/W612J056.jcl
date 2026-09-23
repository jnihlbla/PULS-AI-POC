//W612J056 JOB (640W6120100W612J056,W100),'RTN W612D4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P056                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W612.W612D4.W61257(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612D4.W61257(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W612.W612D4.W61258(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612D4.W61258(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W612.W612D4.W6125A(+1)                               
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612D4.W6125A(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J056                                         
