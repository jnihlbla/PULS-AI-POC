//W612J082 JOB (670W6120100W612J082,W100),'RTN W612V4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
//      INCLUDE MEMBER=DESTN                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W612    EXEC W612P082                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W612.W612V4.W61282(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612V4.W61282(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W612.W612V4.W61282A(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612V4.W61282A(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J082                                         
