//W236J052 JOB (670W2360100W236J052,W100),'RTN W236D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W236    EXEC W236P052                                                         
//*                                                                             
//EMPTY   EXEC WEMPTST,DSIN=W236.W236D1.W23652(+1)                              
//*                                                                             
//    IF (EMPTY.T.RC NE 4) THEN                                                 
//SOP     EXEC WSOP,COMMAND='ORDER W236J052'                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W236J052                                         
