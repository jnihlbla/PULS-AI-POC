//W213J036 JOB (670W2130100W213J036,W100),'RTN W200D1',                         
//             CLASS=1,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W213    EXEC W213P036                                                         
//*                                                                             
//EMPTY   EXEC WEMPTST,DSIN=W213.W200D1.W21336(+1)                              
//*                                                                             
//    IF (EMPTY.T.RC NE 4) THEN                                                 
//SOP     EXEC WSOP,COMMAND='ORDER W213J036'                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J036                                         
