//W215J003 JOB (670W2150100W215J003,W100),'RTN W215S1',                         
//             CLASS=1,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W215    EXEC W215P003                                                         
//*                                                                             
//EMPTY   EXEC WEMPTST,DSIN=W215.W215S1.W21510(+1)                              
//*                                                                             
//    IF (EMPTY.T.RC NE 4) THEN                                                 
//SOP     EXEC WSOP,COMMAND='ORDER W215J003'                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215J003                                         
