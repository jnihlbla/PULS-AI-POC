//W553J005 JOB (640W5530100W553J005,W100),'RTN W553B3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W553    EXEC W553P005                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W553.W553B3.W55305F(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W553.W553B3.W55305F(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J005                                         
