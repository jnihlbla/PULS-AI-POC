//WF21J015 JOB (670WF210100WF21J015,W100),'RTN WF21S1',                         
//             CLASS=N,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG1                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF20.WF21S1.WF2011                                   
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//JAVA1 EXEC WF21P015,                                                          
//     DSIN=WF20.WF21S1.WF2011                                                  
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=WF20.WF21S1.WF2031A                                  
//    IF (EMPTY2.T.RC = 0) THEN                                                 
//JAVA2 EXEC WF21P015,                                                          
//     DSIN=WF20.WF21S1.WF2031A                                                 
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF21J015                                         
