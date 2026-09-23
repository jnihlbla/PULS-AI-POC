//WF10J052 JOB (640WF100100WF10J052,W100),'RTN WF10M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//WF10    EXEC WF10P052                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF10.WF10M2.WF1052(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=WF10.WF10M2.WF1052(+1),CPU=3                              
//SYSIN DD *                                                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J052                                         
