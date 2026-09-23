//WF10J041 JOB (640WF100100WF10J041,W100),'RTN WF10D4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF10    EXEC WF10P041                                                         
//WF1041.SYSTSIN DD *                                                           
DSN SYS(D2G0)                                                                   
RUN PROG(WF1041) PLAN (WF1041) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF10.WF10D4.WF1054(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WF10.WF10D4.WF1054(+1)                                    
//SYSIN           DD *                                                          
WF1041-001                                                                      
WF1041                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J041                                         
