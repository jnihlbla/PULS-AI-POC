//WF10J022 JOB (640WF100100WF10J022,W100),'RTN WF10R3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF10    EXEC WF10P022                                                         
//WF1022.SYSTSIN DD *                                                           
DSN SYS(D2G0)                                                                   
RUN PROG(WF1022) PLAN (WF1022) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF10.WF10R3.WF1022A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WF10.WF10R3.WF1022A(+1)                                   
//SYSIN           DD *                                                          
WF1022-001                                                                      
WF1022                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J022                                         
