//WF10J007 JOB (640WF100100WF10J007,W100),'RTN WF10R2',                         
//             USER=?,PASSWORD=?,                                               
//             MSGLEVEL=(1,1),                                                  
//             CLASS=V                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND D2G0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WF10P007 EXEC WF10P007                                                        
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(WF1007) PLAN(WF1007) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF10.WF10R2.WF1013A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WF10.WF10R2.WF1013A(+1)                                   
//SYSIN           DD *                                                          
WF1007-001                                                                      
WF1007                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10J007                                         
