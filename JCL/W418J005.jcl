//W418J005 JOB (640W4180100W418J005,W100),'RTN W418M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W418    EXEC W418P005                                                         
//TSO.SYSTSIN DD *                                                              
DSN SYS(D2G0)                                                                   
RUN PROG(W41805) PLAN (W41805) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W418.W418M2.W41805(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WZ14   EXEC WZ14DAP2,DSIN=W418.W418M2.W41805(+1),CPU=10                       
//SYSIN            DD *                                                         
W41805-001                                                                      
W41805                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J005                                         
