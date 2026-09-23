//W810J013 JOB (640W8100100W810J013,W100),'RTN W810D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
//      INCLUDE MEMBER=SYST8                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W810    EXEC W810P013                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W810.W810D2.W81013(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W810.W810D2.W81013(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W810J013                                         
