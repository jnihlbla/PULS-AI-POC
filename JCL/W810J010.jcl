//W810J010 JOB (640W8100100W810J010,W100),'RTN W810D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
//      INCLUDE MEMBER=SYST1                                                    
//      INCLUDE MEMBER=SYST8                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W810    EXEC W810P010                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W810.W810D2.W81010(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W810.W810D2.W81010(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W810J010                                         
