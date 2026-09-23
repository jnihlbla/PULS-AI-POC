//W371J05J JOB (640W3710100W371J05J,W100),'RTN W371D7',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST8                                                    
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W371    EXEC W371P05J                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371D7.W3715J(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W371.W371D7.W3715J(+1)                                    
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W371J05J                                         
