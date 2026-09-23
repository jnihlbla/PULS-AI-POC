//W371J05N JOB (640W3710100W371J05N,W100),'RTN W371D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W371    EXEC W371P05N                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W371.W371D3.W3715N(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
// EXEC WZ14DAP4,DSIN=W371.W371D3.W3715N(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J05N                                         
