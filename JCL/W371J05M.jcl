//W371J05M JOB (640W3710100W371J05M,W100),'RTN W371D7',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W371    EXEC W371P05M                                                         
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371D7.W3715M(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WZ14   EXEC WZ14DAP4,DSIN=W371.W371D7.W3715M(+1)                              
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J05M                                         
