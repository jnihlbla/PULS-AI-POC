//W413JDAP JOB (640W4130100W413JDAP,W100),'RTN W413D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W413.W413D1.W41305(+0)                               
//*                                                                             
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//      EXEC WZ14PDAP,DSIN=W413.W413D1.W41305(+0)                               
W41305-001                                                                      
W41305                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W413JDAP                                         
