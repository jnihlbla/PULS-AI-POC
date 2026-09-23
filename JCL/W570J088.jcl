//W570J088 JOB (640W5700100W570J088,W100),'RTN W570M3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W570    EXEC W570P088                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570M3.W57088(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W570.W570M3.W57088(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W570.W570M3.W57088A(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W570.W570M3.W57088A(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J088                                         
