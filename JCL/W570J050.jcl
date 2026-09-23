//W570J050 JOB (640W5700100W570J050,W100),'RTN W570M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W570    EXEC W570P050                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570M2.W57050(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W570.W570M2.W57050(+1)                                    
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W570J050                                         
