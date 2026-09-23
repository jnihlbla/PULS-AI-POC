//W560J066 JOB (640W5600100W560J066,W100),'RTN W560Y1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W560    EXEC W560P066                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W560.W560Y1.W56066(+1)                               
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//     EXEC WSOP                                                                
       ACTIVATE W560JD66                                                        
//   ELSE                                                                       
//     EXEC PGM=IEFBR14                                                         
//DD1  DD DSN=W560.W560Y1.W56066(+1),DISP=(OLD,DELETE)                          
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560J066                                         
