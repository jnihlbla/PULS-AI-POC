//W560J076 JOB (640W5600100W560J076,W100),'RTN W560Y4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W560    EXEC W560P076                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W560.W560Y4.W56076(+1)                               
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//     EXEC WSOP                                                                
       ACTIVATE W560JD76                                                        
//   ELSE                                                                       
//     EXEC PGM=IEFBR14                                                         
//DD1  DD DSN=W560.W560Y4.W56076(+1),DISP=(OLD,DELETE)                          
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560J076                                         
