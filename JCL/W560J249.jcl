//W560J249 JOB (650W5600100W560J249,W100),'RTN W560R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W560    EXEC W560P249                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W560.W560R1.W56050(+1)                               
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//     EXEC WSOP                                                                
       ACTIVATE W560JD49                                                        
//   ELSE                                                                       
//     EXEC PGM=IEFBR14                                                         
//DD1  DD DSN=W560.W560R1.W56050(+1),DISP=(OLD,DELETE)                          
//   ENDIF                                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W560J249                                         
