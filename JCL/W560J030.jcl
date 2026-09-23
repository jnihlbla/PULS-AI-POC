//W560J030 JOB (650W5600100W560J030,W100),'RTN W560D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W560    EXEC W560P030                                                         
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W560.W560D1.W56031(+1)                              
//*                                                                             
//    IF (EMPTYT1.T.RC = 4) THEN                                                
//      EXEC WSOP                                                               
//      CANCEL W560J032                                                         
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W560J030                                         
