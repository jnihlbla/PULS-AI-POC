//W478J088 JOB (650W4780100W478J088,W100),'RTN W478V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W478    EXEC W478P088                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W478.W478V2.W47888A(+1)                              
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//     EXEC WSOP                                                                
       ACTIVATE W478JD88                                                        
//   ELSE                                                                       
//     EXEC PGM=IEFBR14                                                         
//DD1  DD DSN=W478.W478V2.W47888A(+1),DISP=(OLD,DELETE)                         
//   ENDIF                                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W478J088                                         
