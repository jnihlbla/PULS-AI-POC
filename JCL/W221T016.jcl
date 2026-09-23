//W221J016 JOB (640W2210100W221J016,W100),'RTN PER-ANDERS HB2N',                
//             CLASS=K,MSGCLASS=H,NOTIFY=R040073                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P016,                                                        
//             INDIN=R040073.W221D1                                             
//*                                                                             
//*OPEND  EXEC WSOPEND,PROCESS=W221J016                                         
