//W233J012 JOB (650W2330100W233J012,W100),'RTN W233PV',                         
//             USER=?,PASSWORD=?,                                               
//            CLASS=K                                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W233    EXEC W233P012                                                         
//SOP     EXEC WSOPEND,PROCESS=W233J012                                         
