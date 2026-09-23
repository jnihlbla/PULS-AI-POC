//W217J009 JOB (640W2170100W217J009,W100),'RTN W217S4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W217    EXEC W217P009                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W217J009                                         
