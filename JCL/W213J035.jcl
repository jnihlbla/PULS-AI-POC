//W213J035 JOB (640W2130100W213J035,W100),'RTN W200D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W213    EXEC W213P035                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J035                                         
