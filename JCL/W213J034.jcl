//W213J034 JOB (670W2130100W213J034,W100),'RTN W200D1',                         
//             CLASS=1,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W213    EXEC W213P034                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J034                                         
