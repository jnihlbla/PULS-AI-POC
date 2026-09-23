//W222J036 JOB (670W2220100W222J036,W100),'RTN W222V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W222    EXEC W222P036                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222J036                                         
