//W612J002 JOB (670W6120100W612J002,W100),'RTN W612D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P002                                                         
//W61202.W61202D2 DD DATACLAS=PSEN                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J002                                         
