//W214J002 JOB (640W2140100W214J002,W100),'RTN W214D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* OM INGA SORTWORKAR I PROCEDUREN TA BORT SORTALLOKERINGEN NEDAN              
//W214    EXEC W214P002                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W214J002                                         
