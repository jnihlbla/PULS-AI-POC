//W224J080 JOB (670W2240100W224J080,W100),'RTN W224V5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W224    EXEC W271P080,                                                        
//             INDIN=W224.W224V5                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J080                                         
