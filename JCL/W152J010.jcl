//W152J010 JOB (640W1520100W152J010,W100),'RTN W152B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* BEHANDLAR ÖVERSATT TEXTFIL FRÅN CBG-konsult                                 
//*                                                                             
//W152     EXEC W152P010                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W152J010                                         
