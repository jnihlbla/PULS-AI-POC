//W330J033 JOB (640W3300100W330J033,W100),'RTN W330X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W330    EXEC W330P033,                                                        
//             INDIN=W330.&VCOM.,                                               
//             INDUT=W330.&VCOM.                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W330J033                                         
