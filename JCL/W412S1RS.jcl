//W412S1RS JOB (640W4120100W412S1RS,W100),'RTN W412S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* ORDTYP = &ORDTYP                                                            
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//TSO.SYSTSIN DD *                                                              
 %WRTNIN2 W412.IN.W41210 W412.&ORDTYP..W41210                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412S1RS                                         
