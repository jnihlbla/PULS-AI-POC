//W152B1RS JOB (640W1520100W152B1RS,W100),'RTN W152B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//RENAME   EXEC W001PTSO,                                                       
//          MEMBER=TEMPNAME,MAXRC=4                                             
//* RENAME THE OLDER GENERATION                                                 
//*                                                                             
//TSO.SYSTSIN DD *                                                              
 %WRTNIN2  W152.W152X1.W15205   W152.W152B1.W15205                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W152B1RS                                         
