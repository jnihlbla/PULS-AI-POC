//W215D1RS JOB (640W2150100W215D1RS,W100),'RTN W215D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//RENAME   EXEC W001PTSO                                                        
//TSO.SYSTSIN DD *                                                              
  %WRTNIN2 W215.W215X1SE.W21501 W215.W215D1.W21501                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215D1RS                                         
