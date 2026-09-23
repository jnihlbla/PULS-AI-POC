//W611S1RS JOB (640W6110100W611S1RS,W100),'RTN W611S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD *                                                                  
 %WRTNIN2 W611.W611X1SE.W61101  W611.W611S1.W61101                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611S1RS                                         
