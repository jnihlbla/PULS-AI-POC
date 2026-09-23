//W476S7RS JOB (640W4760100W476S7RS,W100),'RTN W476S7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W476S7                                               
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD *                                                                  
 %WRTNIN2 W476.W476S5.W47668 W476.W476S7.W47668                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476S7RS                                         
