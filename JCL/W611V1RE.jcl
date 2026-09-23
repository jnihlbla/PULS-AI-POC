//W611V1RE JOB (640W6110100W611V1RE,W100),'RTN W611V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W611V1,MAXRC=8                                        
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD   *                                                                
  %RENDATE  'W611.W611V1.W61179(+0)'  'W020.V%AAVV..MRHIST'                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611V1RE                                         
