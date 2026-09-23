//W463S5RS JOB (540W4630100W463S5RS,W100),'RTN W463S5',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W463S5                                               
//*                                                                             
//RENAME EXEC W001PTSO                                                          
//SYSTSIN DD *                                                                  
 %WRTNIN2  W463.W463X2SE.W46334  W463.W463S5.W46334                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S5RS                                         
