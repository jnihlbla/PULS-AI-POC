//W463S6RS JOB (540W4630100W463S6RS,W100),'RTN W463S6',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  W46381,EXC                                                              
//*                                                                             
//RENAME EXEC W001PTSO                                                          
//SYSTSIN DD *                                                                  
 %WRTNIN2  W463.W463X1SE.W46381  W463.W463S6.W46381                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S6RS                                         
