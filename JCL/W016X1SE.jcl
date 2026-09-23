//W016J002 JOB (540W0010300W016J002,W100),'RTN W016X1',                         
//             CLASS=L                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P002                                                         
//*OP     EXEC WSOPEND,PROCESS=W016J002                                         
