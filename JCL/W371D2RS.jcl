//W371D2RS JOB (650W3710100W371D2RS,W100),'RTN W371D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W371D2                                                
//RENAME EXEC W001PTSO                                                          
//SYSTSIN DD *                                                                  
 %WRTNINP 'W.QASE.CONSTANT(W371D2DS)'                                           
//SOP     EXEC WSOPEND,PROCESS=W371D2RS                                         
